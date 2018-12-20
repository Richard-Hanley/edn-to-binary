(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol Codec
  (alignment* [this])
  (encode* [this data])
  (decode* [this bin])
  (recode* [this encoding]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing the codec registry and the spec metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-codec-spec [codec]
  `(if (keyword? ~codec)
    ~codec
    (or (::spec (meta ~codec))
        (throw (ex-info "No spec defined for given codec" {:codec ~codec})))))

(defonce ^:private registry-ref (atom {}))

(defn registry
  "returns the registry map, prefer 'get-codec' to lookup a codec by name"
  []
  @registry-ref)

(defn get-codec
  "Returns the codec registered with the fully qualified keyword"
  [k] (get @registry-ref k))

(defn- named? [x] (instance? clojure.lang.Named x))

(defn- with-name [codec name]
  (with-meta codec (assoc (meta codec) ::name name)))

(defn reg-resolve
  "returns the codec end of alias chain starting with k, nil if not found, k if k not Named"
  [k]
  (if (named? k)
    (let [reg @registry-ref]
      (loop [codec k]
        (if (named? codec)
          (recur (get reg codec))
          (when codec
            (with-name codec k)))))
    k))

(defn register-codec [k c]
  (swap! registry-ref assoc k c))

(defmacro def 
  "Given a namespace qualified keyword k, this will register the codec and assocaited
  spec to k.  The spec is assumed to be part of the metadata of the passed codec

  Additional agruments are supported.  Using a :spec or :post-spec argument will add in
  extra specs that might not be part of the passed codec.  The resulting spec will be 
  of the form `(s/and spec codec post-spec) "
  [k codec & {:keys [spec post-spec]}]
    (let [base-spec `(get-codec-spec ~codec)
          final-spec (cond
                       (and (some? spec) (some? post-spec)) `(s/and ~spec ~base-spec ~post-spec)
                       (some? spec) `(s/and ~spec ~base-spec)
                       (some? post-spec) `(s/and ~base-spec ~post-spec)
                       :else `~base-spec)]
  `(do 
     (register-codec ~k ~codec)
     (s/def ~k ~final-spec))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for Codec protocol that look in the registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn alignment [codec-form] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (alignment* codec)))

(defn encode [codec-form data] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (encode* codec data)))

(defn decode [codec-form bin] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (decode* codec bin)))

(defn recode [codec-form & encoding-args] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (recode* codec (apply array-map encoding-args))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Specs for encoding codecs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn power-of-2? 
  "A number is a power of two if there is only one bit set"
  [num]
  (= (Long/lowestOneBit num) (Long/highestOneBit num)))

(s/def ::alignment power-of-2?)

(s/def ::force-alignment (s/nilable ::alignment))
(s/def ::word-size #{1 2 4 8})
(s/def ::order #{:little :big :network :native})
(s/def ::flatten boolean?)

(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})

(s/def ::base-encoding (s/keys :req [::word-size ::order ::flatten]))

(def default-encoding {::word-size 1 
                       ::order :little
                       ::flatten true})



(defrecord PrimitiveCodec [size get-buffer put-buffer enc]
  Codec
  (alignment* [this]
    (let [{:keys [::force-alignment ::word-size]} enc]
      (or force-alignment
          (min size word-size))))
  (encode* [this data]
    (let [buff (.order (ByteBuffer/allocate size)
                       (get order-map (::order enc)))
          _ (put-buffer buff (or data 0))]
      (seq (.array buff))))
  (decode* [this bin] (throw (UnsupportedOperationException. "Not implemented yet!")))
  (recode* [this encoding] (update this :enc merge encoding)))

(defmacro reify-primitive 
  ([class get put coerce-to] `(reify-primitive ~class ~get ~put ~coerce-to identity))
  ([class get put coerce-to coerce-from]
   (let [size `(. ~class BYTES)
         get-fn `#(~coerce-from (~get %))
         put-fn `#(~put %1 (~coerce-to %2))]
     `(PrimitiveCodec. ~size ~get-fn ~put-fn default-encoding))))

(defmacro signed-primitive-spec [class]
  `#(<= (. ~class MIN_VALUE) %1 (. ~class MAX_VALUE)))

(defmacro unsigned-primitive-spec [class]
  `(s/int-in 0 (bit-shift-left 1 (. ~class SIZE))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining the primitive specs

(def primitive-prototypes 
  {:int8 (with-meta (reify-primitive Byte .get .put byte)
                     {::spec (signed-primitive-spec Byte)})

   :int16 (with-meta (reify-primitive Short .getShort .putShort short)
                      {::spec (signed-primitive-spec Short)})

   :int32 (with-meta (reify-primitive Integer .getInt .putInt int)
                      {::spec (signed-primitive-spec Integer)})

   :int64 (with-meta (reify-primitive Long .getLong .putLong long)
                      {::spec (signed-primitive-spec Long)})

   :uint8 (with-meta (reify-primitive Byte .get .put unchecked-byte Byte/toUnsignedLong)
                      {::spec (unsigned-primitive-spec Byte)})

   :uint16 (with-meta (reify-primitive Short .getShort .putShort unchecked-short Short/toUnsignedLong)
                       {::spec (unsigned-primitive-spec Short)})

   :uint32 (with-meta (reify-primitive Integer .getInt .putInt unchecked-int Integer/toUnsignedLong)
                       {::spec (unsigned-primitive-spec Integer)})

   :float32 (with-meta (reify-primitive Float .getFloat .putFloat float)
                        {::spec (signed-primitive-spec Float)})

   :float64 (with-meta (reify-primitive Double .getDouble .putDouble double)
                        {::spec (signed-primitive-spec Double)})
   })


; (edn-to-binary.core/def ::int8 (with-meta (reify-primitive Byte .get .put byte)
;                                           {::spec (signed-primitive-spec Byte)}))

; (edn-to-binary.core/def ::int16 (with-meta (reify-primitive Short .getShort .putShort short)
;                                            {::spec (signed-primitive-spec Short)}))

; (edn-to-binary.core/def ::int32 (with-meta (reify-primitive Integer .getInt .putInt int)
;                                            {::spec (signed-primitive-spec Integer)}))

; (edn-to-binary.core/def ::int64 (with-meta (reify-primitive Long .getLong .putLong long)
;                                            {::spec (signed-primitive-spec Long)}))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (edn-to-binary.core/def ::uint8 (with-meta (reify-primitive Byte .get .put unchecked-byte Byte/toUnsignedLong)
;                                            {::spec (unsigned-primitive-spec Byte)}))

; (edn-to-binary.core/def ::uint16 (with-meta (reify-primitive Short .getShort .putShort unchecked-short Short/toUnsignedLong)
;                                             {::spec (unsigned-primitive-spec Short)}))

; (edn-to-binary.core/def ::uint32 (with-meta (reify-primitive Integer .getInt .putInt unchecked-int Integer/toUnsignedLong)
;                                             {::spec (unsigned-primitive-spec Integer)}))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (edn-to-binary.core/def ::float32 (with-meta (reify-primitive Float .getFloat .putFloat float)
;                                              {::spec (signed-primitive-spec Float)}))

; (edn-to-binary.core/def ::float64 (with-meta (reify-primitive Double .getDouble .putDouble double)
;                                              {::spec (signed-primitive-spec Double)}))

; (defmacro reg-primitive [name prim-key & encoding-args]
;   (let [new-key (fn [n] (keyword (str n) (clojure.core/name prim-key)))]
;     `(edn-to-binary.core/def (~new-key ~name) (apply recode (~prim-key primitive-prototypes) ~encoding-args))))

(defmacro register-primitives 
  "Registers the primitive codecs with a given namespace and encoding. This allows developers
  to create a group of primitives with their desired word-size and byte order.

  A name must be a string. It cannot be an experssion or symbol such as (str *ns*)
  This is an unfortunate effects of the way clojure.spec.alpha/def works. 

  The primitives that are defined are as follows
  :int8
  :int16
  :int32
  :int64
  :uint8
  :uint16
  :uint32
  :float32
  :float64

  Calling this macro with a given name and encoding-args will result is a series of calls looking like
  'edn-to-binary.core/def :name/int8 (apply recode :edn-to-binary.core/int8  encoding-args)'
  'edn-to-binary.core/def :name/int16 (apply recode :edn-to-binary.core/int16  encoding-args)'
  ...etc.
  "
  [name & encoding-args]
  (let [ks (mapv #(keyword name (clojure.core/name %)) (keys primitive-prototypes))
        def-forms (mapv (fn [k-form k-proto]
                          ;; This form is a little hand wavy
                          ;; The actual record can't be gotten in the macro because print-dub is not defined
                          ;; so instead we will use the line '(~k-proto primitive-prototypes) to get the codec
                          ;;
                          ;; In a sense the isn't getting the actual codec, but it is the executable code that
                          ;; can get the codec
                          `(edn-to-binary.core/def ~k-form (apply recode (~k-proto primitive-prototypes) [~@encoding-args])))
                        ks
                        (keys primitive-prototypes))

        ]
    `(do ~@def-forms)))

(defmacro register-primitives-with-ns
  "Registers primitives in a passed namespace.  This lets you use register primitives in a 
  namespace without having to hardcode the string name"
  ([namespace] 
   (let [n (str namespace)]
     `(register-primitives ~n)))
  ([namespace & encoding-args] 
   (let [n (str namespace)]
     `(register-primitives ~n ~encoding-args))))

(defmacro register-primitives-in-ns
  "The *ns* token is represented differently from other objects, and it was
  throwing the register-primitives-with-ns off.  This function gets around it by uxing *ns*
  explicitly

  This macro will register the primitives in the namespace where it was called"
  ([] 
   (let [n (str *ns*)]
     `(register-primitives ~n )))
  ([& encoding-args] 
   (let [n (str *ns*)]
     `(register-primitives ~n ~encoding-args))))

(register-primitives-in-ns)
