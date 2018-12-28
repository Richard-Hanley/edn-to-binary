(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol Codec
  (alignment* [this])
  (encode* [this data])
  (decode* [this bin decoding-args])
  (recode* [this encoding]))

(defprotocol BinaryCollection
  (flatten [this]))

(defn make-binary 
  "Makes an annotated binary collection

  A binary collection is a recursive collection of bytes.  These
  collections may be sequential or associative

  Binary collections can have metadata that sets the alignment, or in the
  case of maps a key order"
  [bin & {:keys [order align]}]
  (with-meta bin
             (assoc (meta bin) ::alignment align ::key-order order)))

(defn binary-coll-alignment 
  "Gets the alignment of a binary collection.  If none is specified, it will
  return 1"
  [bin]
  (or (::alignment (meta bin)) 1))

(defn binary-order 
  "This will get the key order for associative binary collections
  If no key order is specified, then key order will be gotten from a 
  call to keys"
  [coll]
  (or (::key-order (meta coll)) (keys coll)))

(defn alignment-padding [align-to position]
  (let [bytes-over (mod position align-to)]
    (if (pos? bytes-over)
      (- align-to bytes-over)
      0)))

(defn indexed-binary [index binary-coll]
  (with-meta binary-coll
             (assoc (meta binary-coll) ::index index)))

(defn current-index [binary-coll]
  (or (::index (meta binary-coll)) 0))

(defn trim-to-alignment [align-to binary-coll]
  (let [bytes-off (alignment-padding align-to 
                                     (current-index binary-coll))]
    (with-meta (drop bytes-off binary-coll)
               (update (meta binary-coll) ::index (fnil #(+ bytes-off %) 0)))))

(extend-protocol BinaryCollection
  nil
  (flatten [this] nil)

  clojure.lang.Keyword
  (flatten [this] nil)

  Byte
  (flatten [this] (list this))

  clojure.lang.Sequential
  (flatten [this]
    (let [coll-alignment (binary-coll-alignment this)
          [bin elem-alignment] (reduce (fn [[accum max-alignment] elem]
                                         (let [elem-alignment (binary-coll-alignment elem)
                                               padding (repeat (alignment-padding elem-alignment
                                                                                  (count accum))
                                                               (byte 0))]
                                           [(concat accum padding elem)
                                            (max elem-alignment max-alignment)]))
                                       [[] 1]
                                       (map flatten this))]
          (make-binary bin :align (max coll-alignment elem-alignment))))
  clojure.lang.IPersistentMap
  (flatten [this]
    (let [coll-alignment (binary-coll-alignment this)
          ordered-vals (map #(get this %) (binary-order this))
          bin (flatten ordered-vals)]
      (make-binary bin :align (max coll-alignment (binary-coll-alignment bin))))))

(defn sizeof [bin]
  (count (flatten bin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing the codec registry and the spec metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extract-codec 
  "Given a spec object, this will extract the annotated codec.  The spec must have 
  a codec embedded in the metadata.  This function will not work on keywords"
  [spec]
  (if (keyword? spec)
    spec
    (or (::codec (meta spec))
        (throw (ex-info "No codec defined for the given spec" {:spec spec :meta (meta spec)})))))

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
  spec to k.  The spec is assumed to be part of the metadata of the passed codec"
  [k specified-codec]
  `(do 
     (register-codec ~k (extract-codec ~specified-codec))
     (s/def ~k ~specified-codec)))


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

(s/def ::primitive-size ::word-size)
;; Get charset is a function that when given a java.nio.ByteOrder object
;; returns a charset.  This must be a function because some charsets are order specific
;; (I'm looking at you utf-16)
(s/def ::get-charset fn?)

(s/def ::custom-charset (s/keys :req [::primitive-size ::get-charset]))
(s/def ::charset (s/or :standard #{:iso-8859-1 :ascii :utf-8 :utf-16-bom :utf-16}
                       :custom ::custom-charset))
(s/def ::null-terminated-strings boolean?)

(def standard-charsets {:ascii {::primitive-size 1 ::get-charset (constantly StandardCharsets/US_ASCII)}
                        :iso-8859-1 {::primitive-size 1 ::get-charset (constantly StandardCharsets/ISO_8859_1)}
                        :utf-8 {::primitive-size 1 ::get-charset (constantly StandardCharsets/UTF_8)}
                        :utf-16-bom {::primitive-size 2 ::get-charset (constantly StandardCharsets/UTF_16)}
                        :utf-16 {::primitive-size 2 ::get-charset (fn [order] (if (= order ByteOrder/LITTLE_ENDIAN)
                                                                                StandardCharsets/UTF_16LE
                                                                                StandardCharsets/UTF_16BE))}
                                 })

(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})

(s/def ::base-encoding (s/keys :req [::word-size ::order ::charset ::null-terminated-strings]))

(def default-encoding (s/conform ::base-encoding {::word-size 1 
                                                  ::order :little
                                                  ::charset :utf-8
                                                  ::null-terminated-strings false}))

;;; Some decoding specs
(s/def ::index (s/and int? (complement neg?)))
(s/def ::byte-size ::index)
(s/def ::count ::index)
(s/def ::every-arg (s/keys))
(s/def ::indexed-args (s/map-of ::index (s/keys)))
(s/def ::keyed-args (s/map-of keyword? (s/keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for Codec protocol that look in the registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn alignment 
  "Returns the alignment of the specified codec"
  [specified-codec] 
  (let [codec (if (keyword? specified-codec) 
                (reg-resolve specified-codec)
                (extract-codec specified-codec))]
    (alignment* codec)))

(defn encode 
  "Returns an encoded binary collection."
  [specified-codec data] 
   (let [codec (if (keyword? specified-codec) 
                 (reg-resolve specified-codec)
                 (extract-codec specified-codec))]
     (encode* codec data)))

(defn decode [specified-codec bin & decoding-args] 
  "Given a binary sequence, this will return a tuple with the decoded value, and the
  rest of the binary that was unused"
  (let [args (s/conform (s/keys*) decoding-args)
        codec (if (keyword? specified-codec) 
                (reg-resolve specified-codec)
                (extract-codec specified-codec))]
    (if (s/invalid? args)
      (throw (ex-info "Invalid decoding args for decode" (s/explain-data (s/keys*) decoding-args)))
      (decode* codec 
               (trim-to-alignment (alignment* codec) (seq bin))
               args))))

(defn recode [specified-codec & encoding-args] 
  (let [enc (s/conform (s/keys*) encoding-args)
        codec (if (keyword? specified-codec) 
                (reg-resolve specified-codec)
                (extract-codec specified-codec))]
    (if (s/invalid? enc)
      (throw (ex-info "Invalid encoding for recode" (s/explain-data (s/keys*) encoding-args)))
      (with-meta (s/spec specified-codec)
                 {::codec (recode* codec enc)}))))

(defn- raw-alignment [codec-form] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (alignment* codec)))

(defn- raw-encode 
  [codec-form data] 
   (let [codec (if (keyword? codec-form) 
                 (reg-resolve codec-form)
                 codec-form)]
     (encode* codec data)))

(defn- raw-decode [codec-form bin decoding-args] 
  (let [args (s/conform (s/keys) (or decoding-args {}))
        codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (if (s/invalid? args)
      (throw (ex-info "Invalid decoding args for raw decode" (s/explain-data (s/keys) decoding-args)))
      (decode* codec 
               (trim-to-alignment (alignment* codec) bin)
               args))))

(defn- raw-recode [codec-form encoding-args] 
  (let [enc (s/conform (s/keys) encoding-args)
        codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (if (s/invalid? enc)
      (throw (ex-info "Invalid encoding for raw recode" (s/explain-data (s/keys) encoding-args)))
      (recode* codec enc))))


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
      (make-binary (seq (.array buff))
                   :align (alignment* this))))
  (decode* [this bin _] 
    (let [[prim remaining] (split-at size bin)
          bytes (.order (ByteBuffer/wrap (byte-array prim))
                         (get order-map (::order enc)))
          data (get-buffer bytes)]
      [data
       (indexed-binary (+ size (current-index bin))
                       remaining)]))
  (recode* [this encoding] (update this :enc merge encoding)))

(defn string-codec [current-enc]
  (let [{:keys [::force-alignment ::word-size ::order ::charset ::null-terminated-strings]} current-enc
        [charset-type charset-value] charset
        charset-map (if (#{:standard} charset-type)
                      (get standard-charsets charset-value)
                      charset-value)
        {:keys [::primitive-size ::get-charset]} charset-map
        order-instance (get order-map order)
        charset-instance (get-charset order-instance)
        take-until-null (fn [bytes]
                          (if (= primitive-size 1)
                            (take-while (complement zero?)  bytes)
                            (flatten (take-while (complement #(= (repeat primitive-size 0) %))
                                                 bytes))))]
    (reify Codec
      (alignment* [this] (or force-alignment
                          (min word-size primitive-size)))
      (encode* [this data] 
        (let [byte-string (seq (.getBytes (or data  "") charset-instance))
              result (if null-terminated-strings
                       (concat byte-string (seq (.getBytes (str \u0000) charset-instance)))
                       byte-string)]
          (make-binary result :align (alignment* this))))
      (decode* [this bin decoding-args]
        (let [{:keys [::byte-size]} decoding-args
              string-bytes (cond
                             null-terminated-strings (take-until-null bin)
                             byte-size (take byte-size bin)
                             :else bin)
              string-size (if null-terminated-strings
                            (+ primitive-size (count string-bytes))
                            (count string-bytes))
              remaining (drop string-size bin)]
          [(String. (byte-array string-bytes) charset-instance)
           (indexed-binary (+ string-size (current-index bin))
                           remaining)]))
      (recode* [this enc] (with-meta (string-codec (merge current-enc enc))
                                     (meta this))))))


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
  {:int8 (with-meta (signed-primitive-spec Byte)
                    {::codec (reify-primitive Byte .get .put byte)})


   :int16 (with-meta (signed-primitive-spec Short)
                     {::codec (reify-primitive Short .getShort .putShort short)})


   :int32 (with-meta (signed-primitive-spec Integer)
                     {::codec (reify-primitive Integer .getInt .putInt int)})


   :int64 (with-meta (signed-primitive-spec Long)
                     {::codec (reify-primitive Long .getLong .putLong long)})


   :uint8 (with-meta (unsigned-primitive-spec Byte)
                     {::codec (reify-primitive Byte .get .put unchecked-byte Byte/toUnsignedLong)})


   :uint16 (with-meta (unsigned-primitive-spec Short)
                      {::codec (reify-primitive Short .getShort .putShort unchecked-short Short/toUnsignedLong)})


   :uint32 (with-meta (unsigned-primitive-spec Integer)
                      {::codec (reify-primitive Integer .getInt .putInt unchecked-int Integer/toUnsignedLong)})


   :float32 (with-meta  (signed-primitive-spec Float)
                       {::codec (reify-primitive Float .getFloat .putFloat float)})


   :float64 (with-meta (signed-primitive-spec Double)
                       {::codec (reify-primitive Double .getDouble .putDouble double)})


   :utf-8 (with-meta string?
                     {::codec (string-codec default-encoding)})


   :utf-16 (with-meta string?
                      {::codec (recode* (string-codec default-encoding) (s/conform (s/keys) {::charset :utf-16}))})

   })

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
                          `(edn-to-binary.core/def 
                             ~k-form 
                             (with-meta (~k-proto primitive-prototypes)
                                        {::codec (recode* (extract-codec (~k-proto primitive-prototypes)) 
                                                          (s/conform (s/keys*) [~@encoding-args]))})))
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
     `(register-primitives ~n ~@encoding-args))))

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
     `(register-primitives ~n ~@encoding-args))))

(register-primitives-in-ns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec wrapper functions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro and 
  "and acts as a wrapper for clojure.spec.alpha/and.  This will take a list of
  specs, find the spec with a codecm, and return a call to s/and with the codec
  annotated in the metadata"
  [& codec-forms]
  (let [specified-codec `(some (fn [form#] (or (and (get-codec form#) form#)
                                              (-> form# (meta) ::codec)))
                              [~@codec-forms])]
    `(with-meta (s/and ~@codec-forms)
                {::codec ~specified-codec})))


(defmacro nilable 
  "Wrapper for clojure.spec.alpha/nilable.  Allows for nil to be conformed.
  Most codecs will be ale to handle nil inputs"
  [codec]
  `(with-meta (s/nilable ~codec)
              {::codec (extract-codec ~codec)}))


(defn align-impl [codec align-to]
  (reify Codec
    (alignment* [_] (max align-to (raw-alignment codec)))
    (encode* [this data] (make-binary (encode codec data)
                                      :align (alignment* this)))
    (decode* [this bin decoding-args] (throw (UnsupportedOperationException. "Not implemented yet!")))
    (recode* [this encoding] (align-impl (apply recode codec (clojure.core/flatten (seq encoding))) align-to))))

(defmacro align [codec align-to]
  (let [align-spec (if (keyword? codec)
                     `(s/spec ~codec)
                     `codec)]
    `(with-meta ~align-spec
                {::codec (align-impl (extract-codec ~codec) ~align-to)})))

(defn fixed-decoder [codec & fixed-decoding-args]
  (let [fixed-args (s/conform (s/keys*) fixed-decoding-args)]
    (reify Codec
      (alignment* [_] (raw-alignment codec))
      (encode* [_ data] (raw-encode codec data))
      (decode* [_ bin decoding-args] (raw-decode codec bin (merge decoding-args fixed-args)))
      (recode* [_ encoding] (apply fixed-decoder
                                   (raw-recode codec encoding)
                                   fixed-decoding-args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn array-impl 
  ([codec]
   (reify Codec
     (alignment* [_] (raw-alignment codec))
     (encode* [this data] (make-binary (mapv (partial raw-encode codec) data)
                                       :align (alignment* this)))
     (decode* [this bin decoding-args]
       (let [{:keys [::count ::indexed-args ::every-arg]} decoding-args
             elem-args (if (some? count)
                         (map #(merge every-arg (get indexed-args %)) (range count))
                         (map #(merge every-arg (get indexed-args %)) (range)))]
         (reduce (fn [[data-accum current-rem] arg]
                   (let [[new-data bin-rem] (raw-decode codec current-rem arg)
                         result [(conj data-accum new-data)
                                 bin-rem]]
                     (if (empty? bin-rem)
                       (reduced result)
                       result)))
                 [[] bin]
                 elem-args)))
     (recode* [_ encoding] (array-impl (raw-recode codec encoding)))))
  ([codec align-to]
   (let [element-alignment (max align-to (raw-alignment codec))]
     (reify Codec
       (alignment* [_] element-alignment)
       (encode* [this data] (make-binary (mapv #(make-binary (raw-encode codec %)
                                                             :align element-alignment)
                                               data)
                                         :align element-alignment))
       (decode* [this bin decoding-args] 
         (let [{:keys [::count ::indexed-args ::every-arg]} decoding-args
               elem-args (if (some? count)
                           (map #(merge every-arg (get indexed-args %)) (range count))
                           (map #(merge every-arg (get indexed-args %)) (range)))]
           (reduce (fn [[data-accum current-rem] arg]
                     (let [[new-data bin-rem] (raw-decode codec current-rem arg)
                           result [(conj data-accum new-data)
                                   bin-rem]]
                       (if (empty? bin-rem)
                         (reduced result)
                         result)))
                   [[] bin]
                   elem-args)))
       (recode* [_ encoding] (array-impl (raw-recode codec encoding)))))))


(defmacro array 
  "Create an array codec and spec.
  The array macro takes the same arguments as s/coll-of and s/every, with 2 differences

  First, there is no support for an :into argument.  All results from the array spec
  will be conformed into a vector

  Second, there is an optional :align field.  The align field will be applied to every
  element in the array
  
  If :count is not specified in the arguments, then the decoding args can take a :edn-to-binary.core/count
  field that will limit the amount of units decoded. Decoding can also support :edn-to-binary.core/indexed-args
  and :edn-to-binary.core/every-arg
  "
  [specified-codec & {:keys [align kind count max-count min-count distinct gen-max gen]}]
  (let [array-imp  `(if ~align
                     (array-impl (extract-codec ~specified-codec) ~align)
                     (array-impl (extract-codec ~specified-codec)))
        codec-imp `(if ~count
                     (fixed-decoder ~array-imp ::count ~count)
                     ~array-imp)]
    `(with-meta (s/coll-of ~specified-codec
                           :into []
                           :kind ~kind
                           :count ~count
                           :max-count ~max-count
                           :min-count ~min-count
                           :distinct ~distinct
                           :gen-max ~gen-max
                           :gen ~gen)
                {::codec ~codec-imp})))

(defn tuple-impl [codecs implicit-decoders]
  (reify Codec
    (alignment* [_] (apply max (map raw-alignment codecs)))
    (encode* [this data] (make-binary (map raw-encode codecs data)
                                   :align (alignment this)))
    (decode* [this bin decoding-args] 
      (let [{:keys [::indexed-args ::every-arg]} decoding-args]
        (reduce (fn [[data-accum current-rem] [i codec]]
                  (let [implicit-fn (get implicit-decoders i (constantly nil))
                        args (merge 
                               every-arg
                               (get indexed-args i)
                               (implicit-fn data-accum decoding-args))
                        [new-data bin-rem] (raw-decode codec current-rem args)]
                    [(conj data-accum new-data)
                     bin-rem]))
                [[] bin]
                (map-indexed vector codecs))))
    (recode* [_ encoding] (tuple-impl 
                            (map #(raw-recode % encoding)
                                 codecs)
                            implicit-decoders))))

(def implicit-decoder (constantly nil))

(defn implicit-decoder? [maybe-sym]
  (if (symbol? maybe-sym) 
    (= #'implicit-decoder (resolve maybe-sym))
    nil))

(defmacro tuple 
  "Takes one or more specified codecs and returns a tuple spec/codec.

  Tuple decoders support ::indexed-args and ::every-arg, along with an implicit
  decoder function

  Each specified codec may be an implicit-decoder. An implicit decoder takes
  a function of data accumulated so far, and the decoding arguments, and returns
  a map that can be used as decoding margs.  This is called using a sequence
  (e/implicit-decoder
    (fn [data args] ....)
    codec)
  "
  [& specified-codecs]
  (let [[implicit-decoders specs] (reduce 
                                    (fn [[de sp] [i sc]]
                                      (if (seq? sc)
                                        (if (implicit-decoder? (first sc))
                                          [(assoc de i (nth sc 1)) (conj sp (nth sc 2))]
                                          [de (conj sp sc)])
                                        [de (conj sp sc)]))
                                    [{} []]
                                    (map-indexed vector specified-codecs))]
  `(with-meta (s/tuple ~@specs)
              {::codec (tuple-impl (mapv extract-codec [~@specs]) ~implicit-decoders)})))

(defn struct-impl [key-codec-pairs implicit-decoders]
  (let [key-order (map first key-codec-pairs)
        codecs (map second key-codec-pairs)
        ordered-values (fn [coll]
                        (map #(get coll %) key-order))]
    (reify Codec
      (alignment* [this] (apply max (map raw-alignment codecs)))
      (encode* [this data] (make-binary (zipmap key-order
                                                (map raw-encode codecs (ordered-values data)))
                                        :align (alignment* this)
                                        :order key-order))
      (decode* [this bin decoding-args] 
        (let [{:keys [::keyed-args ::every-arg]} decoding-args]
          (reduce (fn [[data-accum current-rem] [k codec]]
                    (let [implicit-fn (get implicit-decoders k (constantly nil))
                          args (merge 
                                 every-arg
                                 (get keyed-args k)
                                 (implicit-fn data-accum decoding-args))
                          [new-data bin-rem] (raw-decode codec current-rem args)]
                      [(assoc data-accum k new-data)
                       bin-rem]))
                  [{} bin]
                  (map vector key-order codecs))))
      (recode* [_ encoding] 
        (let [ recoded-codecs (map #(raw-recode % encoding)
                                  codecs)
              args (map vector key-order recoded-codecs)]
          (struct-impl args))))))

(def unqualified (constantly nil))

(defn unqualified? [maybe-sym]
  (if (symbol? maybe-sym) 
    (= #'unqualified (resolve maybe-sym))
    nil))


(defmacro struct 
  "Takes a list of registered spec/codecs and creates a map spec/codec.

  Spec requires that all maps are conformed with fully qualified keywords. That is why calls
  to spec take the form '(s/keys :req [...] :req-un [...])

  This is a bit of a problem for codecs, since order is very importatnt to a struct.
  So a struct takes a list of arguments.  Args can either be keywords or sequences.

  The following call:
  (e/struct ::foo
            ::bar
            ::baz)
  would conform a map of {::foo ... ::bar ... ::baz ...} and encode in that order

  However, the following call:
  (e/struct ::foo
            (unqualified ::bar)
            (unqualified ::baz))
  would conform a map of {::foo ... :bar ... :baz ...} The order is maintianed, but :bar
  and :baz no longer need their namespace

  structs can also use implicit-decoders
  "
  [& registered-codecs]
  (let [[implicit-decoders specs] (reduce 
                                    (fn [[de sp] sc]
                                      (if (seq? sc)
                                        (let [[sym decoder key] sc]
                                          (if (implicit-decoder? sym)
                                            [(assoc de key decoder) (conj sp key)]
                                            (throw (IllegalArgumentException. "Only implicit decoders may be called from struct field"))))
                                        [de (conj sp sc)]))
                                    [{} []]
                                    registered-codecs)
        unk #(-> % name keyword)
        qualified-order (fn [k] [k k])
        unqualified-order (fn [k] [(unk k) k])
        [req req-un order] (reduce (fn [[req req-un order] f]
                                     (cond 
                                       (keyword? f) [(conj req f) req-un (conj order (qualified-order f))]
                                       (and (seq? f)
                                            (unqualified? (first f))) [req (conj req-un (second f)) (conj order (unqualified-order (second f)))]
                                       :else (throw (ex-info "Struct field is not qualified keyword or unqualified sequence"
                                                             {:field f}))))


                                   [[] [] []]
                                   specs)]
    `(with-meta (s/keys :req [~@req] :req-un [~@req-un])
                {::codec (struct-impl [~@order] ~implicit-decoders)})))

;TODO Add in support for decoding with tagged union
(defn union-impl [codec-map]
  (reify Codec
      (alignment* [this] (apply max (map raw-alignment (vals codec-map))))
      (encode* [this [tag data]]
        (let [codec (get codec-map tag)]
          (raw-encode codec data)))
      (decode* [this bin decoding-args] (throw (UnsupportedOperationException. "Not implemented yet!")))
      (recode* [_ encoding] (union-impl (zipmap (keys codec-map)
                                                 (mapv #(raw-recode % encoding) (vals codec-map)))))))

(defmacro union [& key-codec-forms]
  (let [form-pairs (partition 2 key-codec-forms)
        codec-forms (mapv second form-pairs)
        codec-keys (mapv first form-pairs)]
  `(with-meta (s/or ~@key-codec-forms)
              {::codec (union-impl (zipmap ~codec-keys 
                                           (mapv extract-codec [~@codec-forms])))})))
  
;TODO Add in some support for a multicodec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common specs that can be used in codec definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn constant-field [field value]
  (s/conformer #(assoc % field value)))

(defn dependent-field [field f]
  (s/conformer #(assoc % field (f %))))

