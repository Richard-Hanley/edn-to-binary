(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol Codec
  (implicit?* [this])
  (alignment* [this])
  (encode* [this data])
  (decode* [this binary args]))

(defprotocol BinaryCollection
  (flatten [this]))

(defn make-binary 
  "Makes an annotated binary collection

  A binary collection is a recursive collection of bytes.  These
  collections may be sequential or associative

  Binary collections can have metadata that sets the alignment, or in the
  case of maps a key order

  Optional arguments are :key-order and :align"
  [bin & {:keys [key-order align]}]
  (with-meta bin
             (assoc (meta bin) ::alignment align ::key-order key-order)))

(defn indexed-binary [index binary-coll]
  (with-meta binary-coll
             (assoc (meta binary-coll) ::index index)))

(defn current-index [binary-coll]
  (or (::index (meta binary-coll)) 0))

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

(defn sizeof 
  "Given a bianry collection, this will return the size in bytes of the collection when alignment is applied"
  [bin]
  (count (flatten bin)))

(defn alignment-padding [align-to position]
  (let [bytes-over (mod position align-to)]
    (if (pos? bytes-over)
      (- align-to bytes-over)
      0)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for Codec protocol that look in the registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn implicit?
  "Returns whether a codec is implicit"
  [specified-codec] 
  (let [codec (if (keyword? specified-codec) 
                (reg-resolve specified-codec)
                (extract-codec specified-codec))]
    (implicit?* codec)))

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
 
(defn- raw-implicit?
  [codec-form] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (implicit?* codec)))


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

(def default-order ByteOrder/LITTLE_ENDIAN)
(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})


(defmacro signed-primitive-spec [class]
  `#(or (zero? %1)
        (<= (. ~class MIN_VALUE) %1 (. ~class MAX_VALUE))))

(defmacro unsigned-primitive-spec [class]
  `(s/int-in 0 (bit-shift-left 1 (. ~class SIZE))))

(defrecord PrimitiveCodec [size get-buffer put-buffer coerce-from]
  Codec
  (implicit?* [this] true)
  (alignment* [this]
    (let [{:keys [::force-alignment ::word-size] 
           :or {force-alignment nil word-size 1}} this]
      (or force-alignment
          (min size word-size))))
  (encode* [this data]
    (let [buff (.order (ByteBuffer/allocate size)
                       (get order-map (::order this) default-order))
          _ (put-buffer buff (or data 0))]
      (make-binary (seq (.array buff))
                   :align (alignment* this))))
  (decode* [this bin _] 
    (let [[prim remaining] (split-at size bin)
          bytes (.order (ByteBuffer/wrap (byte-array prim))
                         (get order-map (::order this)))
          data (coerce-from (get-buffer bytes))]
      [data
       (indexed-binary (+ size (current-index bin))
                       remaining)])))

(def primitive-codecs
  {Byte (map->PrimitiveCodec {:get-buffer #(.get %) :put-buffer #(.put %1 (unchecked-byte %2)) :size (Byte/BYTES) :coerce-from identity})
   Short (map->PrimitiveCodec {:get-buffer #(.getShort %) :put-buffer #(.putShort %1 (unchecked-short %2)) :size (Short/BYTES) :coerce-from identity})
   Integer (map->PrimitiveCodec {:get-buffer #(.getInt %) :put-buffer #(.putInt %1 (unchecked-int %2)) :size (Integer/BYTES) :coerce-from identity})
   Long (map->PrimitiveCodec {:get-buffer #(.getLong %) :put-buffer #(.putLong %1 (unchecked-long %2)) :size (Long/BYTES) :coerce-from identity})
   Float (map->PrimitiveCodec {:get-buffer #(.getFloat %) :put-buffer #(.putFloat %1 (unchecked-float %2)) :size (Float/BYTES) :coerce-from identity})
   Double (map->PrimitiveCodec {:get-buffer #(.getDouble %) :put-buffer #(.putDouble %1 (unchecked-double %2)) :size (Double/BYTES) :coerce-from identity})})


(defmacro primitive [prim & encoding]
  (let [c `(get primitive-codecs ~prim)
        enc `(if (s/valid? (s/keys*) [~@encoding])
               (s/conform (s/keys*) [~@encoding])
               (throw (ex-info "Unable to conform encoding" (s/explain-data (s/keys*) [~@encoding]))))]
  `(with-meta (signed-primitive-spec ~prim)
              {::codec (merge ~c ~enc)})))

(defmacro unsigned-primitive [prim & encoding]
  (let [coerce-unsigned `#(. ~prim (toUnsignedLong %))
        c `(assoc (get primitive-codecs ~prim) :coerce-from ~coerce-unsigned)
        enc `(if (s/valid? (s/keys*) [~@encoding])
               (s/conform (s/keys*) [~@encoding])
               (throw (ex-info "Unable to conform encoding" (s/explain-data (s/keys*) [~@encoding]))))]
  `(with-meta (unsigned-primitive-spec ~prim)
              {::codec (merge ~c ~enc)})))


(edn-to-binary.core/def ::int8 (primitive Byte))
(edn-to-binary.core/def ::int16 (primitive Short))
(edn-to-binary.core/def ::int32 (primitive Integer))
(edn-to-binary.core/def ::int64 (primitive Long))

(edn-to-binary.core/def ::uint8 (unsigned-primitive Byte))
(edn-to-binary.core/def ::uint16 (unsigned-primitive Short))
(edn-to-binary.core/def ::uint32 (unsigned-primitive Integer))

(edn-to-binary.core/def ::float (primitive Float))
(edn-to-binary.core/def ::double (primitive Double))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Codec wrappers

(defn align-impl [raw-codec align-to]
  (reify Codec
    (implicit?* [_] (raw-implicit? raw-codec))
    (alignment* [_] align-to)
    (encode* [_ data] (make-binary (raw-encode raw-codec data) :align align-to))
    (decode* [_ binary args] 
      (let [trimmed-bin (trim-to-alignment align-to binary)]
        (raw-decode raw-codec binary args)))))

(defmacro align 
  "Takes a given codec, and forces the alignment to align-to"
  [codec-spec align-to]
  (let [align-spec (if (keyword? codec-spec)
                     `(s/spec ~codec-spec)
                     `codec)]
    `(with-meta ~align-spec
                {::codec (align-impl (extract-codec ~codec-spec) ~align-to)})))


