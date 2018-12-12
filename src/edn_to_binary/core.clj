(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol BinaryField
  (binary-seq [this meta]))


(extend-protocol BinaryField
  Byte
  (binary-seq [this _] (cons this '()))
  Short
  (binary-seq [this _] (cons (unchecked-byte this) '()))
  Integer
  (binary-seq [this _] (cons (unchecked-byte this) '()))
  Long
  (binary-seq [this _] (cons (unchecked-byte this) '())))


(defn alignment-padding [align-to position]
  (let [bytes-over (mod position align-to)]
    (if (pos? bytes-over)
      (- align-to bytes-over)
      0)))


(defrecord Alignment [align-to]
  BinaryField
  (binary-seq [this meta]
    (let [{:keys [position] :or {position 0}} meta
          padding (alignment-padding align-to position)]
      (repeat padding (byte 0)))))

(defn alignment? [maybe-alignment] (instance? Alignment maybe-alignment))


(defprotocol BinaryCollection
  (coll-alignment [coll])
  (binary-field-seq [coll]))

(extend-protocol BinaryCollection
  clojure.lang.ISeq
  (coll-alignment [coll]
    (let [head (first coll)]
      (if (alignment? head)
        (:align-to head)
        1)))
  (binary-field-seq [coll] coll)
  clojure.lang.IPersistentMap
  (coll-alignment [coll]
    (let [head (::alignment coll)]
      (if (alignment? head)
        (:align-to head)
        1)))
  (binary-field-seq [coll] (vals coll)))

(defn make-alignment [align-to]
  (map->Alignment {:align-to align-to}))

(defn 
  serialize
  "Returns a sequence of Bytes when passed a collection binary data.  Returns a transducer
  when no collection is provided"
  ([]
   (fn [xf]
     (let [length (volatile! 0)]
       (fn 
         ([] (xf))
         ([result] (xf result))
         ([result input] 
          (let [current-length @length
                input-bytes (binary-seq input {:position current-length})]
            (vreset! length (+ (count input-bytes) current-length))
            (reduce xf result input-bytes)))))))
  ([binary-stream]
   (sequence (serialize) binary-stream)))

(defn to-binary [binary-coll]
  (serialize (flatten (binary-field-seq binary-coll))))


(defprotocol Codec
  (encoder* [this encoding] "Encoder creates a function that will take data, and return a binary collection")
  (decoder* [this encodings] "A decoder works like an encoder, but the function is a bit more complicated

                             The decoder function that is returned takes a sequence of binary data, and an 
                             optional map of decoder arguments.  The decoder arguments can be used to specify
                             additional type information that may have been stripped from the binary edn data type
                             when encoding (e.g. position or number of elements in an array)

                             A decoder function returns a vector of [data bytes-read remaining-bytes]")
  (encoding-spec* [this] "Returns a set of keyword specs that this Codec expects to have defined in the encoding
                         The encoding is a map that is expected to conform to a spec written like (s/keys :req [~@(encoding-spec this)])

                         (The reason this is returning just the keywords instead of a fully reified spec is because it removes
                         much of the complexity in this protocol to not have to worry about making the call to spec macros.)"))

(defn get-codec-spec [codec]
  (if (keyword? codec)
    codec
    (or (::spec (meta codec))
        identity)))

(defonce ^:private registry-ref (atom {}))

(defn registry
  "returns the registry map, prefer 'get-codec' to lookup a codec by name"
  []
  @registry-ref)

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
  ([k codec]
  `(do 
     (register-codec ~k ~codec)
     (s/def ~k (get-codec-spec ~codec))))
  ([k codec & {:keys [spec post-spec] 
              :or {spec identity post-spec identity}}]
  `(do 
     (register-codec ~k ~codec)
     (s/def ~k (s/and
                 ~spec
                 (get-codec-spec ~codec)
                 ~post-spec)))))


(defn encoder [k-or-c encoding]
  (let [codec (if (keyword? k-or-c) 
                (reg-resolve k-or-c)
                k-or-c)]
    (encoder* codec encoding)))

(defn decoder [k-or-c encoding]
  (let [codec (if (keyword? k-or-c) 
                (reg-resolve k-or-c)
                k-or-c)]
    (decoder* codec encoding)))

(defn encoding-spec [k-or-c]
  (let [codec (if (keyword? k-or-c) 
                (reg-resolve k-or-c)
                k-or-c)]
    (encoding-spec* codec)))


(s/def ::word-size #{1 2 4 8})
(s/def ::order #{:little :big :network :native})

(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})

(s/def ::base-encoding (s/keys :req [::word-size ::order]))
(def base-encoding-keys #{::word-size ::order})

(def default-encoding {::word-size 1 ::order :little})

(defmacro reify-primitive 
  ([class get put coerce-to] `(reify-primitive ~class ~get ~put ~coerce-to identity))
  ([class get put coerce-to coerce-from]
   (let [size `(. ~class BYTES)
         get-fn `#(~coerce-from (~get %))
         put-fn `#(~put %1 (~coerce-to %2))]
     `(primitive-impl ~size ~get-fn ~put-fn))))

(defn primitive-impl [size get-buffer put-buffer]
  (reify Codec
    (encoder* [_ encoding]
      (fn [data]
        (let [alignment (min size (::word-size encoding))
              buff (.order (ByteBuffer/allocate size)
                           (get order-map (::order encoding)))
              ;; In case a nil valeu is passed, set the data to 0
              _ (put-buffer buff (or data 0))
              encoded-data (seq (.array buff))]
          (cons (Alignment. alignment) encoded-data))))
    (decoder* [_ encoding]
      (fn decoding-fn
        ([binary] (decoding-fn binary {::position 0}))
        ([binary decoding-args]
         (let [alignment (min size (::word-size encoding))
               bytes-to-align (alignment-padding alignment (::position decoding-args))
               aligned-binary (drop bytes-to-align binary)
               buff (.order (ByteBuffer/wrap (byte-array (take size aligned-binary)))
                            (get order-map (::order encoding)))
               data (get-buffer buff)
               remaining (drop size aligned-binary)]
           [data (+ size bytes-to-align) remaining]))))
    (encoding-spec* [_] base-encoding-keys)))

(defmacro signed-primitive-spec [class]
  `#(<= (. ~class MIN_VALUE) %1 (. ~class MAX_VALUE)))

(defmacro unsigned-primitive-spec [class]
  `(s/int-in 0 (bit-shift-left 1 (. ~class SIZE))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(edn-to-binary.core/def ::int8 (with-meta (reify-primitive Byte .get .put byte)
                                          {::spec (signed-primitive-spec Byte)}))

(edn-to-binary.core/def ::int16 (with-meta (reify-primitive Short .getShort .putShort short)
                                           {::spec (signed-primitive-spec Short)}))

(edn-to-binary.core/def ::int32 (with-meta (reify-primitive Integer .getInt .putInt int)
                                           {::spec (signed-primitive-spec Integer)}))

(edn-to-binary.core/def ::int64 (with-meta (reify-primitive Long .getLong .putLong long)
                                           {::spec (signed-primitive-spec Long)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(edn-to-binary.core/def ::uint8 (with-meta (reify-primitive Byte .get .put unchecked-byte Byte/toUnsignedLong)
                                           {::spec (unsigned-primitive-spec Byte)}))

(edn-to-binary.core/def ::uint16 (with-meta (reify-primitive Short .getShort .putShort unchecked-short Short/toUnsignedLong)
                                            {::spec (unsigned-primitive-spec Short)}))

(edn-to-binary.core/def ::uint32 (with-meta (reify-primitive Integer .getInt .putInt unchecked-int Integer/toUnsignedLong)
                                            {::spec (unsigned-primitive-spec Integer)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(edn-to-binary.core/def ::float32 (with-meta (reify-primitive Float .getFloat .putFloat float)
                                             {::spec (signed-primitive-spec Float)}))

(edn-to-binary.core/def ::float64 (with-meta (reify-primitive Double .getDouble .putDouble double)
                                             {::spec (signed-primitive-spec Double)}))

(defn codec-seq [codecs]
  (reify Codec
    (encoder* [_ encoding]
      (let [encoders (map #(encoder %1 encoding) codecs)]
        (fn [data]
          ;; Pad the data with nil to make sure all of the neoc
          ;; That way all of the encoders will be sent data
          (let [encoded-data (map #(%1 %2) encoders (concat data (repeat nil)))
                encoded-alignment (make-alignment (apply max (map coll-alignment encoded-data)))]
            (cons encoded-alignment encoded-data)))))
    (decoder* [_ encoding]
      (let [decoders (map #(decoder %1 encoding) codecs)]
        (fn decoding-fn
          ([binary])
          ([binary decoding-args])
          )))
    (encoding-spec* [_] (apply set/union 
                               (map encoding-spec codecs)))))

(defn codec-seq-infinite [specs codecs]
  (reify Codec
    (encoder* [_ encoding]
      (let [encoders (map #(encoder %1 encoding) codecs)]
        (fn [data]
          (let [encoded-data (map #(%1 %2) encoders data)
                encoded-alignment (make-alignment (apply max (map coll-alignment encoded-data)))]
            (cons encoded-alignment encoded-data)))))
    (decoder* [_ encoding]
      (let [decoders (map #(decoder %1 encoding) codecs)]
        (fn decoding-fn
          ([binary])
          ([binary decoding-args])
          )))
    (encoding-spec* [_] specs)))

(defn array-impl 
  ([codec] (codec-seq-infinite (encoding-spec codec) (repeat codec)))
  ([n codec] (codec-seq (repeat n codec))))

(defmacro array 
  [codec & {:keys [into kind count max-count min-count distinct gen-max gen]}]
  (let [c `(if (some? ~count)
             (array-impl ~count ~codec )
             (array-impl ~codec))]
    `(with-meta ~c
                {::spec (s/coll-of ~(get-codec-spec codec) 
                                   :into ~into
                                   :kind ~kind
                                   :count ~count
                                   :max-count ~max-count
                                   :min-count ~min-count
                                   :distinct ~distinct
                                   :gen-max ~gen-max
                                   :gen ~gen)})))



(defn tuple-impl [codecs] (codec-seq codecs))

(defmacro tuple [& codecs]
  (let [cs (map #(list get-codec-spec %) codecs)]
    `(with-meta (tuple-impl ~codecs)
                {::spec (s/tuple ~@cs)})))
                                

(defn struct-impl [key-codec-pairs]
  (let [ks (map first key-codec-pairs)
        codecs (map second key-codec-pairs)
        raw-codec (codec-seq codecs)]
    (reify Codec
      (encoder* [_ encoding]
        (let [raw-encoder (encoder raw-codec encoding)]
          (fn [data]
            (let [values (map #(%1 data) ks)
                  encoded-values (raw-encoder values)
                  encoded-keys (cons ::alignment ks)
                  encoded-entries (map #(clojure.lang.MapEntry. %1 %2) encoded-keys encoded-values)]
              (into (array-map) encoded-entries)))))
      (decoder* [_ encoding]
        (fn decoding-fn
          ([binary])
          ([binary decoding-args])))
      (encoding-spec* [_] (encoding-spec raw-codec)))))

;TODO: Figure out how to use unqualified keywords
(defmacro struct [& codec-keys]
  `(with-meta (struct-impl (map #(repeat 2 %) [~@codec-keys]))
              {::spec (s/keys :req [~@codec-keys])}))

(defn multi-codec-impl [mm-codec]
  (reify Codec
    (encoder* [_ encoding] 
      (fn [data]
        (let [enc-fn (encoder (mm-codec data) encoding)]
          (enc-fn data))))
    (decoder* [_ encoding]
      (fn decoding-fn
        ([binary])
        ([binary decoding-args])))
      (encoding-spec* [_] nil)))

(defmacro multi-codec 
  "A multi-codec is an open way to delegate codecs.  It is significantly
  slower, and cannot properly generate encoding-spec"
  [mm-codec mm-spec retag]
  `(with-meta (multi-codec-impl ~mm-codec)
              {::spec (s/multi-spec ~mm-spec ~retag)}))

(defmacro register-codec-method [mm-codec mm-spec dispatch-value codec-key]
  `(do
     (defmethod ~mm-spec ~dispatch-value [~'arg] ~codec-key)
     (defmethod ~mm-codec ~dispatch-value [~'arg] ~codec-key)
     nil))
