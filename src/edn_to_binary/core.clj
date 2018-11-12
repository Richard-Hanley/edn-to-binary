(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]))


; (defprotocol Codec
;   (alignment* [this])
;   (encode* [this data])
;   (decode* [this binary])
;   (encoding* [this] "Returns the encoding map of the passed Codec")
;   (encoding-format* [this])
  ; (recode [this new-encoding] "Returns a new Codec with a the new encoding"))

; (defn encode [codec data])

; (defn decode [codec binary])

; (defn indexed-binary-xform []
;   (map-indexed vector))

; ; (defn serialize [binary] (byte-array (mapcat unchecked-byte binary)))
; (defn serialize [binary] (byte-array (map unchecked-byte binary)))

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


(defprotocol Codec
  (encoder [this encoding] "Encoder creates a function that will take data, and return a binary collection")
  (decoder [this encodings] "A decoder works like an encoder, but the function is a bit more complicated
                            
                            The decoder function that is returned takes a sequence of binary data, and an 
                            optional map of decoder arguments.  The decoder arguments can be used to specify
                            additional type information that may have been stripped from the binary edn data type
                            when encoding (e.g. position or number of elements in an array)

                            A decoder function returns a vector of [data bytes-read remaining-bytes]")
  (encoding-spec [this] "Returns a spec for the encoder and decoder functions of this codec"))

(s/def ::word-size #{1 2 4 8})
(s/def ::order #{:little :big :network :native})

(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})

(s/def ::base-encoding (s/keys :req [::word-size ::order]))

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
    (encoder [_ encoding]
      (fn [data]
        (let [alignment (min size (::word-size encoding))
              buff (.order (ByteBuffer/allocate size)
                           (get order-map (::order encoding)))
              _ (put-buffer buff data)
              encoded-data (seq (.array buff))]
          (if (> alignment 1)
            (cons (Alignment. alignment) encoded-data)
            encoded-data))))
    (decoder [_ encoding]
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
    (encoding-spec [_] ::base-encoding)))


(def int8 (reify-primitive Byte .get .put byte))
(def int16 (reify-primitive Short .getShort .putShort short))
(def int32 (reify-primitive Integer .getInt .putInt int))
(def int64 (reify-primitive Long .getLong .putLong long))

(def uint8 (reify-primitive Byte .get .put unchecked-byte Byte/toUnsignedLong))
(def uint16 (reify-primitive Short .getShort .putShort unchecked-short Short/toUnsignedLong))
(def uint32 (reify-primitive Integer .getInt .putInt unchecked-int Integer/toUnsignedLong))

(def float32 (reify-primitive Float .getFloat .putFloat float))
(def float64 (reify-primitive Double .getDouble .putDouble double))

