(ns edn-to-binary.core)


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


(defn align [align-to]
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
