(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol Binary
  (encode* [this encoding]))

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

(defn encode
  ([data] (encode* data default-encoding))
  ([data & encoding-key-vals] 
   (let [encoding (apply array-map encoding-key-vals)]
     (encode* data (merge default-encoding encoding)))))

(defmacro put-prim [class put-method]
  (let [size `(. ~class BYTES)]
    `(fn [data# encoding#]
       (let [{word-size# ::word-size
              order# ::order} encoding#
             buffer# (.order (ByteBuffer/allocate ~size)
                            (get order-map order#))
             align-to# (min word-size# ~size)]
         (with-meta (seq (.array (~put-method buffer# data#)))
                    {::alignment align-to#})))))

(extend-protocol Binary
  Byte
  (encode* [this encoding] ((put-prim Byte .put) this encoding))
  Short 
  (encode* [this encoding] ((put-prim Short .putShort) this encoding))
  Integer
  (encode* [this encoding] ((put-prim Integer .putInt) this encoding))
  Long
  (encode* [this encoding] ((put-prim Long .putLong) this encoding))
  Float
  (encode* [this encoding] ((put-prim Float .putFloat) this encoding))
  Double
  (encode* [this encoding] ((put-prim Double .putDouble) this encoding)))

(defn alignment [binary-seq]
  (or (::alignment (meta binary-seq)) 1))

(defn update-alignment [binary-seq align-to]
  (with-meta binary-seq
             (assoc (meta binary-seq) ::alignment align-to)))

(defn make-binary
  ([bin] (seq bin))
  ([bin align-to] (with-meta (seq bin)
                             (assoc (meta bin) ::alignment align-to))))

(defn alignment-padding [align-to position]
  (let [bytes-over (mod position align-to)]
    (if (pos? bytes-over)
      (- align-to bytes-over)
      0)))

(defn binary-concat 
  "Concatenates a series of binary sequences, adding in alignment padding as needed
  The resulting binary sequence will have an alignment equal to the greatest of all the passed
  sequences"
  [& bin-seqs]
  (let [[_ coll-align coll] (reduce (fn [[position coll-alignment coll] bin]
                                     (let [align-to (alignment bin)
                                           padding (repeat (alignment-padding align-to position) (byte 0))]
                                       [(+ (count coll) (count padding) (count bin))
                                        (max coll-alignment align-to)
                                        (concat coll padding bin)]))
                                   [0 1 nil]
                                   bin-seqs)]
    (make-binary coll coll-align)))


(extend-protocol Binary
  clojure.lang.Keyword
  ;; If collections are being flattened, then a keyword should be encoded as nil
  ;; Otherwise just return the keyword
  (encode* [this encoding]
    (if (not (::flatten encoding))
      this))
  clojure.lang.Sequential
  ;; A sequntial object may have some metadata that improves encoding
  ;;
  ;; First, there may be a data-xform.  A data x-form takes that sequential argument
  ;; and transforms it into some new sequential data
  ;;
  ;; Second, there may be an encoding transform.  And encoding x-form takes a single encoding
  ;; and create a sequence of encodings that can be directly mapped to the result of the data 
  ;; x-form
  ;;
  ;; Third, there may be a force alignment vector, that forces the alignment of certain
  ;; results
  (encode* [this encoding]
    (let [{:keys [::data-xform ::encoding-xform ::force-align] 
           :or {data-xform identity encoding-xform repeat}}
           (meta this)
          encs (encoding-xform encoding)
          data (data-xform this)
          bin-seq (mapv encode* data encs)
          aligned-seq (reduce (fn [bs [index align-to]]
                                (update bs index update-alignment align-to))
                              bin-seq
                              force-align)]
      (if (::flatten encoding)
        (apply binary-concat aligned-seq)
        aligned-seq)))
  clojure.lang.IPersistentMap
  (encode* [this encoding]))

