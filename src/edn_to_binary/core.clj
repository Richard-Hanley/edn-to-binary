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
                       ::flaten true})

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

(extend-protocol Binary
  clojure.lang.Keyword
  ;; If collections are being flattened, then a keyword should be encoded as nil
  ;; Otherwise just return the keyword
  (encode* [this encoding]
    (if (not (::flatten encoding))
      this))
  clojure.lang.Sequential
  clojure.lang.IPersistentMap)

(defn alignment [binary-seq]
  (or (::alignment (meta binary-seq)) 1))
