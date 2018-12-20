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


(defmacro signed-primitive-spec [class conv]
  `(s/and #(<= (. ~class MIN_VALUE) %1 (. ~class MAX_VALUE))
          (s/conformer ~conv)))

(defmacro unsigned-primitive-spec [class conv]
  `(s/and (s/int-in 0 (bit-shift-left 1 (. ~class SIZE)))
          (s/conformer ~conv)))

(s/def ::int8 (signed-primitive-spec Byte byte))
(s/def ::int16 (signed-primitive-spec Short short))
(s/def ::int32 (signed-primitive-spec Integer int))
(s/def ::int64 (signed-primitive-spec Long long))

(s/def ::uint8 (unsigned-primitive-spec Byte unchecked-byte))
(s/def ::uint16 (unsigned-primitive-spec Short unchecked-short))
(s/def ::uint32 (unsigned-primitive-spec Integer unchecked-int))

(s/def ::float32 (signed-primitive-spec Float float))
(s/def ::float64 (signed-primitive-spec Double double))

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
  ;; A peristent map works like a sequntial collection, but there needs to be
  ;; a way to order the collection.  If the order is specified in the metadata,
  ;; then it will use that.  Otherwise the order will be based off a call to keys
  (encode* [this encoding]
    (let [m (meta this)
          struct-order (or (::struct-order m) (keys this))
          data-seq (reduce (fn [coll k] (conj coll (get this k)))
                           []
                           struct-order)
          bin-seq (encode* (with-meta data-seq m) encoding)]
      (if (::flatten encoding)
        bin-seq
        (zipmap struct-order bin-seq)))))

(defn align [align-to index] 
  (fn [metadata]
    (update metadata ::force-align conj [index align-to])))

; (defn push [& encoding-key-val-pairs]
;   (fn [enc-stack]))

; (defn pop []
;   (fn [enc-stack]))

(defmacro array 
  [enc-pred & {:keys [align encoding into kind count max-count min-count distinct gen-max gen]
               :or {into []}}]
  (let [meta-fn `(fn [coll#]
                  (if ~align
                    (assoc (meta coll#) 
                           ::force-align (map-indexed vector (repeat (count coll#) ~align)))
                   (meta coll#)))]
  `(s/and (s/coll-of ~enc-pred 
                     :into ~into
                     :kind ~kind
                     :count ~count
                     :max-count ~max-count
                     :min-count ~min-count
                     :distinct ~distinct
                     :gen-max ~gen-max
                     :gen ~gen)
          (s/conformer #(with-meta %
                                   (~meta-fn %))))))

(defn res-pragma [pred-encoders]
  (let [[specs pragmas _] (reduce (fn [[specs pragmas index] pred]
                                    (if (and (seq? pred)
                                             (= (first pred) :pragma))
                                      (let [pragma-form (concat (rest pred) [index])]
                                        [specs (conj pragmas pragma-form) index])
                                      [(conj specs pred) pragmas (inc index)]))
                                  [[] [] 0]
                                  pred-encoders)]
    [specs pragmas]))

(defmacro tuple [& pred-encoders]
  (let [[specs pragmas] (res-pragma pred-encoders)
        metadata `(reduce #(%2 %1) {} ~pragmas)]
    `(s/and (s/tuple ~@specs)
           (s/conformer #(with-meta % 
                                    (merge (meta %) ~metadata))))))


(defmacro struct [& key-pred-encoders]
  (let [[specs pragmas] (res-pragma key-pred-encoders)
        metadata `(reduce #(%2 %1) {} ~pragmas)
        unk #(-> % name keyword)
        [req req-un order] (reduce (fn [[req req-un order] f]
                                     (cond 
                                       (keyword? f) [(conj req f) req-un (conj order f)]
                                       (and (seq? f)
                                            (= (first f) :unqualified)) [req (conj req-un (second f)) (conj order (unk (second f)))]
                                       :else (throw (ex-info "Struct field is not qualified keyword or unqualified sequence"
                                                             {:field f}))))

                               
                             [[] [] []]
                             specs)
        ]
    `(s/and (s/keys :req [~@req] :req-un [~@req-un])
            (s/conformer #(with-meta % 
                                     (assoc (merge (meta %) ~metadata) 
                                            ::struct-order ~order))))))
