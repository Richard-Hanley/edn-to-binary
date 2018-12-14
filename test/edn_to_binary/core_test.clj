(ns edn-to-binary.core-test
  (:require [clojure.test :refer :all]
            [edn-to-binary.core :as bin]))

(deftest primitive-encoding
  (testing "little endian"
    (testing "word-size 1"
      (let [val (bin/encode (byte 12) ::bin/word-size 1 ::bin/order :little)]
        (is (= [12] val))
        (is (= 1 (bin/alignment val))))
      (let [val (bin/encode (short 12) ::bin/word-size 1 ::bin/order :little)]
        (is (= [12 0] val))
        (is (= 1 (bin/alignment val))))
      (let [val (bin/encode (int 0x01234567) ::bin/word-size 1 ::bin/order :little)]
        (is (= [0x67 0x45 0x23 0x01] val))
        (is (= 1 (bin/alignment val))))
      (let [val (bin/encode (long 0x0123456776543210) ::bin/word-size 1 ::bin/order :little)]
        (is (= [0x10 0x32 0x54 0x76 0x67 0x45 0x23 0x01] val))
        (is (= 1 (bin/alignment val)))))
    (testing "word-size 2"
      (let [val (bin/encode (byte 12) ::bin/word-size 2 ::bin/order :little)]
        (is (= 1 (bin/alignment val))))
      (let [val (bin/encode (short 12) ::bin/word-size 2 ::bin/order :little)]
        (is (= 2 (bin/alignment val))))
      (let [val (bin/encode (int 0x01234567) ::bin/word-size 2 ::bin/order :little)]
        (is (= 2 (bin/alignment val))))
      (let [val (bin/encode (long 0x0123456776543210) ::bin/word-size 2 ::bin/order :little)]
        (is (= 2 (bin/alignment val)))))
    (testing "word-size 4"
      (let [val (bin/encode (byte 12) ::bin/word-size 4 ::bin/order :little)]
        (is (= 1 (bin/alignment val))))
      (let [val (bin/encode (short 12) ::bin/word-size 4 ::bin/order :little)]
        (is (= 2 (bin/alignment val))))
      (let [val (bin/encode (int 0x01234567) ::bin/word-size 4 ::bin/order :little)]
        (is (= 4 (bin/alignment val))))
      (let [val (bin/encode (long 0x0123456776543210) ::bin/word-size 4 ::bin/order :little)]
        (is (= 4 (bin/alignment val)))))
    (testing "word-size 8"
      (let [val (bin/encode (byte 12) ::bin/word-size 8 ::bin/order :little)]
        (is (= 1 (bin/alignment val))))
      (let [val (bin/encode (short 12) ::bin/word-size 8 ::bin/order :little)]
        (is (= 2 (bin/alignment val))))
      (let [val (bin/encode (int 0x01234567) ::bin/word-size 8 ::bin/order :little)]
        (is (= 4 (bin/alignment val))))
      (let [val (bin/encode (long 0x0123456776543210) ::bin/word-size 8 ::bin/order :little)]
        (is (= 8 (bin/alignment val))))))
  (testing "big endian"
    (let [val (bin/encode (byte 12) ::bin/word-size 1 ::bin/order :big)]
      (is (= [12] val))
      (is (= 1 (bin/alignment val))))
    (let [val (bin/encode (short 12) ::bin/word-size 1 ::bin/order :big)]
      (is (= [0 12] val))
      (is (= 1 (bin/alignment val))))
    (let [val (bin/encode (int 0x01234567) ::bin/word-size 1 ::bin/order :big)]
      (is (= [0x01 0x23 0x45 0x67]  val))
      (is (= 1 (bin/alignment val))))
    (let [val (bin/encode (long 0x0123456776543210) ::bin/word-size 1 ::bin/order :big)]
      (is (= [0x01 0x23 0x45 0x67 0x76 0x54 0x32 0x10] val))
      (is (= 1 (bin/alignment val))))))

(testing "sequential collection"
  (let [tseq [(byte 1) (int 0x1337) (short 12)]
        e0 [1]
        e1 [0x37 0x13 0x00 0x00]
        e2 [12 0]
        padding (repeat (byte 0))]
    (testing "unflattened"
      (testing "unaligned"
        (is (= [e0 e1 e2] (bin/encode tseq ::bin/flatten false))))
      (testing "4 byter alignmnet"
        (is (= [1 4 2] (map bin/alignment (bin/encode tseq ::bin/flatten false ::bin/word-size 4))))))
    (testing "flattened"
      (testing "unaligned"
        (is (= (concat e0 e1 e2) (bin/encode tseq))))
      (testing "4 byter alignmnet"
    (testing "force alignment"
        (let [align-meta {::bin/force-align [[1 8] [2 16]]}
              bs (bin/encode (with-meta tseq align-meta))]
          (is (= (concat e0 (take 7 padding) e1 (take 4 padding) e2) bs)
          (is (= 16 (bin/alignment bs)))))))
      )))

