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

; (deftest simple-serialization
;   (let [expected-bytes (byte-array (map unchecked-byte (range 10)))]
;     (testing "single binary collection"
;       (is (= expected-bytes (serialize (range 10)))))
;     (testing "nested binary collections"
;       (is (= expected-bytes (serialize [[0] [1 2 3] [4 5] [6 7 8 9]]))))
;           ))


; (deftest a-test
;   (testing "FIXME, I fail."
;     (is (= 0 1))))
