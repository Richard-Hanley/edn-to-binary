(ns edn-to-binary.core-test
  (:require [clojure.test :refer :all]
            [edn-to-binary.core :refer :all]))

(deftest simple-serialization
  (let [expected-bytes (byte-array (map unchecked-byte (range 10)))]
    (testing "single binary collection"
      (is (= expected-bytes (serialize (range 10)))))
    (testing "nested binary collections"
      (is (= expected-bytes (serialize [[0] [1 2 3] [4 5] [6 7 8 9]]))))
          ))


; (deftest a-test
;   (testing "FIXME, I fail."
;     (is (= 0 1))))
