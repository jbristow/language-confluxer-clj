(ns language-confluxer-clj.datafile-test
  (:require [clojure.test :refer [deftest is testing]]
            [language-confluxer-clj.datafile :refer :all]))

(deftest compress-sorted-test
  (let [data (flatten
              (map-indexed #(repeat (mod %1 6) (str %2))
                           "abcdefghijklmnopqrstuvwxyz"))
        compressed [["z" 1] ["x" 5] ["w" 4] ["v" 3] ["u" 2] ["t" 1] ["r" 5]
                    ["q" 4] ["p" 3] ["o" 2] ["n" 1] ["l" 5] ["k" 4] ["j" 3]
                    ["i" 2] ["h" 1] ["f" 5] ["e" 4] ["d" 3] ["c" 2] ["b" 1]]]
    (testing "shuffle compress"
      (dotimes [_ 10]
        (is (= compressed (compress-sorted (shuffle data))))))
    (testing "uncompress"
      (is (= (reverse data)
             (uncompress (map #(vector (format "%s%d" (first %) (second %))
                                       (first %)
                                       (str (second %)))
                              compressed)))))))

(deftest uncompress-pairmap-test
  (testing "single pairmap"
    (is (= {"aa" ["a" "a" "a" "b" "c" "c"]}
           (uncompress-pairmap "aa=a3bc2"))))
  (testing "multiple pairmaps"
    (is (= {"aa" ["a" "a" "a" "b" "c" "c"]
            "bb" ["a"]
            "cc" ["a" "a" " " " " " " " " "b"]}
           (uncompress-pairmap "aa=a3bc2,bb=a,cc=a2 4b"))))
  (testing "2 digit numbers"
    (is (= {"aa" (concat (repeat 124 "z") ["a"] (repeat 55 "j"))} (uncompress-pairmap "aa=z124aj55")))))
