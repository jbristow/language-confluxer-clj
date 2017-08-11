(ns language-confluxer-clj.charmap-test
  (:require [clojure.test :refer [deftest is testing]]
            [language-confluxer-clj.charmap :refer :all]))

(deftest variance-testing
  (testing "empty list"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Cannot find the variance of an empty list\."
                          (variance 1 []))))
  (testing "no variance"
    (is (= 0 (variance 2 [2 2 2 2])))
    (is (= 0 (variance 10 [10]))))

  (testing "example tests"
    (is (= 1 (variance 2 [1 3 1 3])))
    (is (= 2 (variance 3 [1 2 3 4 5])))))

(deftest avg-min-max-tests
  (testing "one word"
    (is (= [10.0 10.0] (avg-min-max-wordlen  ["automobile"]))))
  (testing "example tests"
    (is (= [2 (+ 3 (Math/sqrt 8))]
           (avg-min-max-wordlen ["a" "bb" "ccc" "dddd" "eeeee"])))
    (is (= [2 (+ 4 (Math/sqrt (/ 40 9)))]
           (avg-min-max-wordlen ["The" "quick" "brown" "fox" "jumped" "over"
                                 "the" "lazy" "dog"])))))
