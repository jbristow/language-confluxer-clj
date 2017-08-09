(ns language-confluxer-clj.core-test
  (:require [clojure.test :refer :all]
            [language-confluxer-clj.core :refer :all]))

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
    (is (= [1 (+ 3 (Math/sqrt 8))]
           (avg-min-max-wordlen ["a" "bb" "ccc" "dddd" "eeeee"])))
    (is (= [(- 4 (Math/sqrt (/ 40 9))) (+ 4 (Math/sqrt (/ 40 9)))]
           (avg-min-max-wordlen ["The" "quick" "brown" "fox" "jumped" "over"
                                 "the" "lazy" "dog"])))))

(deftest generate-map-test
  (testing "simple"
    (is (= {:pairmap {"wo" [\r] " h" [\e] "o " [\w] "rl" [\d] "ll" [\o]
                      "el" [\l] "lo" [\space] "or" [\l] " w" [\o] "he" [\l]}
            :start-pairs (list " w" " h")
            :minlen 5.0
            :maxlen 5.0}
           (generate-map "hello world"))))
  (testing "more complicated"
    (is (= {:pairmap {" a" [\space \b \a \b \b] "a " [\b \a \b \a]
                      " b" [\space \a \a \a \b] "b " [\a \b \a \b \b]
                      "ab" [\space \space \a \b \space]
                      "ba" [\space \space \a \b] "aa" [\b \space]
                      "bb" [\space \a]}
            :start-pairs (list " b" " b" " a" " b" " a" " a" " b" " a" " b" " a")
            :minlen 1
            :maxlen 4.0}
           (generate-map "a b ab ba aab aba baa abb bab bba")))))

(deftest assemble-fix-test
  (testing "basic examples"
    (is (= "Helloworld" (assemble-fix "Hello" "World")))
    (is (= "Helloworld" (assemble-fix "hello" "WORLD")))))
