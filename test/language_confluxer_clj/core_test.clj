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
    (is (= [2 (+ 3 (Math/sqrt 8))]
           (avg-min-max-wordlen ["a" "bb" "ccc" "dddd" "eeeee"])))
    (is (= [2 (+ 4 (Math/sqrt (/ 40 9)))]
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
            :minlen 2
            :maxlen 4.0}
           (generate-map "a b ab ba aab aba baa abb bab bba")))))

(deftest compress-test
  (testing "example compression"
    (is (= " a=b3a , b=ba3 ,a =b2a2,aa=b ,ab=ba 3,b =b3a2,ba=ba 2,bb=a \n b5, a5\nmin=2\nmax=4.0\n"
           (output-map (generate-map "a b ab ba aab aba baa abb bab bba")))))
  (testing "there-and-back"
    (let [[pm sp & _] (clojure.string/split
                       (output-map (generate-map "a b ab ba aab aba baa abb bab bba")) #"\n")
          m (generate-map "a b ab ba aab aba baa abb bab bba")]
      (is (= (set (:start-pairs m))
             (set (uncompress-start-pairs sp))))
      (is (= (apply hash-map (mapcat (fn [[a b]] [a (reverse (sort (map str b)))]) (:pairmap m)))
             (uncompress-pairmap pm))))))

(deftest assemble-fix-test
  (testing "basic examples"
    (is (= "Helloworld" (assemble-fix "Hello" "World")))
    (is (= "Helloworld" (assemble-fix "hello" "WORLD")))))
