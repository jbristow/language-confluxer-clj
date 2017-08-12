(ns language-confluxer-clj.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [language-confluxer-clj.charmap :as charmap]
            [language-confluxer-clj.datafile :as datafile]))

(deftest compress-test
  (testing "example compression"
    (is (= " a=b3a , b=ba3 ,a =b2a2,aa=b ,ab=ba 3,b =b3a2,ba=ba 2,bb=a \n b5, a5\nmin=2\nmax=4.0\n"
           (datafile/output-map (charmap/generate-map "a b ab ba aab aba baa abb bab bba")))))
  (testing "there-and-back"
    (let [[pm sp & _] (str/split
                       (datafile/output-map (charmap/generate-map "a b ab ba aab aba baa abb bab bba")) #"\n")
          m (charmap/generate-map "a b ab ba aab aba baa abb bab bba")]
      (is (= (set (:start-pairs m))
             (set (datafile/decompress-start-pairs sp))))
      (is (= (apply hash-map (mapcat (fn [[a b]] [a (reverse (sort (map str b)))]) (:pairs m)))
             (datafile/decompress-pairs pm))))))
