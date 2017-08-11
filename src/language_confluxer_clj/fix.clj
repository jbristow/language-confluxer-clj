(ns language-confluxer-clj.fix
  (:require [clojure.string :as str]))

(def vowels "aeiouyAEIOUY")
(def re-ends-with-vowel (re-pattern (format "[%s]$" vowels)))
(def re-begins-with-vowel (re-pattern (format "^[%s]" vowels)))
(def re-vowel (re-pattern (format "[%s]" vowels)))

(defn assemble-fix
  ([w]
   (str (str/upper-case (str (first w))) (str/lower-case (str/join "" (rest w)))))
  ([start end]
   (assemble-fix (str start end))))

(defn make-fixes
  "Generates n prefix/suffixes, where n is [0,# of words/10]"
  [words]
  (->> words
       (count)
       (range 0)
       (shuffle)
       (map #(let [word (nth words %)]
               (if (= 2 (inc (rand-int 2)))
                 [(.substring word 0 (min (+ 2 (rand-int 3)) (count word))) :prefix]
                 [(.substring word (max 0 (- (count word) (+ 2 (rand-int 3))))) :suffix])))
       (filter #(re-find re-vowel (first %)))
       (take (int (/ (count words) 10)))))

(defn ends-with-vowel? [^String n] (re-find re-ends-with-vowel n))

(defn begins-with-vowel? [^String n] (re-find re-begins-with-vowel n))

(defmulti fix-word (fn [chaos [fix pre] word] (if (zero? chaos) :none pre)))
(defmethod fix-word :prefix [_ [fix _] word]
  (assemble-fix fix (if (ends-with-vowel? fix)
                      (str/replace word re-begins-with-vowel "")
                      word)))

(defmethod fix-word :suffix [_ [fix _] word]
  (assemble-fix (if (begins-with-vowel? fix)
                  (str/replace word re-ends-with-vowel "")
                  word)
                fix))
(defmethod fix-word :none [_ _ word] (assemble-fix word))

(defn fix
  "Simulates suffixes and prefixes and adds them to words. As a side effect,
   words will end up with a capitalized first letter and the rest lowercase. At
   the moment this is by design."
  [words]
  (let [fixes (make-fixes words)]
    (map #(fix-word (rand-int 2) (rand-nth fixes) %) words)))
