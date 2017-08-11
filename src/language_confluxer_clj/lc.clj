(ns language-confluxer-clj.lc
  (:require [clojure.string :as str]))

(defn new-word [{:keys [pairmap minlen maxlen]} seed]
  (let [word
        (->> seed
             (iterate (fn [w]
                        (let [letter (rand-nth (get pairmap (str/join (take-last 2 w))))]
                          (str w (if (nil? letter) " " letter)))))
             (drop-while #(not (and (re-matches #".*\s$" %)
                                    (> (count %) minlen))))
             (first))]
    (.substring word 1 (min (dec (count word)) (inc maxlen)))))

(defn generate-words [n {:keys [start-pairs] :as m}]
  (repeatedly n #(new-word m (rand-nth start-pairs))))
