(ns language-confluxer-clj.lc
  (:require [clojure.string :as str]))

(defn new-word [{:keys [pairs min-len max-len]} seed]
  (let [word
        (->> seed
             (iterate (fn [w]
                        (let [letter (rand-nth (get pairs (str/join (take-last 2 w))))]
                          (str w (if (nil? letter) " " letter)))))
             (drop-while #(not (and (re-matches #".*\s$" %)
                                    (> (count %) min-len))))
             (first))]
    (.substring word 1 (min (dec (count word)) (inc max-len)))))

(defn generate-words [n {:keys [start-pairs] :as m}]
  (repeatedly n #(new-word m (rand-nth start-pairs))))
