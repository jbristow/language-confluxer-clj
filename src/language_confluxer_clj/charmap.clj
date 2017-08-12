(ns language-confluxer-clj.charmap
  (:require [clojure.string :as str]))

(defn clean-input [lines]
  (str/join " "
            (->> lines
                 (map #(-> %
                           (str/replace #"#.*" "")
                           (str/replace #"[\W\d&&[^\s'-]]" " ")
                           (str/replace #"(( ')|(' )|(^')|('$))" " ")
                           (str/replace #"\s+" " ")
                           (str/replace #"((^\s*)|(\s*$))" "")))
                 (remove empty?))))

(defn variance [avg lst]
  (when (empty? lst) (throw (ex-info "Cannot find the variance of an empty list." {:causes #{:empty-list}})))
  (/ (reduce #(+ %1 (* (- %2 avg) (- %2 avg))) 0 lst) (count lst)))

(defn avg-min-max-wordlen [words]
  (let [word-count (count words)
        word-lens (map count words)
        avg-word-len (reduce #(+ %1 (/ %2 word-count)) 0 word-lens)
        word-len-stdev (Math/sqrt (variance avg-word-len word-lens))]
    [(max 2 (- avg-word-len (* 2 word-len-stdev)))
     (+ avg-word-len (* 2 word-len-stdev))]))

(defn generate-map [input]
  (let [words (str/split input #" ")
        pairs (reduce (fn [m [a b c]]
                        (assoc m (str a b) (conj (get m (str a b) []) c)
                               :start-pair (if (= a \space)
                                             (conj (get m :start-pair '()) (str a b))
                                             (:start-pair m)))) {}
                      (partition 3 1 (str " " input)))
        [min-len max-len] (avg-min-max-wordlen words)]

    (hash-map :pairs (dissoc pairs :start-pair)
              :min-len min-len
              :max-len max-len
              :start-pairs (:start-pair pairs))))

(defn populate-map-from-sample [input]
  (generate-map (clean-input input)))
