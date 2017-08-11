(ns language-confluxer-clj.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "main entry point"
  [& args]
;; TODO: Add cli functionality.
  (println "Not implemented yet."))

(defn read-file [filename]
  (let [lines (line-seq (java.io.BufferedReader. (java.io.FileReader. filename)))]
    (str/join " "
              (->> lines
                   (map #(-> %
                             (str/replace #"#.*" "")
                             (str/replace #"[\W\d&&[^\s'-]]" " ")
                             (str/replace #"\s+" " ")
                             (str/replace #"(( ')|(' )|(^')|('$))" "")
                             (str/replace #"((^\s*)|(\s*$))" "")))
                   (remove empty?)))))

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
        pairmap (reduce (fn [m [a b c]]
                          (assoc m (str a b) (conj (get m (str a b) []) c)
                                 :start-pair (if (= a \space)
                                               (conj (get m :start-pair '()) (str a b))
                                               (:start-pair m)))) {}
                        (partition 3 1 (str " " input)))
        [minlen maxlen] (avg-min-max-wordlen words)]

    (hash-map :pairmap (dissoc pairmap :start-pair)
              :minlen minlen
              :maxlen maxlen
              :start-pairs (:start-pair pairmap))))
(defn compress-sorted [b]
  (reduce (fn [a b]
            (cond (empty? a)
                  (conj a [b 1])

                  (= (ffirst a) b)
                  (conj (rest a) [b (inc (second (first a)))])

                  :else
                  (conj a [b 1]))) '() (sort b)))

(defn output-map [m]
  (str/join "\n" [(str/join ","
                            (map #(str/join "=" %)
                                 (sort-by first
                                          (map (fn [[a b]]
                                                 [a (str/join "" (map (fn [[a b]] (if (= b 1) a (str a b)))
                                                                      (compress-sorted b)))])
                                               (:pairmap m)))))
                  (str/join "," (map #(str/join "" %) (compress-sorted (:start-pairs m))))
                  (str "min=" (:minlen m))
                  (str "max=" (:maxlen m))
                  ""]))

(defn uncompress [b]
  (mapcat (fn [[_ x y]]
            (if (empty? y) [x] (repeat (Integer. y) x))) b))

(defn uncompress-pairmap [input]
  (reduce (fn [m [_ a b]]
            (if (nil? a)
              m
              (assoc m a (uncompress (re-seq #"(\D)(\d*)" b)))))
          {}
          (re-seq #"([^,].)=((?:(?:.\d+)|(?:[^\d=,]))+)" input)))

(defn uncompress-start-pairs [input]
  (uncompress (re-seq #"( [^,\d])(\d*),?+" input)))

(defn read-compressed [filename]
  (let [[c-pairmap c-start-pairs c-min c-max] (line-seq (java.io.BufferedReader. (java.io.FileReader. filename)))]
    {:pairmap (uncompress-pairmap c-pairmap)
     :start-pairs (uncompress-start-pairs c-start-pairs)
     :minlen (Double. (second (str/split c-min #"=")))
     :maxlen (Double. (second (str/split c-max #"=")))}))

(defn new-word [{:keys [pairmap minlen maxlen] :as data} seed]
  (let [pair (apply str (take-last 2 seed))
        pairmatch (get pairmap pair)
        letter (rand-nth pairmatch)]
    (cond
      (or (and (re-matches #".*\s$" seed)
               (> (dec (count seed)) minlen))
          (nil? letter))
      (.substring seed 0 (min (dec (count seed)) (inc maxlen)))

      (re-matches #"^\s.*" seed)
      (new-word data (str (str/join "" (rest seed)) letter))

      :else
      (new-word data (str seed letter)))))

(defn generate-words [n {:keys [start-pairs] :as m}]
  (repeatedly n #(new-word m (rand-nth start-pairs))))

(defn assemble-fix [start end]
  (let [w (str start (str/lower-case end))]
    (str (str/upper-case (str (first w))) (str/join "" (rest w)))))

(defn make-fixes [words]
  (let [limit (int (/ (count words) 10))]
    (loop [out []]
      (let [how-long (+ 2 (rand-int 3))
            word (str/join "" (rand-nth words))
            fix (if (= 2 (inc (rand-int 2)))
                  [(.substring word 0 (min how-long (count word))) 1]
                  [(.substring word (max 0 (- (count word) how-long))) 0])]
        (cond
          (>= (count out) limit)
          (take limit out)

          (re-find #"[aeiouy]" (first fix))
          (recur (conj out fix))

          :else
          (recur out))))))

(def vowel-ending #"[aeiouyAEIOUY]$")
(def vowel-begin #"^[aeiouyAEIOUY]")
(defn ends-with-vowel? [^String n] (re-find vowel-ending n))
(defn begins-with-vowel? [^String n] (re-find vowel-begin n))

(defn fix [words]
  (let [fixes (make-fixes words)]
    (map (fn [word]
           (let [[fix pre] (rand-nth fixes)
                 chaos (rand-int 2)]
             (cond (zero? chaos)
                   word

                   (and (pos? pre) (ends-with-vowel? fix))
                   (assemble-fix fix (str/replace word vowel-begin ""))

                   (pos? pre)
                   (assemble-fix fix word)

                   (begins-with-vowel? fix)
                   (assemble-fix (str/replace word vowel-ending "") fix)
                   :else
                   (assemble-fix word fix))))
         words)))
