(ns language-confluxer-clj.datafile
  (:require [clojure.string :as str]))

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
                                               (:pairs m)))))
                  (str/join "," (map #(str/join "" %) (compress-sorted (:start-pairs m))))
                  (str "min=" (:min-len m))
                  (str "max=" (:max-len m))
                  ""]))

(defn write-map-to-file [filename m]
  (spit filename (output-map m)))

(defn decompress [b]
  (mapcat (fn [[_ x y]]
            (if (empty? y) [x] (repeat (Integer. y) x))) b))

(defn decompress-pairs [input]
  (reduce (fn [m [_ a b]]
            (if (nil? a)
              m
              (assoc m a (decompress (re-seq #"(\D)(\d*)" b)))))
          {}
          (re-seq #"([^,].)=((?:(?:.\d+)|(?:[^\d=,]))+)" input)))

(defn decompress-start-pairs [input]
  (decompress (re-seq #"( [^,\d])(\d*),?+" input)))

(defn read-datafile [filename]
  (let [[c-pairs c-start-pairs c-min c-max] (line-seq (java.io.BufferedReader. (java.io.FileReader. filename)))]
    {:pairs (decompress-pairs c-pairs)
     :start-pairs (decompress-start-pairs c-start-pairs)
     :min-len (Double. (second (str/split c-min #"=")))
     :max-len (Double. (second (str/split c-max #"=")))}))
