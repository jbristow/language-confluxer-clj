(ns language-confluxer-clj.core
  (:require [clojure.tools.cli :as cli]
            [language-confluxer-clj.charmap :as charmap]
            [language-confluxer-clj.datafile :as datafile]
            [language-confluxer-clj.lc :as lc])
  (:gen-class))

(def cli-options
  [["-n" "--number NUMBER" "Number of words to generate."
    :default 1
    :parse-fn #(Integer. %)]
   ["-f" "--file FILE" "Filename of sample wordlist."]
   ["-d" "--data FILE" "Filename of compressed datafile."]
   ["-o" "--output FILE" "Filename to write compressed datafile to."]
   ["-h" "--help"]])

(defn read-file [filename]
  (line-seq (java.io.BufferedReader. (java.io.FileReader. filename))))

(defn command-lc [{:keys [file data number]}]
  (cond (and file data)
        (println "You can only load from a sample file or a data file, not both. Choose either -f or -d.")

        file
        (doseq [x (lc/generate-words number
                                     (charmap/populate-map-from-sample (read-file file)))]
          (println x))

        data
        (doseq [x (lc/generate-words number (datafile/read-datafile data))]
          (println x))

        :else
        (println "You must define an input file. -f or -d is required.")))

(defn command-compress [{:keys [file output]}]
  (cond
    (and file output)
    (do
      (datafile/write-map-to-file output (charmap/populate-map-from-sample (read-file file)))
      (println (format "Wrote sample file to '%s'." output)))

    file
    (println (datafile/output-map (charmap/populate-map-from-sample (read-file file))))

    :else
    (println "No sample wordlist supplied. -f is required.")))

(defn -main
  "main entry point"
  [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      (println summary)

      (= (first arguments) "lc")
      (command-lc options)

      (= (first arguments) "compress")
      (command-compress options))))
