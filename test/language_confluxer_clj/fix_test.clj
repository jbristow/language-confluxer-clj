(ns language-confluxer-clj.fix-test
  (:require [clojure.test :refer [deftest is testing]]
            [language-confluxer-clj.fix :refer :all]))

(deftest assemble-fix-test
  (testing "basic examples"
    (is (= "Helloworld" (assemble-fix "Hello" "World")))
    (is (= "Helloworld" (assemble-fix "hello" "WORLD")))))
