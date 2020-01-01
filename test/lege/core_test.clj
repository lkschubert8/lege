(ns lege.core-test
  (:require [clojure.test :refer :all]
            [lege.core :as lege]))


(deftest test-basic-char-parser
  (testing "character parser"
    (is (= ((lege/parse-char \x) [\x]) [[] \x])))
  (testing "character parser expected failure"
    (is (= ((lege/parse-char \y) [\x]) [[] {:error :error}]))))

(deftest test-any-combinator
  (testing "Checking if the any combinator works as expected"
    (is
     (= [[] \y]
        ((lege/parse-any
         [(lege/parse-char \x)
         (lege/parse-char \y)]) [\y]))))
  #_(testing "Checking the any combinator fails when expected")
  #_(testing "Checking the any combinator operates with the expected precedence"))
