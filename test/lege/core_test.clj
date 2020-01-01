(ns lege.core-test
  (:require [clojure.test :refer :all]
            [lege.core :as lege]))


(deftest test-basic-char-parser
  (testing "character parser"
    (is (= ((lege/parse-char \x) [\x]) {:sequence [] :result \x})))
  (testing "character parser expected failure"
    (is (= ((lege/parse-char \y) [\x])  {:error "Expecting 'y' found 'x'"}))))


