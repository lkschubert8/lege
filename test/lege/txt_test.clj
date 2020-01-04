(ns lege.txt-test
  (:require [clojure.test :refer :all]
            [lege.txt :as lege-txt]))

(deftest test-int-parser
  (testing "Parser for signed ints"
    (is (= (lege-txt/parse-int (seq "-2345")) 
           {:sequence [] :result -2345}))))