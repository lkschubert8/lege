(ns lege.txt-test
  (:require [clojure.test :refer :all]
            [lege.txt :as lege-txt]))

(deftest test-int-parser
  (testing "Parser for signed ints"
    (is (= (lege-txt/parse-int (seq "-2345")) 
           {:lege/sequence () :lege/result -2345}))))

(deftest test-string-parser
  (testing "Empty string"
    (is (= (lege-txt/parse-string (seq "\"\""))
           {:lege/sequence () :lege/result ""})))
  (testing "Some string"
    (is (= (lege-txt/parse-string (seq "\"blahblahblah\""))
           {:lege/sequence () :lege/result "blahblahblah"}))))

