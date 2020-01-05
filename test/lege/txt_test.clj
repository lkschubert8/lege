(ns lege.txt-test
  (:require [clojure.test :refer :all]
            [lege.txt :as lege-txt]))

(deftest test-int-parser
  (testing "Parser for signed ints"
    (is (= ((lege-txt/int-parser) (seq "-2345"))
           {:lege/sequence () :lege/result -2345}))))

(deftest test-whitespace-parser
  (testing "Testing that whitespace can be parsed"
    (is (= ((lege-txt/whitespace-parser) (seq " \r\n\t \t"))
           {:lege/sequence ()
            :lege/result [\space \return \newline \tab \space \tab]}))))

(deftest test-string-parser
  (testing "Empty string"
    (is (= ((lege-txt/string-parser) (seq "\"\""))
           {:lege/sequence () :lege/result ""})))
  (testing "Some string"
    (is (= ((lege-txt/string-parser) (seq "\"blahblahblah\""))
           {:lege/sequence () :lege/result "blahblahblah"})))
  (testing "Escaped characters"
    (is (= ((lege-txt/string-parser) (seq "\"thing\\t\""))))))