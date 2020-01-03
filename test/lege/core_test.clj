(ns lege.core-test
  (:require [clojure.test :refer :all]
            [lege.core :as lege]))


(deftest test-basic-char-parser
  (testing "character parser"
    (is (= ((lege/parse-val \x) [\x]) {:sequence [] :result \x})))
  (testing "character parser expected failure"
    (is (= ((lege/parse-val \y) [\x])  {:error "Expecting 'y' found 'x'"}))))

(deftest test-and-then-combinator
  (testing "And then combinator"
    (is (=
         ((lege/and-then
           (lege/parse-val \x)
           (lege/parse-val \y))
          [\x \y])
         {:sequence []
          :result [\x \y]})))
  (testing "And then failure"
    (is (=
         ((lege/and-then
           (lege/parse-val \x)
           (lege/parse-val \y))
          [\x \c])
         {:error "Expecting 'y' found 'c'"}))))

(deftest test-or-else-combinator
  (testing "Or else combinator"
    (is (=
         ((lege/or-else
           (lege/parse-val \x)
           (lege/parse-val \y))
          [\y])
         {:sequence []
          :result \y}))))

(deftest any-of-combinator
  (testing "Any of combinator"
    (is (=
         ((lege/any-of [\x \y]) [\y])
         {:sequence []
          :result \y}))))

(deftest map-parser
  (testing "implementing 3 char alpha parser"
    (let [lower-case-parser (lege/any-of (lege/char-range \a \z))]
      (is (=
           ((lege/map-parser #(apply str (flatten %)) 
                            (lege/and-then lower-case-parser (lege/and-then lower-case-parser lower-case-parser)))

                            [\t \h \e])
           {:sequence []
            :result "the"})))))


(deftest seq-parser
  (testing "testing a an ABC parser with sequence"
    (is (= ((lege/sequence-parser 
             [(lege/parse-val \A)
              (lege/parse-val \B)
              (lege/parse-val \C)
              ])[\A \B \C \D])
           {:sequence [\D]
            :result '(\A \B \C)}))))

(deftest string-parser
  (testing "Testing string parsing"
    (is (= ((lege/parse-string "ABC") [\A \B \C \D])
           {:sequence [\D]
            :result "ABC"}))))

(deftest many-parser 
  (testing "Testing many A's"
    (is (= ((lege/parse-many (lege/parse-val \A)) [\A \A \A])
           {:sequence []
            :result [\A \A \A]}))))
