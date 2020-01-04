(ns lege.core
  (:require
   [lege.specs :as specs]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [orchestra.core :refer [defn-spec]]
   [orchestra.spec.test :as st]))

;;For starters I want to be able to parse a single specified char, multiple chars
;;wildcard characters, sequences and between

(defn-spec build-success :lege/parser-success
  [sequence :lege/sequence result :lege/result]
  {:lege/sequence sequence
   :lege/result result})

(defn-spec build-error :lege/parser-error
  [message string?]
  {:lege/error message})

(defn-spec is-error boolean?
  [parser-result :lege/parser-output]
  (contains? parser-result :lege/error))


(defn-spec char-range (s/* char?)
  [start char? end char?]
  (map char (range (int start) (inc (int end)))))

(defn-spec parse-val :lege/parser
  [val any?]
  (fn [sequence]
    (let [current-value (first sequence)]
      (if (= current-value val)
        (build-success (rest sequence) (first sequence))
        (build-error (str "Expecting '" val "' found '" (first sequence) "'")) ;; TODO figure out error handling/threading through parsers 
        ))))

(defn-spec and-then :lege/parser
  [parser-a :lege/parser parser-b :lege/parser]
  (fn [sequence]
    (let [result-a (parser-a sequence)]
      (if (is-error result-a)
        result-a
        (let [result-b (-> result-a :lege/sequence parser-b)]
          (if (is-error result-b)
            result-b
            (update result-b :lege/result (fn [x] [(:lege/result result-a) x]))))))))

(defn-spec or-else :lege/parser
  [parser-a :lege/parser parser-b :lege/parser]
  (fn [sequence]
    (let [result-a (parser-a sequence)]
      (if (is-error result-a)
        (parser-b sequence)
        result-a))))

(defn choice
  [parsers]
  (fn [sequence]
    ((reduce or-else parsers) sequence)))

(defn any-of
  [vals]
  (choice (map parse-val vals)))

(defn map-parser
  [map-fn parser]
  (fn [sequence]
    (let [result (parser sequence)]
      (if (is-error result)
        result
        (update result :lege/result map-fn)))))

(defn return-parser
  [input]
  (fn [sequence] (build-success sequence input)))

(defn apply-parser ; TODO spend more time grokking this
  [fp xp]
  (map-parser #((first %) (second %)) (and-then fp xp)))



(defn- lift2
  [f]
  (fn [xp yp]
    (apply-parser (apply-parser (return-parser f) xp) yp)))

(defn sequence-parser
  [parsers]
  (let [cons-parser (lift2 #(partial cons %))]
    (if (= 0 (count parsers))
      (return-parser [])
      (cons-parser (first parsers) (sequence-parser (rest parsers))))))

(defn parse-string ;; TODO This still assume seq of chars 
  [string]
  (fn
    [sequence]
    ((map-parser #(apply str %) (sequence-parser (map parse-val (seq string)))) sequence)))

(defn- parse-zero-or-more
  [parser]
  (fn
    [sequence]
    (let [result (parser sequence)]
      (if (is-error result)
        (build-success sequence [])
        (let [{vals :lege/result
               rest :lege/sequence} ((parse-zero-or-more parser) (:lege/sequence result))]
          (build-success rest (cons (:lege/result result) vals)))))))

(defn parse-many
  [parser]
  (parse-zero-or-more parser))

(defn parse-many-1
  [parser]
  (fn
    [sequence]
    (let [result (parser sequence)]
      (if (is-error result)
        result
        (let [{vals :lege/result
               rest :lege/sequence} ((parse-zero-or-more parser) (:lege/sequence result))]
          (build-success rest (cons (:lege/result result) vals)))))))

(defn parse-opt
  [parser]
  (or-else parser
           (return-parser nil)))

(defn and-then-ignore-left
  [parser-a parser-b]
  (map-parser second (and-then parser-a parser-b)))

(defn and-then-ignore-right
  [parser-a parser-b]
  (map-parser first (and-then parser-a parser-b)))

(defn parse-between
  [parser-a parser-b parser-c]
  (and-then-ignore-right (and-then-ignore-left parser-a parser-b) parser-c))

(defn parse-sep-by-1
  [parser separator-parser]
  (let [separator-then-parser (and-then-ignore-left separator-parser parser)]
    (map-parser #(cons (first %) (second %)) (and-then parser (parse-many separator-then-parser)))))

(defn parse-sep-by
  [parser separator-parser]
  (or-else (parse-sep-by-1 parser separator-parser) (return-parser [])))

(defn parse-bind
  [f parser]
  (fn [sequence]
    (let [result-a (parser sequence)]
      (if (is-error result-a)
        result-a
        ((-> result-a :lege/result f) sequence)))))

(st/instrument)