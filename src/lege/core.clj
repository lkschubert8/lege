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
        (build-error (str "Expecting '" val "' found '" (first sequence) "'")))))) ;; TODO figure out error handling/threading through parsers 
        

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

(defn-spec choice :lege/parser
  [parsers (s/* :lege/parser)]
  (fn [sequence]
    ((reduce or-else parsers) sequence)))

(defn-spec any-of :lege/parser 
  [vals :lege/sequence]
  (choice (map parse-val vals)))

(defn-spec map-parser :lege/parser
  [map-fn fn? parser :lege/parser]
  (fn [sequence]
    (let [result (parser sequence)]
      (if (is-error result)
        result
        (update result :lege/result map-fn)))))

(defn-spec return-parser :lege/parser
  [input any?]
  (fn [sequence] (build-success sequence input)))

(defn-spec apply-parser :lege/parser
  [fp fn? xp :lege/parser]
  (map-parser #((first %) (second %)) (and-then fp xp)))



(defn- lift2 
  [f]
  (fn [xp yp]
    (apply-parser (apply-parser (return-parser f) xp) yp)))

(defn-spec sequence-parser :lege/parser
  [parsers (s/* :lege/parser)]
  (let [cons-parser (lift2 #(partial cons %))]
    (if (= 0 (count parsers))
      (return-parser [])
      (cons-parser (first parsers) (sequence-parser (rest parsers))))))

(defn-spec parse-string :lege/parser ;; TODO This still assume seq of chars 
  [string string?]
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

(defn-spec parse-many :lege/parser
  [parser :lege/parser]
  (parse-zero-or-more parser))

(defn-spec parse-many-1 :lege/parser
  [parser :lege/parser]
  (fn
    [sequence]
    (let [result (parser sequence)]
      (if (is-error result)
        result
        (let [{vals :lege/result
               rest :lege/sequence} ((parse-zero-or-more parser) (:lege/sequence result))]
          (build-success rest (cons (:lege/result result) vals)))))))

(defn-spec parse-opt :lege/parser
  [parser :lege/parser]
  (or-else parser
           (return-parser nil)))

(defn-spec and-then-ignore-left :lege/parser
  [parser-a :lege/parser parser-b :lege/parser]
  (map-parser second (and-then parser-a parser-b)))

(defn-spec and-then-ignore-right :lege/parser
  [parser-a :lege/parser parser-b :lege/parser]
  (map-parser first (and-then parser-a parser-b)))

(defn-spec parse-between :lege/parser
  [parser-a :lege/parser parser-b :lege/parser parser-c :lege/parser]
  (and-then-ignore-right (and-then-ignore-left parser-a parser-b) parser-c))

(defn-spec parse-sep-by-1 :lege/parser
  [parser :lege/parser separator-parser :lege/parser]
  (let [separator-then-parser (and-then-ignore-left separator-parser parser)]
    (map-parser #(cons (first %) (second %)) (and-then parser (parse-many separator-then-parser)))))

(defn-spec parse-sep-by :lege/parser
  [parser :lege/parser separator-parser :lege/parser]
  (or-else (parse-sep-by-1 parser separator-parser) (return-parser [])))

(defn parse-bind
  [f parser]
  (fn [sequence]
    (let [result-a (parser sequence)]
      (if (is-error result-a)
        result-a
        ((-> result-a :lege/result f) sequence)))))

(st/instrument)