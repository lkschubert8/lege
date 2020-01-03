(ns lege.core
  (:require [clojure.tools.logging :as log]))

;;For starters I want to be able to parse a single specified char, multiple chars
;;wildcard characters, sequences and between

(defn- build-success
  [sequence result]
  {:sequence sequence
   :result result})

(defn- build-error
  [message]
  {:error message})

(defn- is-error
  [parser-result]
  (contains? parser-result :error))


(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn parse-val
  [val]
  (fn [sequence]
    (let [current-value (first sequence)]
      (if (= current-value val)
        (build-success (rest sequence) (first sequence))
        (build-error (str "Expecting '" val "' found '" (first sequence) "'")) ;; TODO figure out error handling/threading through parsers 
        ))))

(defn and-then
  [parser-a parser-b]
  (fn [sequence]
    (let [result-a (parser-a sequence)]
      (if (is-error result-a)
        result-a
        (let [result-b (-> result-a :sequence parser-b)]
          (if (is-error result-b)
            result-b
            (update result-b :result (fn [x] [(:result result-a) x]))))))))

(defn or-else
  [parser-a parser-b]
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
        (update result :result map-fn)))))

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

(defn parse-string
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
        (let [{vals :result
               rest :sequence} ((parse-zero-or-more parser) (:sequence result))]
          (build-success rest (cons (:result result) vals) ))))))

(defn parse-many
  [parser]
  (parse-zero-or-more parser))
