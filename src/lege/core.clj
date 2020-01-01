(ns lege.core
  (:require 
   [clojure.spec.alpha :as spec]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;;For starters I want to be able to parse a single specified char, multiple chars
;;wildcard characters, sequences and between

(defn parse-char 
  ([character]
  (fn [sequence] 
    (let [current-value (first sequence)]
    (if (= current-value character)
      [(rest sequence) character]
      [(rest sequence) {:error :error}] ;; TODO figure out error handling/threading through parsers 
      ))
    )))

(defn parse-any
  [parsers]
  (fn [sequence] (throw (Exception. "Not implemented"))))

(defn parse-many
  [parser]
  (fn [sequence] (throw (Exception. "Not implemented."))))
