(ns lege.core)

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

(defn parse-char 
  [character]
  (fn [sequence] 
    (let [current-value (first sequence)]
    (if (= current-value character)
      (build-success (rest sequence) (first sequence))
      (build-error (str "Expecting '" character "' found '" (first sequence) "'")) ;; TODO figure out error handling/threading through parsers 
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
