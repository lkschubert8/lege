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
  ([character]
  (fn [sequence] 
    (let [current-value (first sequence)]
    (if (= current-value character)
      (build-success (rest sequence) (first sequence))
      (build-error (str "Expecting '" character "' found '" (first sequence) "'")) ;; TODO figure out error handling/threading through parsers 
      )))))
