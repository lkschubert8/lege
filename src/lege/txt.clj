(ns lege.txt
  (:require
   [lege.core :as lege]
   [lege.utils :as utils]
   [clojure.tools.logging :as log]
   [orchestra.core :refer [defn-spec]]
   [orchestra.spec.test :as st]))

;;Everything will be operating on a sequence of chars

(defn-spec int-parser :lege/parser
  "Parses a signed integer from a sequence of characters"
  []
  (lege/map-parser
    #(if (-> % first (= \-))
       (-> % second (* -1))
       (second %))
    (lege/and-then
     (lege/parse-opt (lege/parse-val \-))
     (lege/map-parser
      #(->> %
            (apply str)
            utils/string->int)
      (lege/parse-many-1 (lege/any-of (lege/char-range \0 \9)))))))

(defn-spec whitespace-parser :lege/parser
  "Parses normally used whitespace i.e. :\n \r \t ' '"
  []
  (lege/parse-many-1 (lege/any-of [\newline \return \tab \space ])))



(defn-spec string-parser :lege/parser; TODO Decide if I should support escape characters
  "Parses a traditional double quote surrounded string, only supports ASCII for
   now and doesn't support escape characters at this time"
  []
  (lege/parse-between
    (lege/parse-val \")
    (lege/map-parser
     #(apply str %)
     (lege/parse-many (lege/any-of (map char (range 35 127)))))
    (lege/parse-val \")))
(st/instrument)