(ns lege.txt
  (:require
   [lege.core :as lege]
   [lege.utils :as utils]
   [clojure.tools.logging :as log]))

;;Everything will be operating on a sequence of chars
(defn parse-int
  [sequence]
  ((lege/map-parser
    #(if (-> % first (= \-))
       (-> % second (* -1))
       (second %))
    (lege/and-then
     (lege/parse-opt (lege/parse-val \-))
     (lege/map-parser 
      #(->> %
         (apply str)
         utils/string->int)
      (lege/parse-many-1 (lege/any-of (lege/char-range \0 \9)))))) sequence))

(defn parse-string ; TODO support escape characters
  "Parses a traditional double quote surrounded string, only supports ASCII for
   now and doesn't support escape characters at this time"
  [sequence]
  ((lege/parse-between 
   (lege/parse-val \")
   (lege/map-parser 
    #(apply str %) 
    (lege/parse-many (lege/any-of (map char (range 35 127)))))
   (lege/parse-val \")) sequence))
