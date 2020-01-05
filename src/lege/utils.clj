(ns lege.utils
  (:require
   [orchestra.core :refer [defn-spec]] 
   [orchestra.spec.test :as st]))

(defn-spec string->int int?
  "Parse a string as an integer or throw an Exception" ; TODO decide if this should handle not parsing 
  [string string?]
  (Integer/parseInt string))

(st/instrument)