(ns lege.specs
  (:require [clojure.spec.alpha :as s]))

(s/def :lege/error string?)
(s/def :lege/result any?)
(s/def :lege/sequence seq?)

(s/def :lege/parser-error (s/keys :req [:lege/error]))
(s/def :lege/parser-success (s/keys :req [:lege/result :lege/sequence]))
(s/def :lege/parser-output (s/or :lege/parser-error
                                 :lege/parser-success))

(s/fdef :lege/parser
  :args :lege/sequence
  :ret :lege/parser-output)