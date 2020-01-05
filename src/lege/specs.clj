(ns lege.specs
  (:require [clojure.spec.alpha :as s]))

(s/def :lege/error string?)
(s/def :lege/result any?)
(s/def :lege/sequence sequential?)
(s/def :lege/char-sequence (s/* char?))

(s/def :lege/parser-error (s/keys :req [:lege/error]))
(s/def :lege/parser-success (s/keys :req [:lege/result :lege/sequence]))
(s/def :lege/parser-output (s/or :lege/error :lege/parser-error
                                 :success :lege/parser-success))

(s/fdef :lege/parser
  :args (s/cat :lege/sequence :lege/sequence)
  :ret :lege/parser-output)
