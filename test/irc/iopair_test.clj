(ns irc.iopair-test
  (:require [midje.sweet :refer :all]
            [irc.iopair :as iopair :refer [->IOPair]]))

(def pair (->IOPair :in :out))

(facts "in-chan"
  (iopair/in-chan pair) => :in
  )

(facts "out-chan"
  (iopair/out-chan pair) => :out
  )
