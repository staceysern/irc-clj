(ns irc.core)

(def verbose true)

(defn log [& msg]
  (if verbose
    (apply println msg)))
