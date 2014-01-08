(ns irc.iopair
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols]))

;; An IOPair associates two unidirectional core.async channels and
;; presents a bidirectional interface.  IOPair implements the same
;; interfaces that are used to read, write and close core.async channels.

(defrecord IOPair [in-chan out-chan]
  clojure.core.async.impl.protocols.ReadPort
  (take! [user fn1-handler]
    (clojure.core.async.impl.protocols/take! in-chan fn1-handler))

  clojure.core.async.impl.protocols.WritePort
  (put! [user val fn0-handler]
    (clojure.core.async.impl.protocols/put! out-chan val fn0-handler))

  clojure.core.async.impl.protocols.Channel
  (close! [user]
    (async/close! in-chan)
    (async/close! out-chan)))

(defn in-chan [io-pair]
  (:in-chan io-pair))

(defn out-chan [io-pair]
  (:out-chan io-pair))
