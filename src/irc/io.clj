(ns irc.io
  (:require [irc.core :refer [log]]
            [clojure.core.async :as async :refer [go]]
            [clojure.core.match :refer [match]]
            [clojure.core.async.impl.protocols]))

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

(defn create-io-process!
  "Create an asynchronous process that monitors a control channel
  and a set of incoming channels and notifies another process,
  via a dispatch channel, about events on those channels.

  Each incoming channel along with a paired outgoing channel
  corresponds to a user who is assigned a unique user id.  Users
  are added and removed via the control channel.

  For the irc server, the dispatch channel is the source of
  messages for the server.

  Params: dispatch-chan is the channel to which the io process
          reports events
  Return: the control channel for the io process"
  [dispatch-chan]
  (let [ctrl-chan (async/chan)]
    (go
     (loop [next-uid 1
            ins-to-uids {}]
       (try
         (do
           (match (alts! (conj (keys ins-to-uids) ctrl-chan))
             [nil ctrl-chan]
             ;; When the control channel is closed, close the channel
             ;; to the server and exit the process
             (do
               (async/close! dispatch-chan)
               (log "Exiting dispatcher"))

             [[in-chan out-chan] ctrl-chan]
             ;; Notify the server of a new user and its incoming and
             ;; outgoing channels
             (do
               (let [pair (->IOPair in-chan out-chan)]
                 (>! dispatch-chan [:add next-uid pair])
                 (recur (inc next-uid) (assoc ins-to-uids in-chan next-uid))))

             [nil in-chan]
             ;; When a incoming channel is closed, notify the server
             (do
               (>! dispatch-chan [:remove (ins-to-uids in-chan)])
               (recur next-uid (dissoc ins-to-uids in-chan)))

             [msg in-chan]
             ;; Pass along a message that comes in from a user to the server
             (do

               (>! dispatch-chan [:message (ins-to-uids in-chan) msg])
               (recur next-uid ins-to-uids))

             :else (throw (java.lang.Exception.
                           "No match found. (create-dispatcher-process!)"))))
         (catch Exception e (println "IO Process Exception:" e)))))
    ctrl-chan))
