(ns irc.server.process
  (:require [irc.core :refer [log]]
            [irc.iopair :refer [in-chan]]
            [irc.server.parser :refer [parse]]
            [irc.server.receive :refer [process-command]]
            [irc.server.server :refer [->server add-user user-by-uid]]
            [irc.server.user :refer [->user]]
            [clojure.core.async :refer [go]]
            [clojure.core.match :refer [match]]))

(defn start-server-process!
  [ctrl-chan]
  (go
    (loop [next-uid 1
           ins->uids {}
           server (->server)]
      (try
        (match (alts! (conj (keys ins->uids) ctrl-chan))
          [nil ctrl-chan]
          ;; When the control channel is closed, exit the process
          nil

          [io-pair ctrl-chan]
          (do
            (log (format "%5d connected" next-uid))
            (recur (inc next-uid) (assoc ins->uids (in-chan io-pair) next-uid)
                   (add-user server (->user next-uid io-pair))))

          [nil in-chan]
          (do
            (let [uid (ins->uids in-chan)]
              (log (format "%5d disconnected" uid))
              (recur next-uid (dissoc ins->uids in-chan)
                     (process-command server uid {:command :disconnect}))))

          [message in-chan]
          (do
            (let [uid (ins->uids in-chan)]
              ;; Make sure that the server hasn't disconnected the
              ;; user.  This could happen if the user sends a quit
              ;; message followed by another message.
              (if (user-by-uid server uid)
                (do
                  (log (format "%5d rx: %s" uid message))
                  (recur next-uid ins->uids
                         (process-command server uid (parse message))))
                (recur next-uid ins->uids server))))

          :else (throw (java.lang.Exception.
                        "No match found. (start-server-process!)")))
        (catch Exception e (println "Server process exception:" e)
               (.printStackTrace e)))))
  ctrl-chan)
