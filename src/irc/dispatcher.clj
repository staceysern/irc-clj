(ns irc.dispatcher
  (:require [irc.core :refer :all]
            [irc.parser :refer :all]
            [irc.receive :refer :all]
            [irc.server :refer :all]
            [irc.user :refer :all]
            [clojure.core.async :as async :refer [go <!]]
            [clojure.core.match :refer [match]]))

(defn create-dispatcher-process!
  ;; Create an asynchronous process that monitors a channel which
  ;; receives messages from users and dispatches the messages to be
  ;; handled

  ;; Params: msg-chan is the channel for incoming messages
  ;; Return: the message channel
  [msg-chan]
  
  (go
   (loop [server (make-server)]
     (try
       (match (<! msg-chan)
         nil
         ;; When the message channel is closed, exit the process
         (log "Exiting server")
         
         [:add uid io]
         ;; When a new connection is made, add a user to the server
         (do (log "Add " uid)
             (recur (add-user server (make-user uid io))))
         
         [:remove uid]
         ;; When a connection is lost, remove the user from
         ;; the server
         (do (log "Remove " uid)
             (recur (remove-user server uid)))
         
         [:message uid message]
         ;; When an IRC message string is received from a user, process it
         (do (log (format "%2d rx: %s" uid message))
             (recur (process-command server uid (parse message))))

         :else (throw (java.lang.Exception.
                       "No match found. (create-server-process!)")))
       (catch Exception e (println "Server exception:" e))))
   msg-chan))



