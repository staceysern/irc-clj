(ns irc.outgoing.process
  (:import [java.nio.channels Selector]
           [java.nio.charset Charset])
  (:require [irc.core :refer [log]]
            [irc.io :refer [out-chan->socket out-chans
                            socket->io-pair io-pair->socket
                            remove-io-pair->socket!]]
            [irc.outgoing.queue :refer [->queue pending]]
            [irc.outgoing.select :refer [start-outgoing-select-loop!]]
            [clojure.core.async :as async :refer [go]]
            [clojure.core.match :refer [match]]))

;; The outgoing process cooperates with the outgoing select loop to
;; transmit messages sent by the server process.  The two threads
;; share the !queues hash map which maps each socket to a hash map which
;; contains a pending queue.  The outgoing process adds entries into
;; the pending queue and the outgoing select loop removes entries from
;; it.  The entries are either messages to be transmitted or an
;; indication to disconnect.

;; The server process passes outgoing messages for a socket through the
;; outgoing channel of the io-pair associated with the socket.  When the
;; server process receives a quit message from the client, it closes the
;; outgoing channel.  The outgoing process then removes the entry for
;; the socket in the io-pair->socket mapping in the !io-map and places a
;; disconnect indication in the socket's pending queue for the outgoing
;; select loop to handle after it has transmitted all other queued messages
;; for the socket.  After the outgoing select loop disconnects, it notifies
;; the outgoing process through its control channel.

(defn conj-pending! [state socket value]
  "Add an entry into the pending queue for a socket."
  (swap! (:!queues state)
         (fn [queues]
           (update-in queues [socket :pending] conj value)))
  state)

(defn add-queue! [state socket]
  "Add an entry for a socket into the !queues hash map."
  (swap! (:!queues state)
         (fn [queues] (assoc queues socket (->queue))))
  state)

(defn remove-queue! [state socket]
  "Remove the entry for a socket from the !queues hash map."
  (swap! (:!queues state)
         (fn [queues] (dissoc queues socket)))
  state)

(defn enqueue [state socket message]
  "Enqueue a message for a socket into its pending queue. If the socket
   has no other pending messages, wakeup the select loop so that it can
   start transmitting the message."
  (let [wakeup? (empty? (pending state socket))
        state' (conj-pending! state socket message)]
    (when wakeup?
      (.wakeup (:selector state')))
    state'))

(defn disconnect-before-incoming! [state socket]
  "Start the process of removing a connection."
  (let [!io-map (:!io-map state)]
    (remove-io-pair->socket! !io-map (socket->io-pair !io-map socket)))
  (enqueue state socket :disconnect))

(defn disconnect-after-incoming! [state socket io-pair]
  "Complete the process of removing a connection that was begun by the
   incoming process."
  (let [!io-map (:!io-map state)]
    (remove-io-pair->socket! (:!io-map state) io-pair))
  (remove-queue! state socket))

(defn start-outgoing-process! [system ctrl-chan]
  "Start the outgoing select loop thread and then monitor the outgoing
   control channel and the io-pairs' outgoing channels for messages."
  (let [selector (Selector/open)
        state (merge system
                     {:selector selector
                      :charset (Charset/forName "US-ASCII")
                      :!queues (atom {})})]
    (async/thread (start-outgoing-select-loop! state selector))
    (go
      (loop [state state]
        (try
          (match (alts! (conj (out-chans (:!io-map state)) ctrl-chan))
            [nil ctrl-chan]
            ;; When the control channel is closed, exit the process
            nil

            [[:add io-pair] ctrl-chan]
            ;; Notification of a new connection from the incoming process.
            (recur (add-queue! state (io-pair->socket (:!io-map state)
                                                      io-pair)))

            [[:remove io-pair] ctrl-chan]
            ;; Notification of a lost connection from the incoming process.
            ;; Stop processing outgoing messages immediately.
            (recur (disconnect-after-incoming!
                    state (io-pair->socket (:!io-map state) io-pair)
                    io-pair))

            [nil out-chan]
            ;; Notification from the server process that the user has quit.
            ;; Send out any queued messages before dropping the connection.
            (recur (disconnect-before-incoming!
                    state
                    (out-chan->socket (:!io-map state) out-chan)))

            [[:disconnected socket] ctrl-chan]
            ;; Notification from the outgoing select loop that all
            ;; queued messages have been sent and the socket has been closed.
            (recur (remove-queue! state socket))

            [message out-chan]
            ;; Notification of an outgoing message from the server process.
            ;; Queue it for transmission by the outgoing select loop.
            (recur (enqueue state (out-chan->socket (:!io-map state) out-chan)
                            message))

            :else (throw (java.lang.Exception.
                          "No match found. (start-outgoing-process!)")))
          (catch Exception e (println "Outgoing process exception:" )
                 (.printStackTrace e)))))
    ctrl-chan))
