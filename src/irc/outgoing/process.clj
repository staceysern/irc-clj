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

(defn conj-pending! [state socket value]
  (swap! (:!queues state)
         (fn [queues]
           (update-in queues [socket :pending] conj value)))
  state)

(defn add-queue! [state socket]
  (swap! (:!queues state)
         (fn [queues] (assoc queues socket (->queue))))
  state)

(defn remove-queue! [state socket]
  (swap! (:!queues state)
         (fn [queues] (dissoc queues socket)))
  state)

(defn enqueue [state socket message]
  (let [wakeup? (empty? (pending state socket))
        state' (conj-pending! state socket message)]
    (when wakeup?
      (.wakeup (:selector state')))
    state'))

(defn disconnect-before-incoming! [state socket]
  (let [!io-map (:!io-map state)]
    (remove-io-pair->socket! !io-map (socket->io-pair !io-map socket)))
  (enqueue state socket :disconnect))

(defn disconnect-after-incoming! [state socket io-pair]
  (let [!io-map (:!io-map state)]
    (remove-io-pair->socket! (:!io-map state) io-pair))
  (remove-queue! state socket))

(defn start-outgoing-process! [system ctrl-chan]
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
            (recur (add-queue! state (io-pair->socket (:!io-map state)
                                                      io-pair)))

            [[:remove io-pair] ctrl-chan]
            ;; The incoming process has let us know that it detected a lost
            ;; connection. Close things down immediately
            (recur (disconnect-after-incoming!
                    state (io-pair->socket (:!io-map state) io-pair)
                    io-pair))

            [[:disconnected socket] ctrl-chan]
            (recur (remove-queue! state socket))

            [nil out-chan]
            ;; The server has let us know that the user has quit.
            ;; We need to send out any queued messages and then drop the
            ;; connection
            (recur (disconnect-before-incoming!
                    state
                    (out-chan->socket (:!io-map state) out-chan)))

            [message out-chan]
            ;; An outgoing message has been received from the server
            (recur (enqueue state (out-chan->socket (:!io-map state) out-chan)
                            message))

            :else (throw (java.lang.Exception.
                          "No match found. (start-outgoing-process!)")))
          (catch Exception e (println "Outgoing process exception:" )
                 (.printStackTrace e)))))
    ctrl-chan))
