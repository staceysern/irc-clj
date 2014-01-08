(ns irc.outgoing.select
  (:import  [java.nio.channels SelectionKey])
  (:require [irc.core :refer [log]]
            [irc.outgoing.queue :refer [current pending]]
            [clojure.core.async :refer [put!]]
            [clojure.set :refer [difference]]))

;; The outgoing select loop cooperates with the outgoing process to
;; transmit messages.  The two threads share the !queues hash map
;; which maps each sockets to a hash map which contains a pending queue
;; and a current buffer.  The outgoing process adds entries into
;; the pending queue and the outgoing select loop removes entries from
;; it.  The entries are either messages to be transmitted or an
;; indication to disconnect.

;; The outgoing select loop registers a socket to provide notification
;; that it is ready for writing only when a message is pending for that
;; socket.  To begin transmitting a message, the select loop removes it
;; from the pending queue, encodes it for transmission, and stores it as
;; the current buffer for the socket.  It repeatedly writes from the
;; buffer to the socket until the entire message has been sent.  If
;; further messages are pending, it repeats the process for the next
;; one.  If no further messages are pending, it unregisters the
;; socket.  If the next entry in the pending queue is a disconnect
;; notification, it closes the socket and notifies the outgoing
;; process that the disconnect has taken place.

(defn set-current! [state socket value]
  (swap! (:!queues state)
         (fn [queues]
           (assoc-in queues [socket :current] value)))
  state)

(defn pop-pending! [state socket]
  "Remove the oldest entry in the pending queue for a socket."
  (swap! (:!queues state)
         (fn [queues]
           (update-in queues [socket :pending] pop)))
  state)

(defn registered-sockets [state]
  "Return a sequence of sockets which are registered."
  (keys (filter (fn [queue]
                  (get-in (val queue) [:current]))
                @(:!queues state))))

(defn sockets-with-pending-messages [state]
  "Return a sequence of sockets which have pending messages."
  (keys (filter (fn [queue]
                  (seq (get-in (val queue) [:pending])))
                @(:!queues state))))

(defn sending? [state socket]
  "Return true for a socket which is in the midst of transmitting a message
   and false otherwise."
  (not (nil? (current state socket))))

(defn pending? [state socket]
  "Return true for a socket which has pending messages."
  (boolean (seq (pending state socket))))

(defn disconnect! [state socket]
  "Close a socket and notify the outgoing process."
  (.close socket)
  (put! (:outgoing-chan state) [:disconnected socket])
  state)

(defn start-next-message! [state socket]
  "Begin transmission of the next message in the pending queue."
  (let [buffer (.encode (:charset state)
                        (str (peek (pending state socket)) "\r\n"))]
    (try
      (.write socket buffer)
      (catch Exception e))
    (set-current! state socket buffer)
    (pop-pending! state socket)))

(defn next-message-is-disconnect [state socket]
  "Return true if the next message in the pending queue for a socket is a
   disconnect indication and false otherwise."
  (= (peek (pending state socket)) :disconnect))

(defn register [state socket]
  "Register a socket to provide notifications when it is ready to write."
  (.register socket (:selector state) SelectionKey/OP_WRITE)
  state)

(defn unregister [state socket key]
  "Stop receiving notifications from a socket."
  (.cancel key)
  (set-current! state socket nil))

(defn put-socket! [state key]
  "Manage a socket which is ready for writing.  The current buffer contains
   the message that is in the process of being transmitted.  Each call to
   write on the socket modifies the buffer to reflect how many bytes have been
   sent.  This function handles three cases: a socket which has just been
   registered, a socket that has completed sending a message, and a socket
   which is in the midst of transmitting a message."
  (let [socket (.channel key)
        buffer (current state socket)]
    (cond (not (sending? state socket))
          (if (next-message-is-disconnect state socket)
            (disconnect! state socket)
            (start-next-message! state socket))

          (zero? (.remaining buffer))
          (if (not (pending? state socket))
            (unregister state socket key)
            (if (next-message-is-disconnect state socket)
              (-> state
                  (unregister socket key)
                  (disconnect! socket))
              (start-next-message! state socket)))

          :else (try
                  (.write socket buffer)
                  state
                  (catch Exception e state)))))

(declare service-keys)

(defn select [state selector]
  "Wait until one of a set of sockets is ready for writing or the outgoing
   process sends a wakeup.  When a message comes in for a socket which has
   no other pending messages, the socket may need to be registered.  In that
   instance, the outgoing process sends a wakeup to the outgoing select loop.
   Each time the outgoing select loop unblocks, it determines whether any
   sockets need to be registered by comparing the list of sockets which are
   registered to those which have pending messages.  If there are none, it
   services the set of sockets which are read for writing."
  (.select selector)
  (let [registered (registered-sockets state)
        queued (sockets-with-pending-messages state)]
    (when (not= (count registered) (count queued))
      (let [sockets (difference (set queued)
                                (set registered))]
        (doseq [socket sockets]
          (register state socket)))))
  #(service-keys state selector (-> selector (.selectedKeys) (.iterator))))

(defn service-keys [state selector iter]
  "Iterate through a set of ready keys."
  (if (.hasNext iter)
    (let [key (.next iter)
          state' (try
                   (if (.isWritable key)
                     (put-socket! state key)
                     (throw Exception "Key not writable (Should never happen)"))
                   (catch Exception e state))]
      (.remove iter)
      (recur state' selector iter))
    #(select state selector)))

(defn start-outgoing-select-loop! [state selector]
  (try
    (trampoline (select state selector))
    (catch Exception e (println "Outgoing select loop exception:")
           (.printStackTrace e))))
