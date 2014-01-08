(ns irc.outgoing.queue)

;; The !queues atom is shared between the two outgoing threads and
;; provides a mapping between sockets and their outgoing messages.
;; It contains a queue of pending messages which is written by the
;; outgoing process and read by the outgoing select loop.  The current
;; buffer is used by the outgoing select loop for the message
;; currently being transmitted.

(defn ->queue []
  {:current nil
   :pending (clojure.lang.PersistentQueue/EMPTY)})

(defn current [state socket]
  (get-in @(state :!queues) [socket :current]))

(defn pending [state socket]
  (get-in @(state :!queues) [socket :pending]))
