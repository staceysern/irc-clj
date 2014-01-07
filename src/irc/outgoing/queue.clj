(ns irc.outgoing.queue)

(defn ->queue []
  {:current nil
   :pending (clojure.lang.PersistentQueue/EMPTY)})

(defn current [state socket]
  (get-in @(state :!queues) [socket :current]))

(defn pending [state socket]
  (get-in @(state :!queues) [socket :pending]))
