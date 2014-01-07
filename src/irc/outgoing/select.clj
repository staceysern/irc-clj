(ns irc.outgoing.select
  (:import  [java.nio.channels SelectionKey])
  (:require [irc.core :refer [log]]
            [irc.outgoing.queue :refer [current pending]]
            [clojure.core.async :refer [put!]]
            [clojure.set :refer [difference]]))

(defn set-current! [state socket value]
  (swap! (:!queues state)
         (fn [queues]
           (assoc-in queues [socket :current] value)))
  state)

(defn pop-pending! [state socket]
  (swap! (:!queues state)
         (fn [queues]
           (update-in queues [socket :pending] pop)))
  state)

(defn registered-sockets [state]
  (keys (filter (fn [queue]
                  (get-in (val queue) [:current]))
                @(:!queues state))))

(defn sockets-with-pending-messages [state]
  (keys (filter (fn [queue]
                  (seq (get-in (val queue) [:pending])))
                @(:!queues state))))

(defn sending? [state socket]
  (not (nil? (current state socket))))

(defn pending? [state socket]
  (boolean (seq (pending state socket))))

(defn disconnect! [state socket]
  (.close socket)
  (put! (:outgoing-chan state) [:disconnected socket])
  state)

(defn start-next-message! [state socket]
  (let [buffer (.encode (:charset state)
                        (str (peek (pending state socket)) "\r\n"))]
    (try
      (.write socket buffer)
      (catch Exception e))
    (set-current! state socket buffer)
    (pop-pending! state socket)))

(defn next-message-is-disconnect [state socket]
  (= (peek (pending state socket)) :disconnect))

(defn register [state socket]
  (.register socket (:selector state) SelectionKey/OP_WRITE)
  state)

(defn unregister [state socket key]
  (.cancel key)
  (set-current! state socket nil))

(defn put-socket! [state key]
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
