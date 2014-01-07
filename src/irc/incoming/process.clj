(ns irc.incoming.process
  (:import [java.nio ByteBuffer]
           [java.nio.channels Selector SelectionKey]
           [java.nio.charset Charset])
  (:require [irc.core :refer [log]]
            [irc.io :refer [socket->io-pair io-pair->socket
                            add-io! remove-socket->io-pair!]]
            [irc.iopair :refer [->IOPair in-chan]]
            [clojure.core.async :as async :refer [put!]]
            [clojure.set :refer [difference]]))

(def channel-size 100)

(def max-message-length 510)

(def carriage-return (byte \return))
(def line-feed (byte \newline))

(defn ->buffer []
  {:buffer (ByteBuffer/allocate 1024)
   :start 0
   :scan 0
   :discard? false})

(defn buffer [state connection]
  (get-in state [:connections connection :buffer]))

(defn start [state connection]
  (get-in state [:connections connection :start]))

(defn scan [state connection]
  (get-in state [:connections connection :scan]))

(defn discard? [state connection]
  (get-in state [:connections connection :discard?]))

(defn set-start [state connection value]
  (assoc-in state [:connections connection :start] value))

(defn set-scan [state connection value]
  (assoc-in state [:connections connection :scan] value))

(defn set-discard? [state connection value]
  (assoc-in state [:connections connection :discard?] value))

(defn add-connection [state connection]
  (assoc-in state [:connections connection] (->buffer)))

(defn remove-connection [state connection]
  (update-in state [:connections] dissoc connection))

(defn accept-and-register! [state key]
  (try
    (let [connection (-> key (.channel) (.accept))
          state' (add-connection state connection)]
      (.configureBlocking connection false)
      (.register connection (:selector state) SelectionKey/OP_READ
                 (buffer state' connection))
      [state' connection])
    (catch Exception e nil)))

(defn accept-connection! [state key]
  (if-let [[state' connection] (accept-and-register! state key)]
    (let [in-chan (async/chan channel-size)
          out-chan (async/chan channel-size)
          pair (->IOPair in-chan out-chan)]

      (add-io! (:!io-map state') connection pair)
      (put! (:outgoing-chan state') [:add pair])
      (put! (:server-chan state') pair)
      state')
    state))

(defn remove-connection-after-outgoing! [state sockets]
  (if (empty? sockets)
    state
    (do
      (remove-socket->io-pair! (:!io-map state) (first sockets))
      (recur (remove-connection state (first sockets)) (rest sockets)))))

(defn remove-connection-before-outgoing! [state key]
  (let [connection (.channel key)
        io-pair (socket->io-pair (:!io-map state) connection)]
    (.cancel key)
    (remove-socket->io-pair! (:!io-map state) connection)

    (when (io-pair->socket (:!io-map state) io-pair)
      (try
        (.close connection)
        (catch Exception e))
      (async/close! (in-chan io-pair))
      (put! (:outgoing-chan state) [:remove io-pair]))
    (remove-connection state connection)))

(defn compact! [state connection]
  (let [position (.position (buffer state connection))]
    (.compact (buffer state connection))
    (-> state
        (set-start connection (- (start state connection) position))
        (set-scan connection (- (scan state connection) position)))))

(defn discard! [state connection]
  (.clear (buffer state connection))
  (.flip  (buffer state connection))
  (-> state
      (set-start connection 0)
      (set-scan connection 0)
      (set-discard? connection true)))

(defn get-message! [state connection]
  "Return a list consisting of the next message and the new server state
   received on the socket.  If a full carriage return/line feed delimited
   message is not available, return nil for the next message."
  (let [buf (buffer state connection)]
    (loop [state state
           x (scan state connection)]

      (let [discard? (discard? state connection)]
        (cond (= x (.limit buf))
              ;; Reached the end of the input without detecting a
              ;; message delimiter.  If the length of the portion of the
              ;; message received so far exceeds the maximum message
              ;; length or the discard? flag is set, discard the contents
              ;; of the buffer and continue discarding the message until
              ;; a message delimiter is received.
              (if (and (not discard?)
                       (<= (.remaining buf) max-message-length))
                (list nil (set-scan state connection (.limit buf)))
                (list nil (discard! state connection)))

              (and (= (.get buf x) carriage-return)
                   (= (.limit buf) (inc x)))
              ;; Encountered a carriage return at the end of the input.
              ;; If the length of the portion of the message received so
              ;; far, excluding the carriage return, exceeds the maximum
              ;; message length or the discard? flag is set, discard the
              ;; contents of the buffer and continue discarding the message
              ;; until a message delimiter is received. Next time start
              ;; scanning for a message delimiter at the carriage return
              ;; position.
              (if (and (not discard?)
                       (<= (dec (.remaining buf)) max-message-length))
                (list nil (set-scan state connection x))
                (list nil (discard! state connection)))

              (and (= (.get buf x) carriage-return)
                   (= (.get buf (inc x)) line-feed))
              ;; Found a message delimiter.  If the length of the message
              ;; exceeds the maximum message length or the discard? flag
              ;; is set, discard the message and continue scanning the
              ;; buffer for a message.  Otherwise, convert the
              ;; message into a string.
              (if (or discard?
                      (> (- x (scan state connection)) max-message-length))
                (do
                  (.position buf (+ x 2))
                  (recur (-> state
                             (set-start connection (+ x 2))
                             (set-scan connection (+ x 2))
                             (set-discard? connection false))
                         (+ x 2)))
                (let [limit (.limit buf)
                      state' (-> state
                                 (set-start connection (+ x 2))
                                 (set-scan connection (+ x 2)))]
                  (try
                    (.position buf (start state connection))
                    (.limit buf x)
                    (list (.toString (.decode (:charset state) (.slice buf)))
                          state')
                    (catch Exception e
                      (log "Couldn't decode message: " e)
                      (list nil state'))
                    (finally
                      (.limit buf limit)
                      (.position buf (+ x 2))))))

              :else (recur state (inc x)))))))

(defn read-connection! [state key]
  ;; Returns true if socket has been successfully read
  ;; false otherwise
  (let [connection (.channel key)]
    (try
      (> (.read connection (buffer state connection)) 0)
      (catch java.io.IOException e false))))

(defn take-connection! [state key]
  (if-not (read-connection! state key)
    (remove-connection-before-outgoing! state key)
    (let [connection (.channel key)]
      (.flip (buffer state connection))
      (loop [state state]
        (let [[message state'] (get-message! state connection)]
          (if (nil? message)
            (compact! state' connection)
            (do
              (put! (in-chan (socket->io-pair (:!io-map state') connection))
                    message)
              (recur state'))))))))

(declare service-keys)

(defn select [state selector]
  (.select selector 5000)
  (let [!io-map @(:!io-map state)]
    (if (= (count (:sockets->io-pairs !io-map))
           (count (:io-pairs->sockets !io-map)))
      #(service-keys state selector (-> selector (.selectedKeys) (.iterator)))
      (let [sockets (difference (set (keys (:sockets->io-pairs !io-map)))
                                (set (map val
                                          (:io-pairs->sockets !io-map))))]
        (recur (remove-connection-after-outgoing! state sockets) selector)))))

(defn service-keys [state selector iter]
  (if (.hasNext iter)
    (let [key (.next iter)
          state' (try
                   (cond (.isReadable key) (take-connection! state key)
                         (.isAcceptable key) (accept-connection! state key)
                         :else (throw Exception e
                                      (str "Key not acceptable or readable "
                                           "(Should never happen)")))
                   (catch Exception e state))]
      (.remove iter)
      (recur state' selector iter))
    #(select state selector)))

(defn start-incoming! [system]
  (try
    (let [selector (Selector/open)
          state (merge system {:selector selector
                               :charset (Charset/forName "US-ASCII")
                               :connections {}})]
      (.register (:listener state) selector SelectionKey/OP_ACCEPT)
      (trampoline (select state selector)))
    (catch Exception e (println "Incoming process exception:")
           (.printStackTrace e))))

(defn start-incoming-process! [system]
  (async/thread (start-incoming! system)))
