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

;; The incoming process is a select loop over a set of sockets.
;; Initially that set consists of a listener socket which is
;; registered to provide notification of connection requests.  When
;; a connection request is accepted, the resulting socket is
;; registered to provide notification of incoming bytes and a new
;; entry is made into the connections hash map which maps the socket
;; to a hash map used for buffering incoming bytes.

;; The connections entry for a socket consists of a buffer into which
;; bytes are read from the socket, start and scan indices which are used
;; in scanning the incoming bytes for delimiters to determine the bounds
;; of a complete message, and a discard? flag which indicates that
;; bytes received until the next delimiter should be discarded.

;; When a complete message is received from a socket, the incoming
;; process passes it to the server process through the incoming
;; channel of the io-pair associated with the socket.  When the
;; incoming process detects that a client has disconnected, it removes
;; the entry for the socket in the socket->io-pair mapping in the
;; !io-map.  Then it closes the socket and notifies the server process
;; and the outgoing process.

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
  "Accept a new connection and register it to provide notifications of
   received bytes.  Return a list of the new state and the new connection, or
   nil if an error occurs."
  (try
    (let [connection (-> key (.channel) (.accept))
          state' (add-connection state connection)]
      (.configureBlocking connection false)
      (.register connection (:selector state) SelectionKey/OP_READ
                 (buffer state' connection))
      [state' connection])
    (catch Exception e nil)))

(defn accept-connection! [state key]
  "Accept a connection request, create an io-pair for the socket, add the
   mapping between the socket and io-pair to the !io-map, and notify the
   outgoing process and the server process of the new user."
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
  "Complete the process of removing a connection that was begun by the
   outgoing process."
  (if (empty? sockets)
    state
    (do
      (remove-socket->io-pair! (:!io-map state) (first sockets))
      (recur (remove-connection state (first sockets)) (rest sockets)))))

(defn remove-connection-before-outgoing! [state key]
  "Start the process of removing a connection and notify the outgoing and the
   server processes."
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
  "Move the remaining bytes in the buffer for a socket to the beginning."
  (let [position (.position (buffer state connection))]
    (.compact (buffer state connection))
    (-> state
        (set-start connection (- (start state connection) position))
        (set-scan connection (- (scan state connection) position)))))

(defn discard! [state connection]
  "Discard the contents of the buffer for a socket."
  (.clear (buffer state connection))
  (.flip  (buffer state connection))
  (-> state
      (set-start connection 0)
      (set-scan connection 0)
      (set-discard? connection true)))

(defn get-message! [state connection]
  "Return a list consisting of the next message received on a socket and
   the new server state.  If a full carriage return/line feed delimited
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
  "Read from a socket into its buffer.  Return true if the read is successful
   and false otherwise."
  (let [connection (.channel key)]
    (try
      (> (.read connection (buffer state connection)) 0)
      (catch java.io.IOException e false))))

(defn take-connection! [state key]
  "Read from a socket and pass each complete message received to the server.
   A failure on the read indicates that the client has disconnected.  In that
   case, start the process of removing recources associated with that client.
   Otherwise, repeatedly look for the next complete message in the buffer and
   pass it to the server process through the incoming channel of the io-pair
   associated with the socket.  When no more complete messages are left in
   the buffer, move any remaining bytes to the beginning of the buffer to
   leave adequate space for the rest of the message."
  (if-not (read-connection! state key)
    (remove-connection-before-outgoing! state key)
    (let [connection (.channel key)]
      (.flip (buffer state connection))
      ;; Repeatedly look for the next complete message in the buffer.
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
  "Wait until one of a set of sockets is ready to be read or accepted but
   timeout after five seconds.  The timeout is designed to allow a cleanup of
   resources for sockets which were disconnected at the behest of the
   outgoing process even when there is no other system activity.  When a
   socket is disconnected by the outgoing process, that process removes the
   socket's entry in the io-pairs->sockets map.  Each time the select
   unblocks, compare the numbers of entries in the io-pairs->sockets map to
   the number in the sockets->io-pairs map.  When they differ, determine which
   sockets have been disconnected and remove the incoming process's data
   for those sockets.  Otherwise, service the set of sockets which are ready."
  ;; seconds has passed.
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
  "Iterate through a set of ready keys accepting connections or handling
   incoming bytes."
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
