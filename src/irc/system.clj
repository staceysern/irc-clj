(ns irc.system
  (:import [java.net InetSocketAddress]
           [java.nio.channels ServerSocketChannel])
  (:require [irc.core :refer [log]]
            [irc.incoming.process :refer [start-incoming-process!]]
            [irc.io :refer [->!io-map sockets]]
            [irc.server.process :refer [start-server-process!]]
            [irc.outgoing.process :refer [start-outgoing-process!]]
            [clojure.core.async :as async :refer [<!! >!!]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class :main true))

;; This IRC Server supports a subset of the RFC 2812 client protocol and
;; has been designed to mimic the behavior of freenode servers which can
;; be reached at chat.freenode.net:6667.

;; This IRC server consists of four threads: the server process (the
;; word process is use liberally here), the incoming process, the
;; outgoing process, and the outgoing select loop.  A high-level
;; overview of these threads and the mechanism for communication
;; between threads follows.

;; The server process manages the state of the server, which consists
;; of a set of users and IRC channels.  Each user has an IOPair which
;; associates a pair of core.async channels, one to receive incoming
;; messages that come from the incoming process, and another to transmit
;; outgoing messages that are handled by the outgoing process.  The
;; pair of channels for a user has a direct correspondence to a socket
;; through which bidirectional communication takes place with an IRC
;; client.  The incoming and outgoing processes maintain and share the
;; !io-map which is an atom that maps sockets to io-pairs and vice
;; versa.

;; The incoming process accepts connection requests from IRC clients
;; on a listening port.  It reads bytes sent by a client from a socket,
;; scans the bytes for message delimiters, and passes complete messages
;; to the server process through an io-pair's incoming channel.

;; Transmission of messages from the server to clients is handled by a
;; pair of cooperating threads.  The outgoing process monitors the
;; io-pair outgoing channels for messages from the server.  When it
;; receives a message, it queues the message up for the outgoing
;; select loop which writes the message to the appropriate socket.

;; Communication between threads takes place between carefully
;; designed interfaces.  To minimize accidental violations, the code
;; for the threads has been placed in three separate directories (the
;; two outgoing threads share a directory).  Functions associated with
;; one thread should never call functions associated with one of the
;; other threads.  Functions which may be safely called by multiple
;; threads are found in the top level source directory.

(def !system (atom nil))

(defn create-listener! [port]
  (try
    (let [listener (ServerSocketChannel/open)]
      (.configureBlocking listener false)
      (.bind listener (InetSocketAddress. port)))
    (catch Exception e nil)))

(defn stop-irc-server! [system]
  (log "Stopping IRC Server")
  (async/close! (:server-chan system))
  (doseq [socket (sockets (:!io-map system))]
    (.close socket))
  (.close (:listener system))
  (swap! !system (fn [system]
                   nil)))

(defn start-irc-server! [port]
  (log "Starting IRC Server")
  (when @!system
    (stop-irc-server! !system))

  (let [server-chan (async/chan 100)
        outgoing-chan (async/chan 100)
        listener (create-listener! port)
        state {:!io-map (->!io-map)
               :server-chan server-chan
               :outgoing-chan outgoing-chan
               :listener listener}]
    (if listener
      (do
        (start-server-process! server-chan)
        (start-incoming-process! state)
        (start-outgoing-process! state outgoing-chan)
        (swap! !system (fn [system] state)))
      (do
        (println "Unable to start server on port" port)
        (System/exit 1)))))

(defn reset [port]
  (if @!system
    (stop-irc-server! @!system))
  (start-irc-server! port))

(def cli-options
  [["-p" "--port port"
    :default 6667
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000)
               "Must be a number between 0 and 65536"]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Usage: lein trampoline run irc [options]"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn error-msg [errors]
  (string/join \newline errors))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond (:help options) (exit 0 (usage summary))
          errors (exit 1 (error-msg errors)))

    (let [shutdown-chan (async/chan)]
      ;; Register a thread to be run before the JVM shuts down.  The
      ;; thread writes a message to the shutdown channel.
      (.addShutdownHook
       (Runtime/getRuntime)
       (Thread. #(async/put! shutdown-chan "Shutdown")))

      (async/thread (start-irc-server! (:port options)))

      ;; Block until a message is received on the shutdown channel and
      ;; then stop the server and release resources.
      (<!! shutdown-chan)
      (stop-irc-server! @!system))))
