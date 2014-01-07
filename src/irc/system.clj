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
      (.addShutdownHook
       (Runtime/getRuntime)
       (Thread. #(async/put! shutdown-chan "Shutdown")))

      (async/thread (start-irc-server! (:port options)))

      (<!! shutdown-chan)
      (stop-irc-server! @!system))))
