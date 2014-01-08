(ns irc.io)

;; The !io-map provides a mapping between sockets and io-pairs.  It is
;; an atom that is shared between the incoming and outgoing processes.

(defn ->!io-map []
  (atom
   {:sockets->io-pairs {}
    :io-pairs->sockets {}}))

(defn socket->io-pair [!io-map socket]
  (get-in @!io-map [:sockets->io-pairs socket]))

(defn io-pair->socket [!io-map io-pair]
  (get-in @!io-map [:io-pairs->sockets io-pair]))

(defn io-pairs [!io-map]
  (keys (:io-pairs->sockets @!io-map)))

(defn sockets [!io-map]
  (keys (:sockets->io-pairs @!io-map)))

(defn out-chans [!io-map]
  (map :out-chan (io-pairs !io-map)))

(defn out-chan->socket [!io-map out-chan]
  (let [io-pair
        (first (for [pair (io-pairs !io-map) :when (= (:out-chan pair) out-chan)]
                 pair))]
    (if-not (nil? io-pair)
      (io-pair->socket !io-map io-pair)
      nil)))

(defn add-io! [!io-map socket io-pair]
  (swap! !io-map
         (fn [io-map]
           (-> io-map
               (assoc-in [:sockets->io-pairs socket] io-pair)
               (assoc-in [:io-pairs->sockets io-pair] socket)))))

(defn remove-socket->io-pair! [!io-map socket]
  (swap! !io-map
         (fn [io-map]
           (update-in io-map [:sockets->io-pairs] dissoc socket))))

(defn remove-io-pair->socket! [!io-map io-pair]
  (swap! !io-map
         (fn [io-map]
           (update-in io-map [:io-pairs->sockets] dissoc io-pair))))
