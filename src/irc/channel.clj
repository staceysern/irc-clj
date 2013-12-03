(ns irc.channel)

(defrecord Channel [cname uids])

(defn make-channel [cname]
  (->Channel cname #{}))

(defn channel? [channel]
  (instance? Channel channel))

(defn channel-cname [channel]
  (:cname channel))

(defn channel-uids [channel]
  (:uids channel))

(defn channel-add-user [channel uid]
  (update-in channel [:uids] conj uid))

(defn channel-remove-user [channel uid]
  (update-in channel [:uids] disj uid))

(defn print-channel [channel]
  (println "channel: " (channel-cname channel))
  (println "   uids: " (channel-uids channel)))
