(ns irc.channel)

(defrecord Channel [cname uids])

(defn ->channel [cname]
  (->Channel cname #{}))

(defn channel? [channel]
  (instance? Channel channel))

(defn cname [channel]
  (:cname channel))

(defn uids [channel]
  (:uids channel))

(defn add-user [channel uid]
  (update-in channel [:uids] conj uid))

(defn remove-user [channel uid]
  (update-in channel [:uids] disj uid))

(defn print-channel [channel]
  (println "channel: " (cname channel))
  (println "   uids: " (uids channel)))
