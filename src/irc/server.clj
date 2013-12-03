(ns irc.server
  (:require [irc.channel :refer :all]
            [irc.core :refer :all]
            [irc.user :refer :all]
            [clojure.set :refer [difference union]]))

(defn make-server []
  {:host (.getHostName (java.net.InetAddress/getLocalHost))
   :version "irc.sds.0.1"
   :create-date "Wed Nov 20 2013 at 15:19:23 EST"
   :users {}          ;; map uid to users
   :channels {}       ;; map channel name to channels
   })

(defn server-host [server]
  (:host server))

(defn server-version [server]
  (:version server))

(defn server-create-date [server]
  (:create-date server))

(defn uids [server]
  (sequence (keys (:users server))))

(defn users [server]
  (sequence (vals (:users server))))

(defn user-by-uid [server uid]
  (get-in server [:users uid]))

(defn user-by-nick [server nick]
  (let [user (filter #(= (user-nick %) nick) (vals (:users server)))]
    (if (zero? (count user))
      nil
      (first user))))

(defn channel-names [server]
  (sequence (keys (:channels server))))

(defn channels [server]
  (sequence (vals (:channels server))))

(defn channel-by-name [server cname]
  (get-in server [:channels cname]))

(defn uids-on-channel [server cname]
  (sequence (channel-uids (channel-by-name server cname))))

(defn users-on-channel [server cname]
  (map #(user-by-uid server %)
       (uids-on-channel server cname)))

(defn channels-for-user [server uid]
  (map #(channel-by-name server %)
       (user-cnames (user-by-uid server uid))))

(defn uids-on-channels-with [server uid]
  (sequence (disj (reduce union (map (comp set channel-uids)
                                     (channels-for-user server uid)))
                  uid)))

(defn add-user [server user]
  (update-in server [:users] assoc (:uid user) user))

(defn add-channel [server channel]
  (update-in server [:channels] assoc (:cname channel) channel))

(defn add-user-to-channel [server uid cname]
  (-> server
      (assoc-in [:channels cname]
                (channel-add-user (channel-by-name server cname) uid))
      (assoc-in [:users uid]
                (user-add-channel (user-by-uid server uid) cname))))

(defn remove-user-from-channel [server uid cname]
  (-> server
      (assoc-in [:channels cname]
                (channel-remove-user (channel-by-name server cname) uid))
      (assoc-in [:users uid]
                (user-remove-channel (user-by-uid server uid) cname))))

(defn remove-user [server uid]
  (update-in (reduce #(remove-user-from-channel %1 uid %2)
                     server
                     (user-cnames (user-by-uid server uid)))
             [:users] dissoc uid))

(defn remove-channel [server cname]
  (if-not (zero? (count (channel-uids (channel-by-name server cname))))
    (throw (IllegalStateException. "Can't remove a channel with users on it."))
    (update-in server [:channels] dissoc cname)))

(defn update-user [server uid set-fn value]
  (update-in server [:users uid] set-fn value))

(defn print-server [server]
  (doseq [u (users server)]
    (print-user u))
  (doseq [c (channels server)]
    (print-channel c)))
