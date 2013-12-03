(ns irc.user)

(defrecord User [uid io registered? nick realname cnames])

(defn make-user [uid io]
  (->User uid io false nil nil #{}))

(defn user-uid [user]
  (:uid user))

(defn user-io [user]
  (:io user))

(defn user-registered? [user]
  (:registered? user))

(defn user-nick [user]
  (:nick user))

(defn user-realname [user]
  (:realname user))

(defn user-cnames [user]
  (:cnames user))

(defn user-set-registered? [user registered?]
  (assoc-in user [:registered?] registered?))

(defn user-set-nick [user nick]
  (assoc-in user [:nick] nick))

(defn user-set-realname [user realname]
  (assoc-in user [:realname] realname))

(defn user-add-channel [user cname]
  (update-in user [:cnames] conj cname))

(defn user-remove-channel [user cname]
  (update-in user [:cnames] disj cname))

(defn print-user [user]
  (println "uid: " (user-uid user)
           " nick: " (user-nick user)
           " realname: " (user-realname user))
  (println "  registered?: " (user-registered? user))
  (println "  cnames: " (user-cnames user)))
