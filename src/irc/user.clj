(ns irc.user)

(defrecord User [uid io registered? nick realname cnames])

(defn ->user [uid io]
  (->User uid io false nil nil #{}))

(defn uid [user]
  (:uid user))

(defn io [user]
  (:io user))

(defn registered? [user]
  (:registered? user))

(defn nick [user]
  (:nick user))

(defn realname [user]
  (:realname user))

(defn cnames [user]
  (:cnames user))

(defn set-registered? [user registered?]
  (assoc-in user [:registered?] registered?))

(defn set-nick [user nick]
  (assoc-in user [:nick] nick))

(defn set-realname [user realname]
  (assoc-in user [:realname] realname))

(defn add-channel [user cname]
  (update-in user [:cnames] conj cname))

(defn remove-channel [user cname]
  (update-in user [:cnames] disj cname))

(defn print-user [user]
  (println "uid: " (uid user)
           " nick: " (nick user)
           " realname: " (realname user))
  (println "  registered?: " (registered? user))
  (println "  cnames: " (cnames user)))
