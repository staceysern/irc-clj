(ns irc.server.receive
  (:require [irc.server.channel :refer [->channel channel?]]
            [irc.server.send :refer [notify disconnect]]
            [irc.server.server :as server]
            [irc.server.user :as user]))

(def max-nick-len 16)

(defn valid-nick? [nick]
  (boolean (re-matches #"[a-zA-z\[\]\\\`_^{|}][a-zA-Z0-9\[\]\\\`_^{|}-]{0,15}"
                       nick)))

(defn valid-chan? [channel-name]
  (boolean (re-matches (re-pattern
                        (str "[&#][\u0001-\u0006\u0008-\u0009\u000B-\u000C"
                             "\u000E-\u001F\u0021-\u002B\u002D-\u0039"
                             "\u003B-\u00FF]{1,49}"))
                       channel-name)))

(defn welcome [server uid]
  (let [user (server/user-by-uid server uid)]
    (doseq [message [:rpl-welcome :rpl-yourhost :rpl-created :rpl-myinfo]]
      (notify user server {:message message}))))

(defn register [server uid]
  (let [server' (server/update-user server uid user/set-registered? true)]
    (welcome server' uid)
    server'))

(defn set-nick [server uid nick]
  (let [server' (server/update-user server uid user/set-nick nick)]
    (if (user/realname (server/user-by-uid server uid))
      (register server' uid)
      server')))

(defn change-nick [server uid nick]
  (let [server' (server/update-user server uid user/set-nick nick)]
    (doseq [u (conj (server/uids-on-channels-with server uid) uid)]
      (notify (server/user-by-uid server' u) server'
              {:message :nick
               :source (user/nick (server/user-by-uid server uid))
               :nick nick}))
    server'))

(defn set-realname [server uid realname]
  (let [server' (server/update-user server uid user/set-realname realname)]
    (if (user/nick (server/user-by-uid server uid))
      (register server' uid)
      server')))

(defn on-channel? [server uid cname]
  (boolean (some #{cname} (user/cnames (server/user-by-uid server uid)))))

(defn add-to-channel-and-notify [server uid cname]
  (let [user (server/user-by-uid server uid)
        server' (server/add-user-to-channel server uid cname)]
    (doseq [u (server/users-on-channel server' cname)]
      (notify u server'
              {:message :join
               :source (user/nick user)
               :chan cname}))
    (notify user server' {:message :rpl-name-reply :chan cname})
    (notify user server' {:message :rpl-end-of-names :chan cname})
    server'))

(defn join-channel [server uid cname]
  (let [user (server/user-by-uid server uid)
        error (when (not (valid-chan? cname))
                (notify user server {:message :err-no-such-channel
                                     :chan cname}))]
    (cond error server
          ;; ignore a user's attempt to a join a channel of which they
          ;; are already a part
          (on-channel? server uid cname)
          server

          ;; create a channel if it doesn't exist
          (not (some #{cname} (server/channel-names server)))
          (-> server
              (server/add-channel (->channel cname))
              (add-to-channel-and-notify uid cname))

          :else
          (add-to-channel-and-notify server uid cname))))

(defn leave-channel [server uid cname]
  (let [user (server/user-by-uid server uid)
        server' (server/remove-user-from-channel server uid cname)]
    (doseq [u (server/users-on-channel server cname)]
      (notify u server'
              {:message :part
               :source (user/nick user)
               :chan cname}))
    server'))

(defn leave-channels [server uid]
  (reduce #(leave-channel %1 uid %2) server
          (user/cnames (server/user-by-uid server uid))))

(defn get-target [server target]
  (let [user (server/user-by-nick server target)]
    (cond (not (nil? user))
          user

          (some #{target} (server/channel-names server))
          (server/channel-by-name server target))))

(defmulti process-command
  (fn [server uid command]
    (if (:invalid command)
      (:invalid command)
      (:command command))))

(defmethod process-command :need-more-params
  [server uid command]
  (notify (server/user-by-uid server uid) server
          {:message :err-need-more-params :command (:command command)}))

(defmethod process-command :no-nickname-given
  [server uid command]
  (notify (server/user-by-uid server uid) server
          {:message :err-no-nickname-given}))

(defmethod process-command :no-recipient
  [server uid command]
  (notify (server/user-by-uid server uid) server
          {:message :err-no-recipient :command (:command command)}))

(defmethod process-command :no-text
  [server uid command]
  (notify (server/user-by-uid server uid) server
          {:message :err-no-text-to-send}))

(defmethod process-command :unknown-command
  [server uid command]
  (let [user (server/user-by-uid server uid)
        response (when (and (not= (:command command) :invalid)
                            (user/registered? user))
                   {:message :err-unknown-command :command (:command command)})]
    (if response
      (notify user server response)
      server)))

(defmethod process-command :pass
  [server uid command]
  (let [user (server/user-by-uid server uid)
        error (cond (user/registered? user)
                    {:message :err-already-registered}

                    (not (:password command))
                    {:message :err-need-more-params
                     :command (:command command)})]
    (if error
      (notify user server error)
      server)))

(defmethod process-command :nick
  [server uid command]
  (let [user (server/user-by-uid server uid)
        nick (subs (:nick command) 0 (min (count (:nick command)) max-nick-len))
        error (cond (not (valid-nick? nick))
                    {:message :err-erroneous-nickname :nick nick}

                    (server/user-by-nick server nick)
                    {:message :err-nickname-in-use :nick nick})]
    (cond error
          (notify user server error)

          (not (user/registered? user))
          (set-nick server uid nick)

          :else (change-nick server uid nick))))

(defmethod process-command :user
  [server uid command]
  (let [user (server/user-by-uid server uid)
        error (when (user/registered? user)
                {:message :err-already-registered})]
    (if error
      (notify user server error)
      (set-realname server uid (:realname command)))))

(defmethod process-command :join
  [server uid command]
  (let [user (server/user-by-uid server uid)
        error (when-not (user/registered? user)
                {:message :err-not-registered})]
    (cond error (notify user server error)
          (= (:chan command) "0") (leave-channels server uid)
          :else (join-channel server uid (:chan command)))))

(defmethod process-command :part
  [server uid command]
  (let [user (server/user-by-uid server uid)
        error (cond (not (user/registered? user))
                    {:message :err-not-registered}

                    (not (some #{(:chan command)} (server/channel-names server)))
                    {:message :err-no-such-channel :chan (:chan command)}

                    (not (on-channel? server uid (:chan command)))
                    {:message :err-not-on-channel :chan (:chan command)})]
    (if error
      (notify user server error)
      (leave-channel server uid (:chan command)))))

(defmethod process-command :privmsg
  [server uid command]
  (let [user (server/user-by-uid server uid)
        target (get-target server (:target command))
        error (cond (not (user/registered? user))
                    {:message :err-not-registered}

                    (nil? target)
                    {:message :err-no-such-nick :nick (:target command)}

                    (and (channel? (get-target server (:target command)))
                         (not (on-channel? server uid (:target command))))
                    {:message :err-cannot-send-to-channel
                     :chan (:target command)})]
    (if error
      (notify user server error)
      (notify target server {:message :privmsg
                             :text (:text command)
                             :source (user/nick user)
                             :target (:target command)}))))

(defmethod process-command :quit
  [server uid command]
  (let [user (server/user-by-uid server uid)]
    (when (user/registered? user)
      (doseq [u (conj (server/uids-on-channels-with server uid) uid)]
        (notify (server/user-by-uid server u) server
                {:message :quit
                 :source (user/nick (server/user-by-uid server uid))
                 :text "Client Quit"})))
    (let [source (if (user/registered? user)
                   (server/host server)
                   "127.0.0.1")]
      (notify user server
              {:message :error
               :text (str ":Closing Link: " source " (Client Quit)")}))
    (disconnect server user)
    (server/remove-user server uid)))

(defmethod process-command :disconnect
  [server uid command]
  (let [user (server/user-by-uid server uid)]
    (when (user/registered? user)
      (doseq [u (server/uids-on-channels-with server uid)]
        (notify (server/user-by-uid server u) server
                {:message :quit
                 :source (user/nick (server/user-by-uid server uid))
                 :text "Remote host closed connection"})))
    (server/remove-user server uid)))

(defmethod process-command :default
  [server uid command]
  server)
