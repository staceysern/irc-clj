(ns irc.send
  (:import [irc.user.User]
           [irc.channel.Channel])
  (:require [irc.channel :as channel]
            [irc.core :refer :all]
            [irc.server :as server :refer [users-on-channel user-by-uid]]
            [irc.user :as user]
            [clojure.core.async :as async :refer [>!! <!!]]
            [clojure.string :refer [join]]))

(def numeric
  {:rpl-welcome  "001"
   :rpl-yourhost  "002"
   :rpl-created  "003"
   :rpl-myinfo  "004"
   :rpl-no-topic  "331"
   :rpl-name-reply  "353"
   :rpl-end-of-names  "366"
   :err-no-such-nick  "401"
   :err-no-such-channel  "403"
   :err-cannot-send-to-channel  "404"
   :err-no-recipient  "411"
   :err-no-text-to-send  "412"
   :err-unknown-command  "421"
   :err-no-nickname-given  "431"
   :err-erroneous-nickname  "432"
   :err-nickname-in-use  "433"
   :err-not-on-channel  "442"
   :err-not-registered  "451"
   :err-need-more-params  "461"
   :err-already-registered  "462"})

(def command
  {:join  "JOIN"
   :nick  "NICK"
   :part  "PART"
   :pass  "PASS"
   :privmsg  "PRIVMSG"
   :quit  "QUIT"
   :user  "USER"})

(defn nick-string [user]
  (or (user/nick user) "*"))

(defn format-numeric [server user message message-text]
  (str ":" (server/host server) " " (message numeric) " " (nick-string user) " "
       message-text))

(defn format-command-response [server source message message-text]
  (str ":" source " " (message command) " " message-text))

(defmulti construct-message
  (fn  [server user message]
    (:message message)))

(defmethod construct-message :rpl-welcome
  [server user message]
  (format-numeric server user :rpl-welcome
                  (str ":Welcome to the IRC Chat Server " (user/nick user))))

(defmethod construct-message :rpl-yourhost
  [server user message]
  (format-numeric server user :rpl-yourhost
                  (str ":Your host is " (server/host server) ", running version "
                       (server/version server))))

(defmethod construct-message :rpl-created
  [server user message]
  (format-numeric server user :rpl-created
                  (str ":This server was created " (server/create-date server))))

(defmethod construct-message :rpl-myinfo
  [server user message]
  (format-numeric server user :rpl-myinfo
                  (str (server/host server) " " (server/version server))))

(defmethod construct-message :rpl-no-topic
  [server user message]
  (format-numeric server user :rpl-no-topic
                  (str (:chan message) " :No topic is set")))

(defmethod construct-message :rpl-name-reply
  [server user message]
  (->> (:chan message)
       (users-on-channel server)
       (map user/nick)
       (join " ")
       (str "= " (:chan message) " :")
       (format-numeric server user :rpl-name-reply)))

(defmethod construct-message :rpl-end-of-names
  [server user message]
  (format-numeric server user :rpl-end-of-names
                  (str (:chan message) " :End of NAMES list")))

(defmethod construct-message :err-no-such-nick
  [server user message]
  (format-numeric server user :err-no-such-nick
                  (str (:nick message) " :No such nick/channel")))

(defmethod construct-message :err-no-such-channel
  [server user message]
  (format-numeric server user :err-no-such-channel
                  (str (:chan message) " :No such channel")))

(defmethod construct-message :err-cannot-send-to-channel
  [server user message]
  (format-numeric server user :err-cannot-send-to-channel
                  (str (:chan message) " :Cannot send to channel")))

(defmethod construct-message :err-no-recipient
  [server user message]
  (format-numeric server user :err-no-recipient
                  (str ":No recipient given ("
                       ((:command message) command) ")")))

(defmethod construct-message :err-no-text-to-send
  [server user message]
  (format-numeric server user :err-no-text-to-send
                  ":No text to send"))

(defmethod construct-message :err-unknown-command
  [server user message]
  (format-numeric server user :err-unknown-command
                  (str (:command message) " :Unknown command")))

(defmethod construct-message :err-no-nickname-given
  [server user message]
  (format-numeric server user :err-no-nickname-given
                  ":No nickname given"))

(defmethod construct-message :err-erroneous-nickname
  [server user message]
  (format-numeric server user :err-erroneous-nickname
                  (str (:nick message) " :Erroneous nickname")))

(defmethod construct-message :err-nickname-in-use
  [server user message]
  (format-numeric server user :err-nickname-in-use
                  (str (:nick message) " :Nickname is already in use")))

(defmethod construct-message :err-not-on-channel
  [server user message]
  (format-numeric server user :err-not-on-channel
                  (str (:chan message) " :You're not on that channel")))

(defmethod construct-message :err-not-registered
  [server user message]
  (format-numeric server user :err-not-registered
                  ":You have not registered"))

(defmethod construct-message :err-need-more-params
  [server user message]
  (format-numeric server user :err-need-more-params
                  (str ((:command message) command)
                       " :Not enough parameters")))

(defmethod construct-message :err-already-registered
  [server user message]
  (format-numeric server user :err-already-registered
                  ":You may not reregister"))

(defmethod construct-message :nick
  [server user message]
  (format-command-response server
                           (:source message)
                           :nick
                           (:nick message)))

(defmethod construct-message :join
  [server user message]
  (format-command-response server
                           (:source message)
                           :join
                           (:chan message)))

(defmethod construct-message :part
  [server user message]
  (format-command-response server
                           (:source message)
                           :part
                           (:chan message)))

(defmethod construct-message :privmsg
  [server user message]
  (format-command-response server
                           (:source message)
                           :privmsg
                           (str (:target message) " :" (:text message))))

(defmethod construct-message :quit
  [server user message]
  (format-command-response server
                           (:source message)
                           :quit
                           (str ":" (:text message))))

(defn send-message [server user message]
  (let [string (construct-message server user message)]
    (log (format " %2d tx: %s" (user/uid user) string))
    (async/put! (user/io user) string)
    string))

(defprotocol IMessage
  (notify [messageable server message]))

(extend-protocol IMessage
  irc.user.User
  (notify [user server message]
    (send-message server user message)
    server))

(extend-protocol IMessage
  irc.channel.Channel
  (notify [channel server message]
    (doseq [uid (if (not= (:message message) :privmsg)
                  (channel/uids channel)
                  (filter #(not= (user/nick (user-by-uid server %))
                                 (:source message))
                          (channel/uids channel)))]
      (notify (user-by-uid server uid) server message))
    server))
