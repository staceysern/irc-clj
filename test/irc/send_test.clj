(ns irc.send-test
  (:require [midje.sweet :refer :all]
            [irc.send :as send]
            [irc.io :refer [->IOPair]]
            [irc.channel :refer [->Channel]]
            [irc.user :refer [->User]]
            [irc.validate :refer [validate-state drain-ios! no-messages]]
            [clojure.core.async :as async]))

(def io1 (->IOPair (async/chan) (async/chan 5)))
(def io2 (->IOPair (async/chan) (async/chan 5)))
(def io3 (->IOPair (async/chan) (async/chan 5)))
(def io4 (->IOPair (async/chan) (async/chan 5)))

(def uid1 1)
(def uid2 2)
(def uid3 3)
(def uid4 4)

(def chan1 (->Channel "#chan1" #{uid1}))
(def chan2 (->Channel "#chan2" #{uid1 uid2}))
(def chan3 (->Channel "#chan3" #{uid1 uid2 uid3}))
(def chan4 (->Channel "#chan4" #{uid1 uid2 uid3 uid4}))

(def user1 (->User uid1 io1 true "user1" "User 1"
                   #{"#chan1" "#chan2" "#chan3" "#chan4"}))
(def user2 (->User uid2 io2 true "user2" "User 2" #{"#chan2" "#chan3" "#chan4"}))
(def user3 (->User uid3 io3 true "user3" "User 3" #{"#chan3" "#chan4"}))
(def user4 (->User uid4 io4 true "user4" "User 4" #{"#chan4"}))

(def server {:host "localhost"
             :version "irc-sds.0.1"
             :create-date "Thu Oct 24 2013 at 7:23:58 EST"
             :users {uid1 user1 uid2 user2 uid3 user3 uid4 user4}
             :channels {"&chan1" chan1 "&chan2" chan2 "&chan3" chan3
                        "&chan4" chan4}})

(defn drain-all! []
  (drain-ios! [io1 io2 io3 io4]))

(facts "construct-message :rpl-welcome"
  (send/construct-message server user1 {:message :rpl-welcome})
  => ":localhost 001 user1 :Welcome to the IRC Chat Server user1"
  )

(facts "construct-message :rpl-yourhost"
  (send/construct-message server user1 {:message :rpl-yourhost})
  => (str ":localhost 002 user1 :Your host is localhost, "
          "running version irc-sds.0.1")
  )

(facts "construct-message :rpl-created"
  (send/construct-message server user1 {:message :rpl-created})
  => (str ":localhost 003 user1 :This server was created "
          "Thu Oct 24 2013 at 7:23:58 EST")
  )

(facts "construct-message :rpl-myinfo"
  (send/construct-message server user1 {:message :rpl-myinfo})
  => ":localhost 004 user1 localhost irc-sds.0.1"
  )

(facts "construct-message :rpl-no-topic"
  (send/construct-message server user1
                          {:message :rpl-no-topic :chan "&chan1"})
  => ":localhost 331 user1 &chan1 :No topic is set"
  )

(facts "construct-message :rpl-name-reply"
  (send/construct-message server user1
                          {:message :rpl-name-reply :chan "&chan1"})
  => ":localhost 353 user1 = &chan1 :user1"

  (send/construct-message server user1
                          {:message :rpl-name-reply :chan "&chan4"})
  => ":localhost 353 user1 = &chan4 :user1 user2 user3 user4"
  )

(facts "construct-message :rpl-end-of-names"
  (send/construct-message server user1
                          {:message :rpl-end-of-names :chan "&chan1"})
  => ":localhost 366 user1 &chan1 :End of NAMES list")

(facts "construct-message :err-no-such-nick"
  (send/construct-message server user1
                          {:message :err-no-such-nick :nick "user0"})
  => ":localhost 401 user1 user0 :No such nick/channel"
  )

(facts "construct-message :err-no-such-channel"
  (send/construct-message server user1
                          {:message :err-no-such-channel :chan "#chan0"})
  => ":localhost 403 user1 #chan0 :No such channel"
  )

(facts "construct-message :err-no-recipient"
  (send/construct-message server user1
                          {:message :err-no-recipient :command :privmsg})
  => ":localhost 411 user1 :No recipient given (PRIVMSG)"
  )

(facts "construct-message :err-no-text-to-send"
  (send/construct-message server user1
                          {:message :err-no-text-to-send})
  => ":localhost 412 user1 :No text to send"
  )

(facts "construct-message :err-unknown-command"
  (send/construct-message server user1
                          {:message :err-unknown-command :command "cmd1234"})
  => ":localhost 421 user1 cmd1234 :Unknown command"
  )

(facts "construct-message :err-no-nickname-given"
  (send/construct-message server user1
                          {:message :err-no-nickname-given})
  => ":localhost 431 user1 :No nickname given"
  )

(facts "construct-message :err-erroneous-nickname"
  (send/construct-message server user1
                          {:message :err-erroneous-nickname :nick "1234"})
  => ":localhost 432 user1 1234 :Erroneous nickname"
  )

(facts "construct-message :err-nickname-in-use"
  (send/construct-message server user1
                          {:message :err-nickname-in-use :nick "user2"})
  => ":localhost 433 user1 user2 :Nickname is already in use"
  )

(facts "construct-message :err-not-on-channel"
  (send/construct-message server user1
                          {:message :err-not-on-channel :chan "&chan"})
  => ":localhost 442 user1 &chan :You're not on that channel"
  )

(facts "construct-message :err-not-registered"
  (send/construct-message server user1 {:message :err-not-registered})
  => ":localhost 451 user1 :You have not registered"
  )

(facts "construct-message :err-need-more-params"
  (send/construct-message server user1
                          {:message :err-need-more-params :command :join})
  => ":localhost 461 user1 JOIN :Not enough parameters"

  (send/construct-message server user1
                          {:message :err-need-more-params :command :user})
  => ":localhost 461 user1 USER :Not enough parameters"

  (send/construct-message server user1
                          {:message :err-need-more-params :command :part})
  => ":localhost 461 user1 PART :Not enough parameters"
  )

(facts "construct-message :err-already-registered"
  (send/construct-message server user1 {:message :err-already-registered})
  => ":localhost 462 user1 :You may not reregister"
  )

(facts "construct-message :nick"
  (send/construct-message server user1
                          {:message :nick :source "user3" :nick "user99"})
  => ":user3 NICK user99"
  )

(facts "construct-message :join"
  (send/construct-message server user1
                          {:message :join :source "user3" :chan "#chan3"})
  => ":user3 JOIN #chan3"
  )

(facts "construct-message :part"
  (send/construct-message server user1
                          {:message :part :source "user3" :chan "#chan3"})
  => ":user3 PART #chan3"
  )

(facts "construct-message :privmsg"
  (send/construct-message server user1 {:message :privmsg :source "user3"
                                        :target "#chan3" :text "hello world"})
  => ":user3 PRIVMSG #chan3 :hello world"

  (send/construct-message server user1 {:message :privmsg :source "user3"
                                        :target "user1" :text "hello world"})
  => ":user3 PRIVMSG user1 :hello world"
  )

(facts "construct-message :quit"
  (send/construct-message server user1
                          {:message :quit :source "user3" :text "Client Quit"})
  => ":user3 QUIT :Client Quit"
  )

(facts "notify"
  (drain-all!)
  (send/notify user1 server {:message :err-not-registered})
  => (validate-state server
                     (merge {user1 [(str ":localhost 451 user1 :You have "
                                         "not registered")]}
                            (no-messages [user2 user3 user4])))

  (drain-all!)
  (send/notify chan3 server {:message :nick :source "user1" :nick "user99"})
  => (validate-state server
                     {user1 [":user1 NICK user99"]
                      user2 [":user1 NICK user99"]
                      user3 [":user1 NICK user99"]
                      user4 []})

  (drain-all!)
  (send/notify chan3 server {:message :privmsg :source "user1"
                             :text "hello world" :target "#chan3"})
  => (validate-state server
                     {user1 []
                      user2 [":user1 PRIVMSG #chan3 :hello world"]
                      user3 [":user1 PRIVMSG #chan3 :hello world"]
                      user4 []})
  )
