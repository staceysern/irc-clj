(ns irc.receive-test
  (:require [midje.sweet :refer :all]
            [irc.receive :refer :all]
            [irc.io :refer :all]
            [irc.user :as user :refer [->User]]
            [irc.channel :as channel :refer [->Channel]]
            [irc.validate :refer :all]
            [clojure.core.async :as async]))

(def io1 (->IOPair (async/chan) (async/chan 10)))
(def io2 (->IOPair (async/chan) (async/chan 10)))
(def io3 (->IOPair (async/chan) (async/chan 10)))
(def io4 (->IOPair (async/chan) (async/chan 10)))

(def uid1 1)
(def uid2 2)
(def uid3 3)
(def uid4 4)

(def chan1 (->Channel "#chan1" #{uid1}))
(def chan2 (->Channel "#chan2" #{uid1 uid2}))
(def chan3 (->Channel "#chan3" #{uid1 uid2 uid3}))
(def chan4 (->Channel "#chan4" #{uid3 uid4}))

(def user1 (->User uid1 io1 true "user1" "User 1" #{"#chan1" "#chan2" "#chan3"}))
(def user2 (->User uid2 io2 true "user2" "User 2" #{"#chan2" "#chan3"}))
(def user3 (->User uid3 io3 true "user3" "User 3" #{"#chan3" "#chan4"}))
(def user4 (->User uid4 io4 true "user4" "User 4" #{"#chan4"}))

(def server {:host "localhost"
             :version "irc-sds.0.1"
             :create-date "Thu Oct 24 2013 at 7:23:58 EST"
             :users {uid1 user1 uid2 user2 uid3 user3 uid4 user4}
             :channels {"#chan1" chan1 "#chan2" chan2 "#chan3" chan3
                        "#chan4" chan4}})

(def user2-changed (->User uid2 io2 true "user0000" "User 2"
                           #{"#chan2" "#chan3"}))
(def server-nick-changed (assoc-in server [:users uid2] user2-changed))

(def chan5 (->Channel "#chan5" #{}))
(def chan5-1user (->Channel "#chan5" #{uid1}))
(def chan5-2users (->Channel "#chan5" #{uid1 uid2}))

(def user1-chan5 (->User uid1 io1 true "user1" "User 1"
                         #{"#chan1" "#chan2" "#chan3" "#chan5"}))
(def user2-chan5 (->User uid2 io2 true "user2" "User 2"
                         #{ "#chan2" "#chan3" "#chan5"}))

(def server-chan5 (assoc-in server [:channels "#chan5"] chan5))
(def server-chan5-1user (assoc-in (assoc-in server [:users uid1] user1-chan5)
                                  [:channels]
                                  (assoc (:channels server) "#chan5"
                                         chan5-1user)))
(def server-chan5-2users (assoc-in (assoc-in server-chan5-1user [:users uid2]
                                             user2-chan5)
                                   [:channels]
                                   (assoc (:channels server) "#chan5"
                                          chan5-2users)))

(def user1-part (->User uid1 io1 true "user1" "User 1" #{}))
(def chan1-part (->Channel "#chan1" #{}))
(def chan2-part (->Channel "#chan2" #{uid2}))
(def chan3-part (->Channel "#chan3" #{uid2 uid3}))
(def server-part {:host "localhost"
                  :version "irc-sds.0.1"
                  :create-date "Thu Oct 24 2013 at 7:23:58 EST"
                  :users {uid1 user1-part uid2 user2 uid3 user3 uid4 user4}
                  :channels {"#chan1" chan1-part "#chan2" chan2-part
                             "#chan3" chan3-part "#chan4" chan4}})

(def server-quit {:host "localhost"
                  :version "irc-sds.0.1"
                  :create-date "Thu Oct 24 2013 at 7:23:58 EST"
                  :users {uid2 user2 uid3 user3 uid4 user4}
                  :channels {"#chan1" chan1-part "#chan2" chan2-part
                             "#chan3" chan3-part "#chan4" chan4}})

(def io (->IOPair (async/chan) (async/chan 1000)))
(def uid 99)

(def user (->User uid io true "user9999" "User 9999" #{}))
(def usr-none (->User uid io false nil nil #{}))
(def usr-nick (->User uid io false "user9999" nil #{}))
(def usr-realname (->User uid io false nil "User 9999" #{}))
(def usr-both (->User uid io false "user9999" "User 9999" #{}))
(def usr-registered (->User uid io true "user9999" "User 9999" #{}))
(def usr-changed (->User uid io true "user0000" "User 9999" #{}))

(def server-none (assoc-in server [:users uid] usr-none))
(def server-nick (assoc-in server [:users uid] usr-nick))
(def server-realname (assoc-in server [:users uid] usr-realname))
(def server-both (assoc-in server [:users uid] usr-both))
(def server-registered (assoc-in server [:users uid] usr-registered))
(def server-changed (assoc-in server [:users uid] usr-changed))

(defn drain-all! []
  (drain-ios! [io1 io2 io3 io4]))

(defn welcome-messages [user]
  (let [nick (user/nick user)]
    {user [(str ":localhost 001 " nick
                " :Welcome to the IRC Chat Server " nick)
           (str ":localhost 002 " nick " :Your host is localhost, "
                "running version irc-sds.0.1")
           (str ":localhost 003 " nick " :This server was created "
                "Thu Oct 24 2013 at 7:23:58 EST")
           (str ":localhost 004 " nick " localhost irc-sds.0.1")]}))

(facts "valid-nick?"
  (valid-nick? "s") => true

  (let [letters (concat (map char (range 97 123)) (map char (range 65 91)))
        special (concat (map char (range 0x5b 0x61))
                        (map char (range 0x7b 0x7e)))
        non-initial (concat (map str (range 10)) ["-"])]

    (doseq [c (concat letters special)]
      (valid-nick? (str c "a1[")) => true)

    (doseq [c (concat special non-initial)]
      (valid-nick? (str "a" c)) => true)

    (doseq [c non-initial]
      (valid-nick? (str c "a")) => false))

  (valid-nick? (clojure.string/join (repeat 8 "A"))) => true
  (valid-nick? (clojure.string/join (repeat 9 "A"))) => false
  )

(facts "valid-chan?"
  (valid-chan? "&chan") => true
  (valid-chan? "#chan") => true
  (valid-chan? "!chan") => false
  (valid-chan? "+chan") => false
  (valid-chan? "chan") => false
  (valid-chan? "#") => false
  (valid-chan? "&") => false
  (valid-chan? "+") => false
  (valid-chan? "!") => false

  (valid-chan? (clojure.string/join (conj (repeat 48 "A") "&"))) => true
  (valid-chan? (clojure.string/join (conj (repeat 51 "A") "&"))) => false

  (valid-chan? "&abc def") => false
  (valid-chan? "&abc,def") => false
  (valid-chan? "&abc:def") => false
  (valid-chan? "&abc\u0007def") => false
  )

(facts "on-channel?"
  (on-channel? server uid3 "#chan3") => true
  (on-channel? server uid3 "#chan1") => false
  )

(facts "get-target"
  (get-target server "user3") => user3
  (get-target server "#chan3") => chan3
  (get-target server "user17") => nil
  (get-target server "#chan17") => nil
  )

(facts "register"
  (drain-all!)
  (register server-both uid)
  => (validate-state server-registered
                     (merge (welcome-messages user)
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "set-nick"
  (drain-all!)
  (set-nick server-none uid "user9999")
  => (validate-state server-nick
                     (no-messages [user1 user2 user3 user4 user]))

  (drain-all!)
  (set-nick server-realname uid "user9999")
  => (validate-state server-registered
                     (merge (welcome-messages user)
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "set-realname"
  (drain-all!)
  (set-realname server-none uid "User 9999")
  => (validate-state server-realname
                     (no-messages [user1 user2 user3 user4 user]))

  (drain-all!)
  (set-realname server-nick uid "User 9999")
  => (validate-state server-registered
                     (merge (welcome-messages user)
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "change-nick"
  (drain-all!)
  (change-nick server-registered uid "user0000")
  => (validate-state server-changed
                     (merge {user [":user9999 NICK user0000"]}
                            (no-messages [user1 user2 user3 user4])))

  (drain-all!)
  (change-nick server uid2 "user0000")
  => (validate-state server-nick-changed
                     {user1 [":user2 NICK user0000"]
                      user2 [":user2 NICK user0000"]
                      user3 [":user2 NICK user0000"]
                      user4 []})
  )

(facts "process-command :need-more-params"
  ;; user
  (drain-all!)
  (process-command server uid1 {:invalid :need-more-params :command :user})
  => (validate-state server
                     (merge {user1 [(str ":localhost 461 user1 "
                                         "USER :Not enough parameters")]}
                            (no-messages [user2 user3 user4])))

  ;; join
  (drain-all!)
  (process-command server uid1 {:invalid :need-more-params :command :join})
  => (validate-state server
                     (merge {user1 [(str ":localhost 461 user1 "
                                         "JOIN :Not enough parameters")]}
                            (no-messages [user2 user3 user4])))

  ;; part
  (drain-all!)
  (process-command server uid1 {:invalid :need-more-params :command :part})
  => (validate-state server
                     (merge {user1 [(str ":localhost 461 user1 "
                                         "PART :Not enough parameters")]}
                            (no-messages [user2 user3 user4])))
  )

(facts "process-command :no-nickname-given"
  (drain-all!)
  (process-command server uid1 {:invalid :no-nickname-given :command :nick})
  => (validate-state server
                     (merge {user1 [(str ":localhost 431 user1 "
                                         ":No nickname given")]}
                            (no-messages [user2 user3 user4])))
  )


(facts "process-command :unknown-command"
  (drain-all!)
  (process-command server uid1 {:invalid :unknown-command :command :invalid})
  => (validate-state server (no-messages [user1 user2 user3 user4]))
  )

(facts "process-command :no-recipient"
  ;; privmsg - no target or text
  (drain-all!)
  (process-command server uid1 {:invalid :no-recipient :command :privmsg})
  => (validate-state server
                     (merge {user1 [(str ":localhost 411 user1 "
                                         ":No recipient given (PRIVMSG)")]}
                            (no-messages [user2 user3 user4]))))

(facts "process-command :no-text"
  ;; privmsg - no text
  (drain-all!)
  (process-command server uid1 {:invalid :no-text :command :privmsg})
  => (validate-state server
                     (merge {user1 [(str ":localhost 412 user1 "
                                         ":No text to send")]}
                            (no-messages [user2 user3 user4]))))

(facts "process-command) :pass (with password)"
  ;; before nick and user
  (drain-all!)
  (process-command server-none uid {:command :pass :password "pwd"})
  => (validate-state server-none (no-messages [user1 user2 user3 user4 user]))

  ;; after nick and before user
  (drain-all!)
  (process-command server-nick uid {:command :pass :password "pwd"})
  => (validate-state server-nick (no-messages [user1 user2 user3 user4 user]))

  ;; after user and before nick
  (drain-all!)
  (process-command server-realname uid {:command :pass :password "pwd"})
  => (validate-state server-realname
                     (no-messages [user1 user2 user3 user4 user]))

  ;; after registration
  (drain-all!)
  (process-command server-registered uid {:command :pass :password "pwd"})
  => (validate-state server-registered
                     (merge {user [(str ":localhost 462 user9999 "
                                        ":You may not reregister")]}
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "process-command :pass (without password)"
  ;; before nick and user
  (drain-all!)
  (process-command server-none uid {:command :pass})
  => (validate-state server-none
                     (merge {user [(str ":localhost 461 * "
                                        "PASS :Not enough parameters")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; after nick and before user
  (drain-all!)
  (process-command server-nick uid {:command :pass})
  => (validate-state server-nick
                     (merge {user [(str ":localhost 461 user9999 "
                                        "PASS :Not enough parameters")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; after user and before nick
  (drain-all!)
  (process-command server-realname uid {:command :pass})
  => (validate-state server-realname
                     (merge {user [(str ":localhost 461 * "
                                        "PASS :Not enough parameters")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; after registration
  (drain-all!)
  (process-command server-registered uid {:command :pass})
  => (validate-state server-registered
                     (merge {user [(str ":localhost 462 user9999 "
                                        ":You may not reregister")]}
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "process-command :nick (before registration)"
  ;; nick before user
  (drain-all!)
  (process-command server-none uid {:command :nick :nick "user9999"})
  => (validate-state server-nick (no-messages [user1 user2 user3 user4 user]))

  ;; nick after user
  (drain-all!)
  (process-command server-realname uid {:command :nick :nick "user9999"})
  => (validate-state server-registered
                     (merge (welcome-messages user)
                            (no-messages [user1 user2 user3 user4])))

  ;; invalid nick
  (drain-all!)
  (process-command server-none uid {:command :nick :nick "-user1"})
  => (validate-state server-none
                     (merge {user [(str ":localhost 432 * -user1 "
                                        ":Erroneous nickname")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; nick already in use
  (drain-all!)
  (process-command server-none uid {:command :nick :nick "user1"})
  => (validate-state server-none
                     (merge {user [(str ":localhost 433 * user1 "
                                        ":Nickname is already in use")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; truncate long nick
  (drain-all!)
  (process-command server-realname uid {:command :nick :nick "user99999"})
  => (validate-state server-registered
                     (merge (welcome-messages user)
                            (no-messages [user1 user2 user3 user4])))

  ;; truncated nick already in use
  (drain-all!)
  (process-command server-none uid {:command :nick :nick "user1"})
  => (validate-state server-none
                     (merge {user [(str ":localhost 433 * user1 "
                                        ":Nickname is already in use")]}
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "process-command :nick (after registration)"
  ;; change nick
  (drain-all!)
  (process-command server uid2 {:command :nick :nick "user0000"})
  => (validate-state server-nick-changed
                     {user1 [":user2 NICK user0000"]
                      user2 [":user2 NICK user0000"]
                      user3 [":user2 NICK user0000"]
                      user4 []})

  ;; invalid nick
  (drain-all!)
  (process-command server uid3 {:command :nick :nick "-user1"})
  => (validate-state server
                     (merge {user3 [(str ":localhost 432 user3 -user1 "
                                         ":Erroneous nickname")]}
                            (no-messages [user1 user2 user4])))

  ;; nick already in use
  (drain-all!)
  (process-command server uid3 {:command :nick :nick "user1"})
  => (validate-state server
                     (merge {user3 [(str ":localhost 433 user3 user1 "
                                         ":Nickname is already in use")]}
                            (no-messages [user1 user2 user4])))

  ;; truncate nick
  (drain-all!)
  (process-command server uid2 {:command :nick :nick "user00000"})
  => (validate-state server-nick-changed
                     {user1 [":user2 NICK user0000"]
                      user2 [":user2 NICK user0000"]
                      user3 [":user2 NICK user0000"]
                      user4 []})

  ;; nick already in use
  (drain-all!)
  (process-command server-nick-changed uid3 {:command :nick :nick "user00000"})
  => (validate-state server-nick-changed
                     (merge {user3 [(str ":localhost 433 user3 user0000 "
                                         ":Nickname is already in use")]}
                            (no-messages [user1 user2 user4])))
  )

(facts "process-command :user (before registration)"
  ;; user before nick
  (drain-all!)
  (process-command server-none uid {:command :user :realname "User 9999"})
  => (validate-state server-realname
                     (no-messages [user1 user2 user3 user4 user]))

  ;; user after nick
  (drain-all!)
  (process-command server-nick uid {:command :user :realname "User 9999"})
  => (validate-state server-registered
                     (merge (welcome-messages user)
                            (no-messages [user1 user2 user3 user4])))
  )

(facts "process-command :user (after registration)"
  (drain-all!)
  (process-command server uid1 {:command :user :realname "User 1"})
  => (validate-state server (merge {user1 [(str ":localhost 462 user1 "
                                                ":You may not reregister")]}
                                   (no-messages [user2 user3 user4])))
  )

(facts "process-command :join"
  ;; user not registered
  (drain-all!)
  (process-command server-none uid {:command :join :chan "#chan1"})
  => (validate-state server-none
                     (merge {user [(str ":localhost 451 * "
                                        ":You have not registered")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; new channel
  (drain-all!)
  (process-command server uid1 {:command :join :chan "#chan5"})
  => (validate-state server-chan5-1user
                     (merge {user1 [":user1 JOIN #chan5"
                                    ":localhost 353 user1 = #chan5 :user1"
                                    (str ":localhost 366 user1 #chan5 "
                                         ":End of NAMES list")]}
                            (no-messages [user2 user3 user4])))

  ;; existing channel
  (drain-all!)
  (process-command server-chan5-1user uid2 {:command :join :chan "#chan5"})
  => (validate-state server-chan5-2users
                     (merge {user1 [":user2 JOIN #chan5"]
                             user2 [":user2 JOIN #chan5"
                                    (str ":localhost 353 user2 = #chan5 "
                                         ":user1 user2")
                                    (str ":localhost 366 user2 #chan5 "
                                         ":End of NAMES list")]}
                            (no-messages [user3 user4])))

  ;; invalid channel
  (drain-all!)
  (process-command server uid1 {:command :join :chan "chan"})
  => (validate-state server
                     (merge {user1 [(str ":localhost 403 user1 "
                                         "chan :No such channel")]}
                            (no-messages [user2 user3 user4])))

  ;; same channel twice
  (drain-all!)
  (process-command (process-command server uid1 {:command :join :chan "#chan5"})
                   uid1
                   {:command :join :chan "#chan5"})
  => (validate-state server-chan5-1user
                     (merge {user1 [":user1 JOIN #chan5"
                                    ":localhost 353 user1 = #chan5 :user1"
                                    (str ":localhost 366 user1 #chan5 "
                                         ":End of NAMES list")]}
                            (no-messages [user2 user3 user4])))
  )

(facts "join 0"
  (drain-all!)
  (process-command server uid1 {:command :join :chan "0"})
  => (validate-state server-part {user1 [":user1 PART #chan1"
                                         ":user1 PART #chan2"
                                         ":user1 PART #chan3"]
                                  user2 [":user1 PART #chan2"
                                         ":user1 PART #chan3"]
                                  user3 [":user1 PART #chan3"]
                                  user4 []})
  )

(facts "process-command :part"
  ;; user not registered
  (drain-all!)
  (process-command server-none uid {:command :part :chan "#chan1"}) =>
  (validate-state server-none
                  (merge {user [(str ":localhost 451 * "
                                     ":You have not registered")]}
                         (no-messages [user1 user2 user3 user4])))

  ;; existing channel
  (drain-all!)
  (process-command server-chan5-2users uid2 {:command :part :chan "#chan5"})
  => (validate-state server-chan5-1user
                     (merge {user1 [":user2 PART #chan5"]
                             user2 [":user2 PART #chan5"]}
                            (no-messages [user3 user4])))

  ;; last user on existing channel
  (drain-all!)
  (process-command server-chan5-1user uid1 {:command :part :chan "#chan5"})
  => (validate-state server-chan5
                     (merge {user1 [":user1 PART #chan5"]}
                            (no-messages [user3 user4])))

  ;; non-existent channel
  (drain-all!)
  (process-command server uid1 {:command :part :chan "#chan6"})
  => (validate-state server
                     (merge {user1 [(str ":localhost 403 user1 #chan6 "
                                         ":No such channel")]}
                            (no-messages [user2 user3 user4])))

  ;; not in channel
  (drain-all!)
  (process-command server uid1 {:command :part :chan "#chan4"})
  => (validate-state server
                     (merge {user1 [(str ":localhost 442 user1 #chan4 "
                                         ":You're not on that channel")]}
                            (no-messages [user2 user3 user4])))
  )

(facts "process-command :privmsg"
  ;; user not registered
  (drain-all!)
  (process-command server-none uid
                   {:command :privmsg :target "#chan1" :text "hello world"})
  => (validate-state server-none
                     (merge {user [(str ":localhost 451 * "
                                        ":You have not registered")]}
                            (no-messages [user1 user2 user3 user4])))

  ;; to user
  (drain-all!)
  (process-command server uid1
                   {:command :privmsg :target "user2" :text "hello world"})
  => (validate-state server
                     (merge {user2 [":user1 PRIVMSG user2 :hello world"]}
                            (no-messages [user1 user3 user4])))

  ;; to channel
  (drain-all!)
  (process-command server uid1
                   {:command :privmsg :target "#chan3" :text "hello world"})
  => (validate-state server
                     (merge {user2 [":user1 PRIVMSG #chan3 :hello world"]
                             user3 [":user1 PRIVMSG #chan3 :hello world"]}
                            (no-messages [user1 user4])))

  ;; from non-channel member
  (drain-all!)
  (process-command server uid4
                   {:command :privmsg :target "#chan3" :text "hello world"})
  => (validate-state server
                     (merge {user4 [(str ":localhost 404 user4 #chan3 "
                                         ":Cannot send to channel")]}
                            (no-messages [user1 user2 user3])))


  ;; non-existent target
  (drain-all!)
  (process-command server uid1
                   {:command :privmsg :target "#chan99" :text "hello world"})
  => (validate-state server
                     (merge {user1 [(str ":localhost 401 user1 "
                                         "#chan99 :No such nick/channel")]}
                            (no-messages [user2 user3 user4])))
  )

(facts ":process-command quit"
  ;; quit
  (drain-all!)
  (process-command server uid1
                   {:command :quit :source "user1" :text "Client Quit"})
  => (validate-state server-quit  {user1 [":user1 QUIT :Client Quit"]
                                   user2 [":user1 QUIT :Client Quit"]
                                   user3 [":user1 QUIT :Client Quit"]
                                   user4 []})
  )
