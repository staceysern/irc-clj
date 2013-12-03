(ns irc.server-test
  (:require [midje.sweet :refer :all]
            [irc.server :refer :all]
            [irc.io :refer :all]
            [irc.channel :refer :all]
            [irc.user :refer :all]
            [clojure.core.async :as async]))

(def host "localhost")
(def version "irc-sds.0.1")
(def create-date "Thu Oct 24 2013 at 7:23:58 EST")

(def io1 (->IOPair (async/chan 5) (async/chan 5)))
(def io2 (->IOPair (async/chan 5) (async/chan 5)))
(def io3 (->IOPair (async/chan 5) (async/chan 5)))
(def io4 (->IOPair (async/chan 5) (async/chan 5)))
(def io5 (->IOPair (async/chan 5) (async/chan 5)))
(def io6 (->IOPair (async/chan 5) (async/chan 5)))
(def io7 (->IOPair (async/chan 5) (async/chan 5)))

(def uid1 1)
(def uid2 2)
(def uid3 3)
(def uid4 4)
(def uid5 5)
(def uid6 6)
(def uid7 7)

(def chan1 (->Channel "#chan1" #{uid1 uid6}))
(def chan2 (->Channel "#chan2" #{uid1 uid2}))
(def chan3 (->Channel "#chan3" #{uid1 uid2 uid3}))
(def chan4 (->Channel "#chan4" #{uid1 uid2 uid3 uid4}))
(def chan5 (->Channel "#chan5" #{}))

(def user1 (->User uid1 io1 true "user1" "User 1"
                   #{"#chan1" "#chan2" "#chan3" "#chan4"}))
(def user2 (->User uid2 io2 true "user2" "User 2" #{"#chan2" "#chan3" "#chan4"}))
(def user3 (->User uid3 io3 true "user3" "User 3" #{"#chan3" "#chan4"}))
(def user4 (->User uid4 io4 true "user4" "User 4" #{"#chan4"}))
(def user5 (->User uid5 io5 true "user5" "User 5" #{}))
(def user6 (->User uid6 io6 true "user6" "User 6" #{"#chan1"}))
(def user7 (->User uid7 io7 false "*" nil #{}))

(def user7-nick (->User uid7 io7 false "user7" nil #{}))
(def user7-realname (->User uid7 io7 false "user7" "User 7" #{}))
(def user7-registered (->User uid7 io7 true "user7" "User 7" #{}))

(def server
  {:host host
   :version version
   :create-date create-date
   :users {uid1 user1 uid2 user2 uid3 user3 uid4 user4
           uid5 user5 uid6 user6}
   :channels {"#chan1" chan1 "#chan2" chan2 "#chan3" chan3
              "#chan4" chan4 "#chan5" chan5}})

(def user3-minus-chan3 (->User uid3 io3 true "user3" "User 3" #{"#chan4"}))
(def chan3-minus-user3 (->Channel "#chan3" #{uid1 uid2}))
(def chan4-minus-user3 (->Channel "#chan4" #{uid1 uid2 uid4}))

(def server-minus-user3
  {:host host
   :version version
   :create-date create-date
   :users {uid1 user1 uid2 user2 uid4 user4
           uid5 user5 uid6 user6}
   :channels {"#chan1" chan1 "#chan2" chan2
              "#chan3" chan3-minus-user3
              "#chan4" chan4-minus-user3
              "#chan5" chan5}})

(def server-minus-user3-on-chan3
  {:host host
   :version version
   :create-date create-date
   :users {uid1 user1 uid2 user2
           uid3 user3-minus-chan3
           uid4 user4 uid5 user5 uid6 user6}
   :channels {"#chan1" chan1 "#chan2" chan2
              "#chan3" chan3-minus-user3
              "#chan4" chan4 "#chan5" chan5}})

(def server-plus-user7
  {:host host
   :version version
   :create-date create-date
   :users {uid1 user1 uid2 user2 uid3 user3 uid4 user4
           uid5 user5 uid6 user6 uid7 user7}
   :channels {"#chan1" chan1 "#chan2" chan2 "#chan3" chan3
              "#chan4" chan4 "#chan5" chan5}})

(def server-minus-chan5
  {:host host
   :version version
   :create-date create-date
   :users {uid1 user1 uid2 user2 uid3 user3 uid4 user4
           uid5 user5 uid6 user6}
   :channels {"#chan1" chan1 "#chan2" chan2 "#chan3" chan3
              "#chan4" chan4}})

(def server-user7
  {:host host
   :version version
   :create-date create-date
   :users {uid7 user7}
   :channels {}})

(def server-user7-nick
  {:host host
   :version version
   :create-date create-date
   :users {uid7 user7-nick}
   :channels {}})

(def server-user7-realname
  {:host host
   :version version
   :create-date create-date
   :users {uid7 user7-realname}
   :channels {}})

(def server-user7-registered
  {:host host
   :version version
   :create-date create-date
   :users {uid7 user7-registered}
   :channels {}})

(facts "server-host"
  (server-host server) => host
  )

(facts "server-version"
  (server-version server) => version
  )

(facts "server-create-date"
  (server-create-date server) => create-date
  )

(facts "uids"
  (uids (make-server)) => ()
  (uids server) => (list uid1 uid2 uid3 uid4 uid5 uid6)
  )

(facts "users"
  (users (make-server)) => ()
  (users server) => (list user1 user2 user3 user4 user5 user6)
  )

(facts "user-by-uid"
  (user-by-uid server uid1) => user1
  (user-by-uid server 99) => nil
  )

(facts "user-by-nick"
  (user-by-nick server "user1") => user1
  (user-by-nick server "user99") => nil
  )

(facts "channel-names"
  (channel-names (make-server)) => ()
  (channel-names server) => '("#chan1" "#chan2" "#chan3" "#chan4" "#chan5")
  )

(facts "channels"
  (channels (make-server)) => ()
  (channels server) => (list chan1 chan2 chan3 chan4 chan5)
  )

(facts "channel-by-name"
  (channel-by-name server "#chan2") => chan2
  (channel-by-name server "#chan99") => nil
  )

(facts "uids-on-channel"
  (uids-on-channel server "#chan3") => (list uid1 uid2 uid3)
  (uids-on-channel server "#chan5") => ()
  )

(facts "users-on-channel"
  (users-on-channel server "#chan3") => (list user1 user2 user3)
  (users-on-channel server "#chan5") => ()
  )

(facts "channels-for-user"
  (channels-for-user server uid2) => (list chan2 chan3 chan4)
  (channels-for-user server uid5) => ()
  )

(facts "uids-on-channels-with"
  (uids-on-channels-with server uid1) => (list uid2 uid3 uid4 uid6)
  (uids-on-channels-with server uid4) => (list uid1 uid2 uid3)
  (uids-on-channels-with server uid6) => (list uid1)
  (uids-on-channels-with server uid5) => ()
  )

(facts "add-user"
  (add-user server user7) => server-plus-user7
  )

(facts "remove-user"
  (remove-user server uid3) => server-minus-user3
  )

(facts "add-channel"
  (add-channel server-minus-chan5 chan5) => server
  )

(facts "remove-channel"
  (remove-channel server "#chan5") => server-minus-chan5
  )

(facts "add-user-to-channel"
  (add-user-to-channel server-minus-user3-on-chan3 uid3 "#chan3") => server
  )

(facts "remove-user-from-channel"
  (remove-user-from-channel server uid3 "#chan3") => server-minus-user3-on-chan3
  )

(facts "update-user"
  (update-user server-user7 uid7 user-set-nick "user7") => server-user7-nick

  (update-user server-user7-nick uid7 user-set-realname "User 7")
  => server-user7-realname

  (update-user server-user7-realname uid7 user-set-registered? true)
  => server-user7-registered
  )
