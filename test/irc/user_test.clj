(ns irc.user-test
  (:require [midje.sweet :refer :all]
            [irc.server.user :as user]))

(def default-user (user/->user 99 "IO"))
(def user-one-chan (assoc-in default-user [:cnames] #{"#chan1"}))
(def user-two-chans (assoc-in default-user [:cnames] #{"#chan1" "#chan2"}))

(facts "uid"
  (user/uid default-user) => 99
  )

(facts "io"
  (user/io default-user) => "IO"
  )

(facts "registered?"
  (user/registered? default-user) => false
  )

(facts "nick"
  (user/nick default-user) => nil
  )

(facts "realname"
  (user/realname default-user) => nil
  )

(facts "cnames"
  (user/cnames default-user) => #{}
  (user/cnames user-one-chan) => #{"#chan1"}
  )

(facts "set-registered?"
  (user/registered? (user/set-registered? default-user true)) => true
  )

(facts "set-nick?"
  (user/nick (user/set-nick default-user "user")) => "user"
  )

(facts "set-realname"
  (user/realname (user/set-realname default-user "User")) => "User"
  )

(facts "add-channel"
  (user/add-channel default-user "#chan1") => user-one-chan

  (user/add-channel user-one-chan "#chan2") => user-two-chans
  )

(facts "remove-channel"
  (user/remove-channel user-two-chans "#chan2") => user-one-chan

  (user/remove-channel user-one-chan "#chan1") => default-user
  )
