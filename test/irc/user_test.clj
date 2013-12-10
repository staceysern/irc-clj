(ns irc.user-test
  (:require [midje.sweet :refer :all]
            [irc.user :refer :all]))

(def default-user (->user 99 "IO"))
(def user-one-chan (assoc-in default-user [:cnames] #{"#chan1"}))
(def user-two-chans (assoc-in default-user [:cnames] #{"#chan1" "#chan2"}))

(facts "uid"
  (uid default-user) => 99
  )

(facts "io"
  (io default-user) => "IO"
  )

(facts "registered?"
  (registered? default-user) => false
  )

(facts "nick"
  (nick default-user) => nil
  )

(facts "realname"
  (realname default-user) => nil
  )

(facts "cnames"
  (cnames default-user) => #{}
  (cnames user-one-chan) => #{"#chan1"}
  )

(facts "set-registered?"
  (registered? (set-registered? default-user true)) => true
  )

(facts "set-nick?"
  (nick (set-nick default-user "user")) => "user"
  )

(facts "set-realname"
  (realname (set-realname default-user "User")) => "User"
  )

(facts "add-channel"
  (add-channel default-user "#chan1") => user-one-chan

  (add-channel user-one-chan "#chan2") => user-two-chans
  )

(facts "remove-channel"
  (remove-channel user-two-chans "#chan2") => user-one-chan

  (remove-channel user-one-chan "#chan1") => default-user
  )
