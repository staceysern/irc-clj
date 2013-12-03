(ns irc.user-test
  (:require [midje.sweet :refer :all]
            [irc.user :refer :all]))

(def default-user (make-user 99 "IO"))
(def user-one-chan (assoc-in default-user [:cnames] #{"#chan1"}))
(def user-two-chans (assoc-in default-user [:cnames] #{"#chan1" "#chan2"}))

(facts "user-uid"
       (user-uid default-user) => 99
       )

(facts "user-io"
       (user-io default-user) => "IO"
       )

(facts "user-registered?"
       (user-registered? default-user) => false
       )

(facts "user-nick"
       (user-nick default-user) => nil
       )

(facts "user-realname"
       (user-realname default-user) => nil
       )

(facts "user-cnames"
       (user-cnames default-user) => #{}
       (user-cnames user-one-chan) => #{"#chan1"}
       )

(facts "user-set-registered?"
       (user-registered? (user-set-registered? default-user true)) => true
       )

(facts "user-set-nick?"
       (user-nick (user-set-nick default-user "user")) => "user"
       )

(facts "user-set-realname"
        (user-realname (user-set-realname default-user "User")) => "User"
        )

(facts "user-add-channel"
       (user-add-channel default-user "#chan1") =>
       user-one-chan

       (user-add-channel user-one-chan "#chan2") =>
       user-two-chans
       )

(facts "user-remove-channel"
       (user-remove-channel user-two-chans "#chan2") =>
       user-one-chan

       (user-remove-channel user-one-chan "#chan1") =>
       default-user
       )
