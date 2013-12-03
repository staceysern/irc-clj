(ns irc.channel-test
  (:require [midje.sweet :refer :all]
            [irc.channel :refer :all]
            [irc.user :refer :all]))

(def default-channel (make-channel "#chan1"))
(def chan-one-uid (channel-add-user default-channel 99))
(def chan-two-uids (channel-add-user chan-one-uid 98))

(facts "channel?"
       (channel? (make-channel "#chan")) => true
       (channel? (make-user 99 nil)) => false
       )

(facts "channel-cname"
       (channel-cname default-channel) => "#chan1"
       )

(facts "channel-uids"
       (channel-uids default-channel) => #{}
       (channel-uids chan-one-uid) => #{99}
       (channel-uids (channel-add-user chan-one-uid 98)) => #{98 99}
       (channel-uids (channel-remove-user chan-two-uids 99)) => #{98}
       )

(facts "channel-add-user"
       (channel-uids chan-one-uid) => #{99}
       (channel-uids chan-two-uids) => #{98 99}
       )

(facts "channel-remove-user"
       (channel-uids (channel-remove-user chan-two-uids 99)) => #{98}
       (channel-uids (channel-remove-user chan-one-uid 99)) => #{}
       )







