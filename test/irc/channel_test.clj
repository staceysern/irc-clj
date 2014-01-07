(ns irc.channel-test
  (:require [midje.sweet :refer :all]
            [irc.server.channel :as channel]
            [irc.server.user :as user]))

(def default-channel (channel/->channel "#chan1"))
(def chan-one-uid (channel/add-user default-channel 99))
(def chan-two-uids (channel/add-user chan-one-uid 98))

(facts "channel?"
  (channel/channel? (channel/->channel "#chan")) => true
  (channel/channel? (user/->user 99 nil)) => false
  )

(facts "cname"
  (channel/cname default-channel) => "#chan1"
  )

(facts "uids"
  (channel/uids default-channel) => #{}
  (channel/uids chan-one-uid) => #{99}
  (channel/uids (channel/add-user chan-one-uid 98)) => #{98 99}
  (channel/uids (channel/remove-user chan-two-uids 99)) => #{98}
  )

(facts "add-user"
  (channel/uids chan-one-uid) => #{99}
  (channel/uids chan-two-uids) => #{98 99}
  )

(facts "remove-user"
  (channel/uids (channel/remove-user chan-two-uids 99)) => #{98}
  (channel/uids (channel/remove-user chan-one-uid 99)) => #{}
  )
