(ns irc.io-test
  (:require [midje.sweet :refer :all]
            [irc.io :as io :refer [->!io-map]]
            [irc.iopair :refer [->IOPair]]))

(def pair1 (->IOPair :in1 :out1))
(def pair2 (->IOPair :in2 :out2))
(def pair3 (->IOPair :in3 :out3))

(def socket1 :socket1)
(def socket2 :socket2)
(def socket3 :socket3)

(def io-map1 {:sockets->io-pairs {socket1 pair1}
              :io-pairs->sockets {pair1 socket1}})

(def io-map2 {:sockets->io-pairs {socket2 pair2}
              :io-pairs->sockets {pair2 socket2}})

(def io-map12 {:sockets->io-pairs {socket1 pair1 socket2 pair2}
               :io-pairs->sockets {pair1 socket1 pair2 socket2}})

(def io-map12-1closed-in {:sockets->io-pairs {socket2 pair2}
                          :io-pairs->sockets {pair1 socket1 pair2 socket2}})

(def io-map12-1closed-out {:sockets->io-pairs {socket1 pair1 socket2 pair2}
                           :io-pairs->sockets {pair2 socket2}})

(defn ->!io-map12 []
  (let [!io-map (->!io-map)]
    (io/add-io! !io-map socket1 pair1)
    (io/add-io! !io-map socket2 pair2)
    !io-map)
  )

(facts "io/add-io!"
  (let [!io-map (->!io-map)]
    (io/add-io! !io-map socket1 pair1)
    (= @!io-map io-map1) => true
    (io/add-io! !io-map socket2 pair2)
    (= @!io-map io-map12) => true)
  )

(facts "remove-socket->io-pair!"
  (let [!io-map (->!io-map)]
    (io/add-io! !io-map socket1 pair1)
    (io/add-io! !io-map socket2 pair2)
    (io/remove-socket->io-pair! !io-map socket1) => io-map12-1closed-in)
  )

(facts "remove-io-pair->socket!"
  (let [!io-map (->!io-map)]
    (io/add-io! !io-map socket1 pair1)
    (io/add-io! !io-map socket2 pair2)
    (io/remove-io-pair->socket! !io-map pair1) => io-map12-1closed-out)
  )

(facts "socket->io-pair"
  (let [!io-map (->!io-map12)]
    (io/socket->io-pair !io-map :socket1) => pair1
    (io/socket->io-pair !io-map :socket2) => pair2
    (io/socket->io-pair !io-map :socket3) => nil)
  )

(facts "io-pair->socket"
  (let [!io-map (->!io-map12)]
    (io/io-pair->socket !io-map pair1) => :socket1
    (io/io-pair->socket !io-map pair2) => :socket2
    (io/io-pair->socket !io-map pair3) => nil)
  )

(facts "io-pairs"
  (let [!io-map (->!io-map12)]
    (set (io/io-pairs !io-map)) => #{pair1 pair2})
  )

(facts "sockets"
  (let [!io-map (->!io-map12)]
    (set (io/sockets !io-map)) => #{:socket1 :socket2}))

(facts "out-chans"
  (let [!io-map (->!io-map12)]
    (set (io/out-chans !io-map)) => #{:out1 :out2}))

(facts "out-chan->socket"
  (let [!io-map (->!io-map12)]
    (io/out-chan->socket !io-map :out1) => socket1
    (io/out-chan->socket !io-map :out2) => socket2
    (io/out-chan->socket !io-map :out3) => nil)
  )
