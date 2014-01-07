(ns irc.incoming-test
  (:require [midje.sweet :refer :all]
            [irc.incoming.process :as incoming :refer [get-message! ->buffer
                                                       compact!]])
  (:import [java.nio.charset Charset]
           [java.nio ByteBuffer]
           [java.util Arrays]))

;; Populate the test-state with only the keys needed by get-message!

(def test-state
  {:charset  (Charset/forName "US-ASCII")
   :connections {:connection (->buffer)}})

(def buf (incoming/buffer test-state :connection))

(def abc-crlf (byte-array (.getBytes "abc\r\n")))
(def def-crlf (byte-array (.getBytes "def\r\n")))
(def ghi (byte-array (.getBytes "ghi")))
(def jkl-cr (byte-array (.getBytes "jkl\r")))
(def lf (byte-array (.getBytes "\n")))

(def crlf (byte-array (.getBytes "\r\n")))

(def bytes-510 (byte-array 510))
(Arrays/fill bytes-510 (byte 0x21))
(def str-510 (clojure.string/join (conj (repeat 510 "!") "")))

(def bytes-512 (byte-array 512))
(Arrays/fill bytes-512 (byte 0x21))

(facts "get-message! 1 message"
  (.clear buf)
  (.put buf abc-crlf)
  (.flip buf)
  (first (get-message! test-state :connection)) => "abc"
  )

(facts "get-message! 2 messages"
  (.clear buf)
  (.put buf abc-crlf)
  (.put buf def-crlf)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)
        [msg2 state2] (get-message! state1 :connection)]
    msg1 => "abc"
    msg2 => "def")
  )

(facts "get-message! message in two parts"
  (.clear buf)
  (.put buf ghi)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)]
    msg1 => nil
    (let [state2 (compact! state1 :connection)]
      (.put buf abc-crlf)
      (.flip buf)
      (let [[msg2 _] (get-message! state2 :connection)]
        msg2 => "ghiabc")))
  )

(facts "get-message! message split between cr and lf"
  (.clear buf)
  (.put buf jkl-cr)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)]
    msg1 => nil
    (let [state1-5 (compact! state1 :connection)]
      (.put buf lf)
      (.flip buf)
      (let [[msg2 _] (get-message! state1-5 :connection)]
        msg2 => "jkl")))
  )

(facts "get-message! message split between cr not followed by lf"
  (.clear buf)
  (.put buf jkl-cr)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)]
    msg1 => nil
    (let [state1-5 (compact! state1 :connection)]
      (.put buf abc-crlf)
      (.flip buf)
      (let [[msg2 _] (get-message! state1-5 :connection)]
        msg2 => "jkl\rabc")))
  )

(facts "get-message! compact"
  (.clear buf)
  (.put buf abc-crlf)
  (.put buf jkl-cr)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)
        [msg2 state2] (get-message! state1 :connection)]
    msg1 => "abc"
    msg2 => nil
    (let [state2-5 (compact! state2 :connection)]
      (.put buf lf)
      (.flip buf)
      (let [[msg3 _] (get-message! state2-5 :connection)]
        msg3 => "jkl")))
  )

(facts "get-message! longest message"
  (.clear buf)
  (.put buf bytes-510)
  (.put buf crlf)
  (.put buf abc-crlf)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)
        [msg2 state2] (get-message! state1 :connection)]
    msg1 => str-510
    msg2 => "abc")
  )

(facts "get-message! discard"
  (.clear buf)
  (.put buf bytes-512)
  (.put buf crlf)
  (.put buf abc-crlf)
  (.flip buf)
  (first (get-message! test-state :connection)) => "abc"
  )

(facts "get-message! full buffer discard"
  (.clear buf)
  (.put buf bytes-512)
  (.put buf bytes-512)
  (.flip buf)
  (let [[msg1 state1] (get-message! test-state :connection)]
    msg1 => nil
    (let [state2 (compact! state1 :connection)]
      (.put buf bytes-512)
      (.put buf crlf)
      (.put buf abc-crlf)
      (.flip buf)
      (first (get-message! state2 :connection)) => "abc"))
  )
