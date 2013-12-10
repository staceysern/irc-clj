(ns irc.validate
  (:use clojure.pprint)
  (:require [midje.sweet :refer :all]
            [irc.io :refer :all]
            [irc.server :refer :all]
            [irc.user :as user]
            [clojure.core.async :as async]))

(defn drain! [chan]
  (when (async/alt!! chan ([msg] msg)
                     :default nil)
    (recur chan)))

(defn drain-ios! [ios]
  (doseq [io ios]
    (drain! (:in-chan io))
    (drain! (:out-chan io))))

(defn no-messages [users]
  (reduce #(assoc %1 %2 []) {} users))

(defn received-messages? [server messages]
  (and
   (every? (fn [[user messages]]
             (every? (fn [message]
                       (let [io (user/io user)
                             io-message (async/alt!! (:out-chan io) ([msg] msg)
                                                     :default ::failed)]
                         (when (not= io-message message)
                           (println "uid:" (user/uid user) "expected: " message
                                    "actual: " io-message))
                         (= io-message message)))
                     messages))
           messages)
   (every? (fn [user]
             (let [io (user/io user)
                   io-message (async/alt!! (:out-chan io) ([msg] msg)
                                           :default ::failed)]
               (when (not= io-message ::failed)
                 (println "uid:" (user/uid user) "expected: no message"
                          "actual: " io-message))
               (= io-message ::failed)))
           (keys messages))))

(defn validate-state [expected-server messages]
  (fn [actual-server]
    (when (not (= expected-server actual-server))
      (println "Expected server:")
      (pprint expected-server)
      (println "Actual server:")
      (pprint actual-server))
    (and (= expected-server actual-server)
         (received-messages? actual-server messages))))
