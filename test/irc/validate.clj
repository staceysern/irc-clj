(ns irc.validate
  (:use clojure.pprint)
  (:require [midje.sweet :refer :all]
            [irc.server.user :as user]
            [clojure.core.async :as async]
            [clojure.set :refer [difference]]))

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

(defn received-messages? [server messages closed]
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
                 (if (nil? io-message)
                   (println "uid:" (user/uid user)
                            "channel should not be closed")
                   (println "uid:" (user/uid user) "expected: no message"
                            "actual: " io-message)))
               (= io-message ::failed)))
           (difference (set (keys messages)) (set closed)))
   (every? (fn [user]
              (let [io (user/io user)
                    io-message (async/alt!! (:out-chan io) ([msg] msg)
                                            :default ::failed)]
                (when io-message
                  (println "uid:" (user/uid user) "channel should be closed"))
                (not io-message)))
           closed)))

(defn validate-state
  ([expected-server messages]
     (fn [actual-server]
       (validate-state expected-server messages [])))
  ([expected-server messages closed]
     (fn [actual-server]
       (when (not (= expected-server actual-server))
         (println "Expected server:")
         (pprint expected-server)
         (println "Actual server:")
         (pprint actual-server))
       (and (= expected-server actual-server)
            (received-messages? actual-server messages closed)))))
