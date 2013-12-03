(ns irc.parser
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [lower-case]]))

(def gen-parse-tree
  (insta/parser (str "https://gist.github.com/staceysern/7744943/"
                     "raw/41f01dafe20a63afcc227c4b4a9bcba69c67c669/irc-grammar")
                :input-format :abnf))

(defn gen-parse-list [s]
  (insta/transform
   {:NICKNAME str
    :COMMAND (comp (partial vector :COMMAND) lower-case str)
    :MIDDLE str
    :TRAILING str}
   (gen-parse-tree s)))

(defn gen-parse-map
  "Generate a map representation of the parse tree"
  [tree]
  (if (insta/failure? tree)
    {:command :invalid}
    (match [(vec tree)]
      [[[:PREFIX prefix] [:COMMAND command] [:PARAMS & params]]]
      {:prefix prefix :command command :params params}

      [[[:PREFIX prefix] [:COMMAND command]]]
      {:prefix prefix :command command}

      [[[:COMMAND command] [:PARAMS & params]]]
      {:command command :params params}

      [[[:COMMAND command]]]
      {:command command})))

(defmulti make-command
  (fn [parse-map] (:command parse-map)))

(defmethod make-command :invalid
  [parse-map]
  {:command :invalid
   :invalid :unknown-command})

(defmethod make-command "pass"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :pass}]
    (if params
      (assoc command :password (first params))
      command)))

(defmethod make-command "nick"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :nick}]
    (if params
      (assoc command :nick (first params))
      (assoc command :invalid :no-nickname-given))))

(defmethod make-command "user"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :user}]
    (if (and params (>= (count params) 4))
      (assoc command :user (nth params 0)
             :mode (nth params 1)
             :realname (nth params 3))
      (assoc command :invalid :need-more-params))))

(defmethod make-command "quit"
  [parse-map]
  {:command :quit})

(defmethod make-command "join"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :join}]
    (if params
      (assoc command :chan (first params))
      (assoc command :invalid :need-more-params))))

(defmethod make-command "part"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :part}]
    (if params
      (assoc command :chan (first params))
      (assoc command :invalid :need-more-params))))

(defmethod make-command "topic"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :topic}]
    (if params
      (if (= (count params) 1)
        (assoc command :chan (first params))
        (assoc command :chan (first params)
               :topic (nth params 1)))
      (assoc command :invalid :need-more-params))))

(defmethod make-command "names"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :names}]
    (if params
      (assoc command :chan (first params))
      command)))

(defmethod make-command "list"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :list}]
    (if params
      (assoc command :chan (first params))
      command)))

(defmethod make-command "kick"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :kick}]
    (if (or (not params) (< (count params) 2))
      (assoc command :invalid :need-more-params)
      (assoc command :chan (first params) :user (nth params 1)))))

(defmethod make-command "privmsg"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :privmsg}]
    (cond (not params) (assoc command :invalid :no-recipient)
          (= (count params) 1) (assoc command :invalid :no-text)
          :else (assoc command :target (first params) :text (nth params 1)))))

(defmethod make-command "ison"
  [parse-map]
  (let [params (:params parse-map)
        command {:command :ison}]
    (if params
      (assoc command :nick (first params))
      (assoc command :invalid :need-more-params))))

(defn parse [s]
  (make-command (gen-parse-map (gen-parse-list s))))
