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
    (let [parse-map
          (match [(vec tree)]
            [[[:PREFIX prefix] [:COMMAND command] [:PARAMS & params]]]
            {:prefix prefix :command command :params params}

            [[[:PREFIX prefix] [:COMMAND command]]]
            {:prefix prefix :command command :params []}

            [[[:COMMAND command] [:PARAMS & params]]]
            {:command command :params params}

            [[[:COMMAND command]]]
            {:command command :params []})]
      (update-in parse-map [:command] keyword))))

(defmulti make-command
  (fn [parse-map] (:command parse-map)))

(defmacro defcommand
  "Defines a make-command method for the given command.

  The body of a defcommand is similar to that of a match.
  The match condition are matched against the parse-map params,
  and the consequence is expected to return a hash map that
  will be merged with {:command command}.

  The following are equivalent:

      (defcommand :foo
        [a b] {:a a :b b}
        [a]   {:a a})

      (defmethod make-command :foo
        [{:keys [params]}]
        (let [result (match params
                       [a b] {:a a :b b}
                       [a]   {:a a}
                       :else {})]
          (assoc result :command :foo)))

  Note the implicit :else clause."
  [command & clauses]
  `(defmethod make-command ~command
     [{params# :params}]
     (let [result# (match params#
                     ~@clauses
                     :else {})]
       (assoc result# :command ~command))))

(defcommand :invalid
  _ {:invalid :unknown-command})

(defcommand :pass
  [password & _] {:password password})

(defcommand :nick
  [nick & _] {:nick nick}
  [] {:invalid :no-nickname-given})

(defcommand :user
  [user mode _ realname & _] {:user user :mode mode :realname realname}
  [& _] {:invalid :need-more-params})

(defcommand :quit)

(defcommand :join
  [chan & _] {:chan chan}
  [] {:invalid :need-more-params})

(defcommand :part
  [chan & _] {:chan chan}
  [] {:invalid :need-more-params})

(defcommand :topic
  [chan topic & _] {:chan chan :topic topic}
  [chan] {:chan chan}
  [] {:invalid :need-more-params})

(defcommand :names
  [chan & _] {:chan chan})

(defcommand :list
  [chan & _] {:chan chan})

(defcommand :kick
  [chan user & _] {:chan chan :user user}
  [& _] {:invalid :need-more-params})

(defcommand :privmsg
  [target text & _] {:target target :text text}
  [target] {:invalid :no-text}
  [] {:invalid :no-recipient})

(defcommand :ison
  [nick & _] {:nick nick}
  [] {:invalid :need-more-params})

(defn parse [s]
  (make-command (gen-parse-map (gen-parse-list s))))
