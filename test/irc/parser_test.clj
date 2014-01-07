(ns irc.parser-test
  (:require [midje.sweet :refer :all]
            [irc.server.parser :refer [gen-parse-list gen-parse-map
                                       make-command parse]]
            [instaparse.core :as insta]))

(facts "message"
  (gen-parse-list ":prefix command")
  => [[:PREFIX "prefix"] [:COMMAND "command"]]

  (gen-parse-list ":prefix command params")
  => [[:PREFIX "prefix"] [:COMMAND "command"] [:PARAMS "params"]]

  (gen-parse-list "command")
  => [[:COMMAND "command"]]

  (gen-parse-list "command params")
  => [[:COMMAND "command"] [:PARAMS "params"]]

  (insta/failure? (gen-parse-list ":prefix ")) => true
  (insta/failure? (gen-parse-list ":prefix")) => true
  (insta/failure? (gen-parse-list ":prefix :prefix")) => true
  )

(facts "nickname"
  (gen-parse-list ":p command") => [[:PREFIX "p"] [:COMMAND "command"]]
  (gen-parse-list ":[ command") => [[:PREFIX "["] [:COMMAND "command"]]
  (gen-parse-list ":] command") => [[:PREFIX "]"] [:COMMAND "command"]]
  (gen-parse-list ":` command") => [[:PREFIX "`"] [:COMMAND "command"]]
  (gen-parse-list ":_ command") => [[:PREFIX "_"] [:COMMAND "command"]]
  (gen-parse-list ":^ command") => [[:PREFIX "^"] [:COMMAND "command"]]
  (gen-parse-list ":{ command") => [[:PREFIX "{"] [:COMMAND "command"]]
  (gen-parse-list ":| command") => [[:PREFIX "|"] [:COMMAND "command"]]
  (gen-parse-list ":} command") => [[:PREFIX "}"] [:COMMAND "command"]]
  (gen-parse-list ":~ command") => [[:PREFIX "~"] [:COMMAND "command"]]
  (gen-parse-list ":\\ command") => [[:PREFIX "\\"] [:COMMAND "command"]]

  (gen-parse-list ":pa9]b0|89]- command")
  => [[:PREFIX "pa9]b0|89]-"] [:COMMAND "command"]]

  (gen-parse-list ":pa9]b0|89zyx]- command")
  => [[:PREFIX "pa9]b0|89zyx]-"] [:COMMAND "command"]]

  (insta/failure? (gen-parse-list ":0 command")) => true
  (insta/failure? (gen-parse-list ":- command")) => true
  (insta/failure? (gen-parse-list ":* command")) => true
  (insta/failure? (gen-parse-list ":a* command")) => true
  )

(facts "command"
  (gen-parse-list "command") => [[:COMMAND "command"]]
  (gen-parse-list "001") => [[:COMMAND "001"]]

  (insta/failure? (gen-parse-list "c123")) => true
  (insta/failure? (gen-parse-list "0ab")) => true
  (insta/failure? (gen-parse-list "0123")) => true
  (insta/failure? (gen-parse-list "c[")) => true
  (insta/failure? (gen-parse-list "c-")) => true
  (insta/failure? (gen-parse-list "c:")) => true
  )

(facts "params"
  (gen-parse-list "command arg1 arg2 :trailing")
  => [[:COMMAND "command"] [:PARAMS "arg1" "arg2" "trailing"]]

  (gen-parse-list "command arg1 arg2 :trailing arg   ")
  => [[:COMMAND "command"] [:PARAMS "arg1" "arg2" "trailing arg   "]]

  (gen-parse-list (str "command arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 "
                       "arg10 arg11 arg12 arg13 arg14 :trailing"))
  => [[:COMMAND "command"] [:PARAMS "arg1" "arg2" "arg3" "arg4" "arg5" "arg6"
                            "arg7" "arg8" "arg9" "arg10" "arg11" "arg12"
                            "arg13" "arg14" "trailing"]]

  (gen-parse-list (str "command arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 "
                       "arg10 arg11 arg12 arg13 arg14 arg15"))
  => [[:COMMAND "command"] [:PARAMS "arg1" "arg2" "arg3" "arg4" "arg5" "arg6"
                            "arg7" "arg8" "arg9" "arg10" "arg11" "arg12"
                            "arg13" "arg14" "arg15"]]

  (insta/failure? (gen-parse-list (str "command arg1 arg2 arg3 arg4 arg5 arg6 "
                                       "arg7 arg8 arg9 arg10 arg11 arg12 arg13 "
                                       "arg14 arg15 :trailing arg")))
  => true

  (insta/failure? (gen-parse-list (str "command arg1 arg2 arg3 arg4 arg5 arg6 "
                                       "arg7 arg8 arg9 arg10 arg11 arg12 arg13 "
                                       "arg14 arg15 arg16")))
  => true)


(facts "spaces"
  (gen-parse-list "   :prefix   command   params   ")
  => [[:PREFIX "prefix"] [:COMMAND "command"] [:PARAMS "params"]]

  (gen-parse-list "   command   ")
  => [[:COMMAND "command"]]

  (gen-parse-list "   command    params   ")
  => [[:COMMAND "command"] [:PARAMS "params"]]

  (gen-parse-list "   command   arg1   arg2   :trailing arg   ")
  => [[:COMMAND "command"] [:PARAMS "arg1" "arg2" "trailing arg   "]]

  (gen-parse-list (str "command arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 "
                       "arg10 arg11 arg12 arg13 arg14 :trailing   "))
  => [[:COMMAND "command"] [:PARAMS "arg1" "arg2" "arg3" "arg4" "arg5" "arg6"
                            "arg7" "arg8" "arg9" "arg10" "arg11" "arg12"
                            "arg13" "arg14" "trailing   "]]

  (gen-parse-list (str "command arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 "
                       "arg10 arg11 arg12 arg13 arg14 arg15   "))
  => [[:COMMAND "command"]
      [:PARAMS "arg1" "arg2" "arg3" "arg4" "arg5" "arg6"
       "arg7" "arg8" "arg9" "arg10" "arg11" "arg12"
       "arg13" "arg14" "arg15"]]
  )

(facts "gen-parse-map"
  (gen-parse-map [[:PREFIX "prefix"] [:COMMAND "command"]
                  [:PARAMS "arg1" "arg2"]])
  => {:prefix "prefix" :command :command :params ["arg1" "arg2"]}

  (gen-parse-map [[:PREFIX "prefix"] [:COMMAND "command"]])
  => {:prefix "prefix" :command :command :params []}

  (gen-parse-map [[:COMMAND "command"] [:PARAMS "arg1" "arg2"]])
  => {:command :command :params ["arg1" "arg2"]}

  (gen-parse-map [[:COMMAND "command"]])
  => {:command :command :params []}
  )

(facts "parse"
  (parse ":prefix join &chan") => {:command :join :chan "&chan"}
  (parse "join &chan") => {:command :join :chan "&chan"}
  (parse ":prefix quit") => {:command :quit}
  (parse "quit") => {:command :quit}
  (parse ":prefix") => {:command :invalid :invalid :unknown-command}
  )

(facts "make-command"
  (make-command {:command :invalid})
  => {:command :invalid :invalid :unknown-command}
  )

(facts "make-command pass"
  (make-command {:command :pass}) => {:command :pass}

  (make-command {:command :pass :params ["password"]})
  => {:command :pass :password "password"}

  (make-command {:command :pass :params ["password" "extra"]})
  => {:command :pass :password "password"}
  )

(facts "make-command nick"
  (make-command {:command :nick :params []})
  => {:command :nick :invalid :no-nickname-given}

  (make-command {:command :nick :params ["nickname"]})
  => {:command :nick :nick "nickname"}

  (make-command {:command :nick :params ["nickname" "extra"]})
  => {:command :nick :nick "nickname"}

  )

(facts "make-command user"
  (make-command {:command :user :params []})
  => {:command :user :invalid :need-more-params}

  (make-command {:command :user :params ["user"]})
  => {:command :user :invalid :need-more-params}

  (make-command {:command :user :params ["user" "mode"]})
  => {:command :user :invalid :need-more-params}

  (make-command {:command :user :params ["user" "mode" "unused"]})
  => {:command :user :invalid :need-more-params}

  (make-command {:command :user :params ["user" "mode" "unused" "realname"]})
  => {:command :user :user "user" :mode "mode" :realname "realname"}

  (make-command {:command :user :params
                 ["user" "mode" "unused" "realname" "extra"]})
  => {:command :user :user "user" :mode "mode" :realname "realname"}
  )

(facts "make-command quit"
  (make-command {:command :quit :params []}) => {:command :quit}

  (make-command {:command :quit :params ["extra"]}) => {:command :quit}
  )

(facts "make-command join"
  (make-command {:command :join :params []})
  => {:command :join :invalid :need-more-params}

  (make-command {:command :join :params ["channel"]})
  => {:command :join :chan "channel"}

  (make-command {:command :join :params ["0"]})
  => {:command :join :chan "0"}

  (make-command {:command :join :params ["channel" "extra"]})
  => {:command :join :chan "channel"}
  )

(facts "make-command part"
  (make-command {:command :part :params []})
  => {:command :part :invalid :need-more-params}

  (make-command {:command :part :params ["channel"]})
  => {:command :part :chan "channel"}

  (make-command {:command :part :params ["channel" "extra"]})
  => {:command :part :chan "channel"}
  )

(facts "make-command topic"
  (make-command {:command :topic :params []})
  => {:command :topic :invalid :need-more-params}

  (make-command {:command :topic :params ["channel"]})
  => {:command :topic :chan "channel"}

  (make-command {:command :topic :params ["channel" "topic"]})
  => {:command :topic :chan "channel" :topic "topic"}

  (make-command {:command :topic :params ["channel" "topic" "extra"]})
  => {:command :topic :chan "channel" :topic "topic"}
  )

(facts "make-command names"
  (make-command {:command :names :params []})
  => {:command :names}

  (make-command {:command :names :params ["channel"]})
  => {:command :names :chan "channel"}

  (make-command {:command :names :params ["channel" "extra"]})
  => {:command :names :chan "channel"}
  )

(facts "make-command list"
  (make-command {:command :list :params []})
  => {:command :list}

  (make-command {:command :list :params ["channel"]})
  => {:command :list :chan "channel"}

  (make-command {:command :list :params ["channel" "extra"]})
  => {:command :list :chan "channel"}
  )

(facts "make-command kick"
  (make-command {:command :kick :params []})
  => {:command :kick :invalid :need-more-params}

  (make-command {:command :kick :params ["channel"]})
  => {:command :kick :invalid :need-more-params}

  (make-command {:command :kick :params ["channel" "user"]})
  => {:command :kick :chan "channel" :user "user"}
  )

(facts "make-command privmsg"
  (make-command {:command :privmsg :params []})

  => {:command :privmsg :invalid :no-recipient}

  (make-command {:command :privmsg :params ["target"]})
  => {:command :privmsg :invalid :no-text}

  (make-command {:command :privmsg :params ["target" "text"]})
  => {:command :privmsg :target "target" :text "text"}
  )

(facts "make-command ison"
  (make-command {:command :ison :params []})
  => {:command :ison :invalid :need-more-params}

  (make-command {:command :ison :params ["nickname"]})
  => {:command :ison :nick "nickname"}
  )
