(defproject irc "0.1.0-SNAPSHOT"
  :description "IRC Server"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [org.clojure/core.match "0.2.0"]
                 [org.clojure/tools.cli "0.3.0"]
                 [midje "1.6-beta1"]
                 [instaparse "1.2.7"]]
  :main irc.system
  :aliases {"launch" ["trampoline" "run" "irc"]}
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
