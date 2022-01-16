(defproject uclj "0.1.2-SNAPSHOT"
  :description "Minimalist and fast Clojure interpreter"
  :url "https://github.com/erdos/uclj"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/core.async "1.5.644"]
                 [org.clojure/core.cache "1.0.225"]
                 [org.clojure/core.logic "1.0.0"]
                 [org.clojure/core.memoize "1.0.253"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/data.xml "0.2.0-alpha6"]
                 [org.clojure/spec.alpha "0.3.214"]
                 [org.clojure/test.check "1.1.1"]
                 [org.clojure/tools.cli "1.0.206"]
                 [org.clojure/tools.trace "0.7.11"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main uclj.core
  :aot :all
  :java-source-paths ["src"]
  :javac-options     ["-target" "1.6" "-source" "1.6"]
  :repl-options {:init-ns uclj.core})
