(defproject myapp "0.1.0-SNAPSHOT"
            :description "A simple Clojure application"
            :dependencies [[org.clojure/clojure "1.10.3"]]
            :main myapp.core
            :target-path "target/%s"
            :profiles {:uberjar {:aot :all}})
c