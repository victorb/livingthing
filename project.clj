(defproject livingthing "0.1.0-SNAPSHOT"
  :description "A permanent REPL"
  :url "https://livingthing.club"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-commons/pomegranate "1.2.0"]
                 [http-kit "2.5.0"]
                 [hiccup "1.0.5"]
                 [ring/ring-core "1.8.2"]
                 [pandect "0.6.1"]]
  :main ^:skip-aot livingthing.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
