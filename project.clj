(defproject liblouis-release-helper "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clojure.java-time "0.3.2"]
                 [cljstache "2.0.4"]]
  :target-path "target/%s"
  :resource-paths ["templates"]
  :main release_helper
  :profiles {:uberjar {:aot :all}})
