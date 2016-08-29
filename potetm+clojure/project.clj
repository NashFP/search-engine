(defproject search-engine "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha11"]]
  :profiles {:test {:dependencies [[com.google.jimfs/jimfs "1.1"]
                                   [nio2 "0.2.3"]
                                   [org.clojure/test.check "0.9.0"]]}})
