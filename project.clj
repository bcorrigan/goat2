(defproject org/goat "0.1"
  :description "A telegram bot for fun and hijinx"
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :test-paths ["test/clj"] 
  :javac-options ["--release" "23"]
  :jvm-opts ["-server"]  
  ;; Add manifest configuration
  :manifest {"Main-Class" "org.goat.Goat"}
  
  ;; Add jar configuration
  :uberjar-exclusions [#"resources/.*"]
  :resource-paths []
  :jar-name ~(format "goat-%s.jar" (.format (java.text.SimpleDateFormat. "yyyyMMdd") (java.util.Date.)))
  :uberjar-name ~(format "goat-%s-standalone.jar" (.format (java.text.SimpleDateFormat. "yyyyMMdd") (java.util.Date.)))  
  

  :dependencies [[org.clojure/clojure "1.12.1"]
                [org.telegram/telegrambots-longpolling "9.0.0"]
                [org.telegram/telegrambots-webhook "9.0.0"]
                [com.google.jimfs/jimfs "1.3.1"]
                [org.telegram/telegrambots-client "9.0.0"]
                [org.telegram/telegrambots-extensions "9.0.0"]
                [org.json/json "20250517"]
                [org.apache.commons/commons-lang3 "3.18.0"]
                [org.reflections/reflections "0.10.2"]
                [com.zoho/hawking "0.1.9"]
                [org.mapdb/mapdb "3.1.0"] 
                 ;;:exclusions [org.jetbrains.kotlin/kotlin-stdlib
                 ;;            org.eclipse.collections/eclipse-collections-api
                 ;;            org.eclipse.collections/eclipse-collections
                 ;;            org.eclipse.collections/eclipse-collections-forkjoin
                  ;;           com.google.guava/guava]]
                [quil "4.3.1563"]
                [org.clojure/java.jdbc "0.7.12"]
                [org.xerial/sqlite-jdbc "3.50.3.0"]]

  :profiles {:dev {:dependencies [[nrepl/nrepl "1.3.1"]]}
             :uberjar {:aot :all}}

  :repositories [["clojars" "https://repo.clojars.org"]]

  :target-path "target/%s"
  
  :clean-targets ^{:protect false} [:target-path])
