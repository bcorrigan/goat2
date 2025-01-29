(defproject org/goat "0.1"
  :description "FIXME: add description"
  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :javac-options ["--release" "23"]
  
  ;; Add manifest configuration
  :manifest {"Main-Class" "org.goat.Goat"}
  
  ;; Add jar configuration
  :jar-name "goat-0.1.jar"
  :uberjar-name "goat-0.1-standalone.jar"
  
  :dependencies [[org.clojure/clojure "1.12.0"]
                [org.telegram/telegrambots-longpolling "8.2.0"]
                [org.telegram/telegrambots-webhook "8.2.0"]
                [com.google.jimfs/jimfs "1.3.0"]
                [org.telegram/telegrambots-client "8.2.0"]
                [org.telegram/telegrambots-extensions "8.2.0"]
                [org.json/json "20250107"]
                [org.apache.commons/commons-lang3 "3.17.0"]
                [org.reflections/reflections "0.10.2"]
                [com.zoho/hawking "0.1.9"]
                [org.mapdb/mapdb "3.1.0" 
                 :exclusions [org.jetbrains.kotlin/kotlin-stdlib
                             org.eclipse.collections/eclipse-collections-api
                             org.eclipse.collections/eclipse-collections
                             org.eclipse.collections/eclipse-collections-forkjoin
                             com.google.guava/guava]]
                [jline/jline "0.9.94"]
                [quil "4.3.1563"]
                [org.clojure/java.jdbc "0.7.12"]
                [org.xerial/sqlite-jdbc "3.48.0.0"]]

  :profiles {:dev {:dependencies [[nrepl/nrepl "1.3.1"]]}
             :uberjar {:aot :all}}

  :repositories [["clojars" "https://repo.clojars.org"]
                ["spring-external" "http://repository.springsource.com/maven/bundles/external"]]

  :target-path "target/%s"
  
  :clean-targets ^{:protect false} [:target-path])
