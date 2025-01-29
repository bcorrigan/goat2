(defproject org/goat "0.1"
  :description "FIXME: add description"
  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :main org.goat.Goat
  :aot :all  ; Add this to address the AOT warning
  
  ; Updated Java compiler options to use --release instead of -source/-target
  :javac-options ["--release" "22"]
  
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
                ; Updated MapDB dependency with exclusions to address version range warnings
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

  :profiles {:dev {:dependencies [[cider/cider-nrepl "0.52.0"]
                                [nrepl/nrepl "1.3.1"]]}}

  :repositories [["clojars" "https://repo.clojars.org"]
                ["spring-external" "http://repository.springsource.com/maven/bundles/external"]]

  :plugins [[cider/cider-nrepl "0.52.0"]]

  :repl-options {:host "localhost"
                :port 40000
                :middleware [cider.nrepl/wrap-apropos
                            cider.nrepl/wrap-classpath
                            cider.nrepl/wrap-complete
                            cider.nrepl/wrap-debug
                            cider.nrepl/wrap-format
                            cider.nrepl/wrap-info
                            cider.nrepl/wrap-inspect
                            cider.nrepl/wrap-macroexpand
                            cider.nrepl/wrap-ns
                            cider.nrepl/wrap-spec
                            cider.nrepl/wrap-profile
                            cider.nrepl/wrap-refresh
                            cider.nrepl/wrap-resource
                            cider.nrepl/wrap-stacktrace
                            cider.nrepl/wrap-test
                            cider.nrepl/wrap-trace
                            cider.nrepl/wrap-out
                            cider.nrepl/wrap-undef
                            cider.nrepl/wrap-version
                            cider.nrepl/wrap-xref]}

  :uberjar-name "goat-0.1-standalone.jar"
  
  :target-path "target/%s"
  
  :clean-targets ^{:protect false} [:target-path])



