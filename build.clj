(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]))

(def lib 'org/goat)
(def version "0.1")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))

;; Generate jar names with timestamp
(defn jar-name []
  (let [date-format (java.text.SimpleDateFormat. "yyyyMMdd")
        date-str (.format date-format (java.util.Date.))]
    (format "goat-%s.jar" date-str)))

(defn uberjar-name []
  (let [date-format (java.text.SimpleDateFormat. "yyyyMMdd")
        date-str (.format date-format (java.util.Date.))]
    (format "goat-%s-standalone.jar" date-str)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-java [_]
  (println "Compiling Java sources...")
  (b/javac {:src-dirs ["src/java"]
            :class-dir class-dir
            :basis basis
            :javac-opts ["--release" "23"]}))

(defn compile-clojure [_]
  (println "AOT compiling Clojure sources...")
  (b/compile-clj {:basis basis
                  :ns-compile '[org.goat.core.message
                                org.goat.core.macros
                                org.goat.db.urls
                                org.goat.db.users
                                org.goat.db.user-stats
                                org.goat.db.words
                                org.goat.db.util
                                org.goat.module.Capture
                                org.goat.module.CoreCommands
                                org.goat.module.Wordle
                                org.goat.module.WordStats]
                  :class-dir class-dir}))

(defn uber [_]
  (clean nil)
  (compile-java nil)
  (compile-clojure nil)

  (let [uber-file (str "target/" (uberjar-name))]
    (println "Creating uberjar:" uber-file)
    (b/uber {:class-dir class-dir
             :uber-file uber-file
             :basis basis
             :main 'org.goat.Goat
             :manifest {"Main-Class" "org.goat.Goat"}}))

  (println "Uberjar created successfully!"))

(defn jar [_]
  (clean nil)
  (compile-java nil)
  (compile-clojure nil)

  (let [jar-file (str "target/" (jar-name))]
    (println "Creating jar:" jar-file)
    (b/jar {:class-dir class-dir
            :jar-file jar-file
            :basis basis
            :main 'org.goat.Goat
            :manifest {"Main-Class" "org.goat.Goat"}}))

  (println "Jar created successfully!"))

(defn compile-all [_]
  (clean nil)
  (compile-java nil)
  (compile-clojure nil)
  (println "Compilation complete!"))
