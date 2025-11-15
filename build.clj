(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]
            [clojure.java.io :as io]))

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

(defn discover-namespaces
  "Discover all .clj namespaces under a given path.
   Returns a list of namespace symbols.
   Example: (discover-namespaces 'src/clj' 'org/goat/module')"
  [src-root ns-path]
  (let [dir (io/file src-root ns-path)
        files (file-seq dir)]
    (->> files
         (filter #(.isFile %))
         (filter #(str/ends-with? (.getName %) ".clj"))
         (map #(-> (.getPath %)
                   (str/replace-first (str src-root "/") "")
                   (str/replace "/" ".")
                   (str/replace ".clj" "")
                   (str/replace "_" "-")  ; Convert underscores to hyphens for namespace names
                   symbol))
         (sort))))

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
  (let [;; Core infrastructure namespaces (always needed)
        core-namespaces '[org.goat.core.message
                          org.goat.core.macros]
        ;; Auto-discover all module namespaces
        module-namespaces (discover-namespaces "src/clj" "org/goat/module")
        ;; Auto-discover all db namespaces
        db-namespaces (discover-namespaces "src/clj" "org/goat/db")
        ;; Combine all namespaces
        all-namespaces (vec (concat core-namespaces db-namespaces module-namespaces))]

    (println "Discovered namespaces to compile:")
    (doseq [ns all-namespaces]
      (println "  -" ns))

    (b/compile-clj {:basis basis
                    :ns-compile all-namespaces
                    :class-dir class-dir})))

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
