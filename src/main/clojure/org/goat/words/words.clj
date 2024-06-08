(ns org.goat.words.words 
  (:require [clojure.java.jdbc :refer :all])
  (:gen-class))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/words.db"
  })	    

(defn create-db
  "If not DB file found, we create db and table"
  []
  (try (db-do-commands db
    (create-table-ddl :words
                      [[ :word :text ]
                       [ :length :int ]
                       [ :definition :text]]))
    (catch Exception e
      (println "Fatal error" (.getMessage e)))))

(defn tbl-exists?
  "Check the given table exists"
  [tbl]
  (not (empty? (query db [(format "SELECT name FROM sqlite_master WHERE type='table' AND name='%s'" tbl )]))))
