(ns org.goat.db.urls
  (:require [clojure.java.jdbc :refer :all :as sql]
            [org.goat.db.util]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/urls.db"})

(defn create-db
  "If not DB file found, we create db and table"
  []
  (when (not (util/tbl-exists? db "urls" ))
    (try (db-do-commands db
                         (create-table-ddl :urls
                                           [[:url :text]
                                            [:msg :text]
                                            [:chatid :int]
                                            [:chatname :text]
                                            [:sender :text]
                                            [:time :datetime]]))
         (sql/execute! db "create index urltimeidx on urls(time)")
         (sql/execute! db "create index urlmsgidx on urls(msg)")
         (catch Exception e
           (println "Cannot create urls table" (.getMessage e))))))


(defn save-url
  "Save url info down to DB. We expect:
  :url - the url
  :msg - the full original message
  :chatid - the chat it was in
  :chatname - the name of the chat it was in
  :sender - the sender
  :time - the time in seconds since epoch"
  [urlinfo]
  (sql/insert! db :urls urlinfo))

(defn get-urls
  "Find all urls matching search otherwise just all urls to limit/offset"
  ([limit] (get-urls limit 0 "%"))
  ([limit offset] (get-urls limit offset "%") )
  ([limit offset search]
   (sql/query db ["select * from urls where url like ? order by time desc limit ?,?" search (str offset) (str limit)])))

(create-db)
