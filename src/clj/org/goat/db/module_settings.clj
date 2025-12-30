(ns org.goat.db.module-settings
  (:require [clojure.java.jdbc :as sql]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/module_settings.db"})

(defn create-db
  "Create the module settings database and tables if they don't exist"
  []
  (when-not (util/tbl-exists? db :chat_module_ignores)
    (try
      (sql/db-do-commands db
        (sql/create-table-ddl :chat_module_ignores
          [[:chat_id :integer "NOT NULL"]
           [:module_name :text "NOT NULL"]
           [:created_at :datetime]
           [:created_by :text]
           ["PRIMARY KEY (chat_id, module_name)"]]))
      (sql/execute! db "CREATE INDEX idx_chat ON chat_module_ignores(chat_id)")
      (sql/execute! db "CREATE INDEX idx_module ON chat_module_ignores(module_name)")
      (catch Exception e
        (println "Cannot create chat_module_ignores table" (.getMessage e))))))

(defn is-module-ignored?
  "Check if module is ignored in this chat"
  [chat-id module-name]
  (boolean
    (seq (sql/query db
           ["SELECT 1 FROM chat_module_ignores WHERE chat_id=? AND module_name=?"
            chat-id module-name]))))

(defn ignore-module!
  "Add module to ignore list for this chat"
  [chat-id module-name username]
  (sql/insert! db :chat_module_ignores
    {:chat_id chat-id
     :module_name module-name
     :created_at (System/currentTimeMillis)
     :created_by username}))

(defn unignore-module!
  "Remove module from ignore list for this chat"
  [chat-id module-name]
  (sql/delete! db :chat_module_ignores
    ["chat_id=? AND module_name=?" chat-id module-name]))

(defn get-ignored-modules
  "Get list of ignored modules for a chat"
  [chat-id]
  (map :module_name
    (sql/query db
      ["SELECT module_name FROM chat_module_ignores WHERE chat_id=? ORDER BY module_name"
       chat-id])))

;; Initialize database on load
(create-db)
