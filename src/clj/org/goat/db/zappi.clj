(ns org.goat.db.zappi
  "Database layer for the Zappi watchdog. Persists the report chat-id and
   a log of up/down transition events."
  (:require [clojure.java.jdbc :as sql]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/zappi.db"})

(defn create-db
  "Initialise the zappi.db schema if it doesn't already exist."
  []
  (try
    (when-not (util/tbl-exists? db :zappi_config)
      (sql/db-do-commands db
        (sql/create-table-ddl :zappi_config
          [[:key :text "PRIMARY KEY"]
           [:value :text]])))
    (when-not (util/tbl-exists? db :zappi_events)
      (sql/db-do-commands db
        (sql/create-table-ddl :zappi_events
          [[:id :integer "PRIMARY KEY AUTOINCREMENT"]
           [:event :text "NOT NULL"]
           [:timestamp :integer "NOT NULL"]
           [:details :text]]))
      (sql/execute! db "CREATE INDEX idx_zappi_events_ts ON zappi_events(timestamp)"))
    (catch Exception e
      (println "Error creating zappi database:" (.getMessage e)))))

;;; Config

(defn get-config
  "Look up a config value by key, returning the string value or nil."
  [k]
  (-> (sql/query db ["SELECT value FROM zappi_config WHERE key=?" (name k)])
      first
      :value))

(defn set-config!
  "Upsert a config key/value pair."
  [k v]
  (sql/execute! db
    ["INSERT INTO zappi_config(key,value) VALUES(?,?)
      ON CONFLICT(key) DO UPDATE SET value=excluded.value"
     (name k) (str v)]))

(defn delete-config!
  "Remove a config key."
  [k]
  (sql/delete! db :zappi_config ["key=?" (name k)]))

(defn get-report-chat-id
  "The chat-id (Long) where the watchdog should send alerts, or nil if unset."
  []
  (when-let [v (get-config :report_chat_id)]
    (try (Long/parseLong v) (catch Exception _ nil))))

(defn set-report-chat-id!
  "Set the chat that receives Zappi up/down notifications."
  [chat-id]
  (set-config! :report_chat_id chat-id))

(defn clear-report-chat-id! []
  (delete-config! :report_chat_id))

;;; Events

(defn add-event!
  "Record a transition event. event is :up or :down. details is a free-form
   string (e.g. diagnostic snapshot) and may be nil."
  [event timestamp details]
  (sql/insert! db :zappi_events
    {:event (name event)
     :timestamp timestamp
     :details details}))

(defn last-n-events
  "Return the most recent n events as maps with :event (keyword) :timestamp :details,
   ordered newest first."
  [n]
  (->> (sql/query db ["SELECT event,timestamp,details FROM zappi_events
                       ORDER BY timestamp DESC LIMIT ?" n])
       (map (fn [r] (update r :event keyword)))
       vec))

(defn all-events-asc
  "All events, oldest first. Used for downtime-period pairing."
  []
  (->> (sql/query db ["SELECT event,timestamp,details FROM zappi_events
                       ORDER BY timestamp ASC"])
       (map (fn [r] (update r :event keyword)))
       vec))

(defn last-event
  "The single most recent event map, or nil if none."
  []
  (first (last-n-events 1)))

;; Initialise on namespace load
(create-db)
