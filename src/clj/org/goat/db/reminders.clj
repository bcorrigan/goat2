(ns org.goat.db.reminders
  "Database layer for reminders - stores and manages reminder data"
  (:require [clojure.java.jdbc :refer :all :as sql]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/reminders.db"})

;;; Database Initialization

(defn create-db
  "Initialize the reminders database schema if it doesn't exist"
  []
  (try
    (when-not (util/tbl-exists? db :reminders)
      (sql/db-do-commands db
        (sql/create-table-ddl :reminders
          [[:reminder_id :integer "PRIMARY KEY AUTOINCREMENT"]
           [:chat_id :integer "NOT NULL"]
           [:username :text "NOT NULL"]
           [:target_user :text "NOT NULL"]
           [:message :text "NOT NULL"]
           [:set_time :integer "NOT NULL"]
           [:due_time :integer "NOT NULL"]
           [:status :text "NOT NULL DEFAULT 'pending'"]
           [:fired_time :integer]]))

      ;; Create indexes for efficient queries
      (sql/execute! db "CREATE INDEX idx_due_time ON reminders(due_time)")
      (sql/execute! db "CREATE INDEX idx_status_due ON reminders(status, due_time)")

      (println "Reminders database initialized"))
    (catch Exception e
      (println "Error creating reminders database:" (.getMessage e)))))

;;; CRUD Operations

(defn add-reminder!
  "Add a new reminder to the database. Returns the reminder ID.

   Parameters:
   - chat-id: Telegram chat ID (Long)
   - username: User who set the reminder
   - target-user: User to be reminded (can be same as username)
   - message: The reminder message/task
   - due-time: When to fire the reminder (milliseconds since epoch)"
  [{:keys [chat-id username target-user message due-time]}]
  (let [set-time (System/currentTimeMillis)]
    (sql/db-transaction* db
      (fn [t-con]
        (sql/execute! t-con
          ["INSERT INTO reminders (chat_id, username, target_user, message, set_time, due_time, status)
            VALUES (?, ?, ?, ?, ?, ?, 'pending')"
           chat-id username target-user message set-time due-time])
        (-> (sql/query t-con ["SELECT last_insert_rowid() as id"])
            first
            :id)))))

(defn get-reminder-by-id
  "Get a specific reminder by its ID"
  [reminder-id]
  (-> (sql/query db
        ["SELECT * FROM reminders WHERE reminder_id = ?" reminder-id])
      first))

(defn get-next-pending-reminder
  "Get the next reminder that needs to fire (earliest due_time).
   Returns nil if no pending reminders."
  []
  (-> (sql/query db
        ["SELECT * FROM reminders
          WHERE status = 'pending'
          ORDER BY due_time ASC
          LIMIT 1"])
      first))

(defn get-pending-reminders
  "Get all pending reminders, ordered by due time"
  []
  (sql/query db
    ["SELECT * FROM reminders
      WHERE status = 'pending'
      ORDER BY due_time ASC"]))

(defn get-user-reminders
  "Get all pending reminders for a specific user (as target)"
  [username]
  (sql/query db
    ["SELECT * FROM reminders
      WHERE target_user = ? COLLATE NOCASE
      AND status = 'pending'
      ORDER BY due_time ASC"
     username]))

(defn mark-reminder-fired!
  "Mark a reminder as fired and record the fired time"
  [reminder-id]
  (sql/execute! db
    ["UPDATE reminders
      SET status = 'fired', fired_time = ?
      WHERE reminder_id = ?"
     (System/currentTimeMillis)
     reminder-id]))

(defn cancel-reminder!
  "Cancel a pending reminder"
  [reminder-id]
  (sql/execute! db
    ["UPDATE reminders
      SET status = 'cancelled'
      WHERE reminder_id = ?"
     reminder-id]))

(defn delete-old-reminders!
  "Delete reminders that have been fired or cancelled and are older than the specified age.
   Default is to delete reminders older than 30 days.

   Parameters:
   - max-age-ms: Maximum age in milliseconds (default: 30 days)"
  ([]
   (delete-old-reminders! (* 30 24 60 60 1000)))
  ([max-age-ms]
   (let [cutoff-time (- (System/currentTimeMillis) max-age-ms)]
     (sql/execute! db
       ["DELETE FROM reminders
         WHERE status IN ('fired', 'cancelled')
         AND fired_time < ?"
        cutoff-time]))))

;; Initialize database on namespace load
(create-db)
