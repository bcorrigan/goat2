(ns org.goat.db.reminders
  "Database layer for reminders - stores and manages reminder data"
  (:require [clojure.java.jdbc :refer :all :as sql]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/reminders.db"})

;;; Database Initialization

(defn- column-exists?
  "Check if a column exists in a table"
  [table-name column-name]
  (try
    (let [result (sql/query db
                   [(str "SELECT COUNT(*) as count FROM pragma_table_info(?) WHERE name = ?")
                    (name table-name) (name column-name)])]
      (pos? (:count (first result))))
    (catch Exception e
      false)))

(defn migrate-db
  "Add new columns to existing reminders table if they don't exist"
  []
  (try
    (when (util/tbl-exists? db :reminders)
      ;; Add recurrence columns if they don't exist
      (when-not (column-exists? :reminders :recurrence_type)
        (println "Migrating reminders database: adding recurrence_type column")
        (sql/execute! db "ALTER TABLE reminders ADD COLUMN recurrence_type TEXT"))

      (when-not (column-exists? :reminders :recurrence_pattern)
        (println "Migrating reminders database: adding recurrence_pattern column")
        (sql/execute! db "ALTER TABLE reminders ADD COLUMN recurrence_pattern TEXT"))

      (when-not (column-exists? :reminders :recurrence_end_time)
        (println "Migrating reminders database: adding recurrence_end_time column")
        (sql/execute! db "ALTER TABLE reminders ADD COLUMN recurrence_end_time INTEGER"))

      (when-not (column-exists? :reminders :parent_reminder_id)
        (println "Migrating reminders database: adding parent_reminder_id column")
        (sql/execute! db "ALTER TABLE reminders ADD COLUMN parent_reminder_id INTEGER")))
    (catch Exception e
      (println "Error migrating reminders database:" (.getMessage e)))))

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
           [:fired_time :integer]
           [:recurrence_type :text]
           [:recurrence_pattern :text]
           [:recurrence_end_time :integer]
           [:parent_reminder_id :integer]]))

      ;; Create indexes for efficient queries
      (sql/execute! db "CREATE INDEX idx_due_time ON reminders(due_time)")
      (sql/execute! db "CREATE INDEX idx_status_due ON reminders(status, due_time)")

      (println "Reminders database initialized"))

    ;; Run migrations for existing databases
    (migrate-db)

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
   - due-time: When to fire the reminder (milliseconds since epoch)
   - recurrence-type: Optional - type of recurrence (e.g., 'interval', 'weekly', 'monthly_weekday')
   - recurrence-pattern: Optional - EDN string of recurrence pattern
   - recurrence-end-time: Optional - when recurring reminder should stop
   - parent-reminder-id: Optional - ID of parent recurring reminder (for instances)"
  [{:keys [chat-id username target-user message due-time
           recurrence-type recurrence-pattern recurrence-end-time parent-reminder-id]}]
  (let [set-time (System/currentTimeMillis)]
    (sql/db-transaction* db
      (fn [t-con]
        (sql/execute! t-con
          ["INSERT INTO reminders (chat_id, username, target_user, message, set_time, due_time, status,
                                   recurrence_type, recurrence_pattern, recurrence_end_time, parent_reminder_id)
            VALUES (?, ?, ?, ?, ?, ?, 'pending', ?, ?, ?, ?)"
           chat-id username target-user message set-time due-time
           recurrence-type recurrence-pattern recurrence-end-time parent-reminder-id])
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

(defn get-parent-reminder
  "Get the parent recurring reminder for an instance, or the reminder itself if it has no parent"
  [reminder-id]
  (let [reminder (get-reminder-by-id reminder-id)]
    (if-let [parent-id (:parent_reminder_id reminder)]
      (get-reminder-by-id parent-id)
      reminder)))

(defn get-recurring-reminders
  "Get all active recurring reminders (those that generate instances)"
  []
  (sql/query db
    ["SELECT * FROM reminders
      WHERE recurrence_type IS NOT NULL
      AND parent_reminder_id IS NULL
      ORDER BY set_time DESC"]))

(defn get-user-listable-reminders
  "Get reminders to show in a list for a user.
   Includes one-shot reminders and parent recurring reminders (not instances).
   Returns them ordered by next due time."
  [username]
  (sql/query db
    ["SELECT * FROM reminders
      WHERE target_user = ? COLLATE NOCASE
      AND status = 'pending'
      AND parent_reminder_id IS NULL
      ORDER BY due_time ASC"
     username]))

(defn get-all-listable-reminders
  "Get all reminders to show in a list.
   Includes one-shot reminders and parent recurring reminders (not instances).
   Returns them ordered by next due time."
  []
  (sql/query db
    ["SELECT * FROM reminders
      WHERE status = 'pending'
      AND parent_reminder_id IS NULL
      ORDER BY due_time ASC"]))

(defn cancel-recurring-and-instances!
  "Cancel a recurring reminder and all its pending instances"
  [parent-id]
  (sql/db-transaction* db
    (fn [t-con]
      ;; Cancel the parent
      (sql/execute! t-con
        ["UPDATE reminders SET status = 'cancelled' WHERE reminder_id = ?" parent-id])
      ;; Cancel all pending instances
      (sql/execute! t-con
        ["UPDATE reminders SET status = 'cancelled'
          WHERE parent_reminder_id = ? AND status = 'pending'" parent-id]))))

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
