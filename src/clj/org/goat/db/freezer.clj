(ns org.goat.db.freezer
  (:require [clojure.java.jdbc :refer :all :as sql]
            [clojure.string :as str]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/freezer.db"})

(defn create-db
  "If no DB file found, create the freezer db and tables"
  []
  (try
    ;; Enable foreign key constraints (required for CASCADE)
    (sql/execute! db "PRAGMA foreign_keys = ON")

    ;; Create freezers table
    (when-not (util/tbl-exists? db :freezers)
      (sql/db-do-commands db
        "create table freezers (
           freezer_id integer primary key autoincrement,
           freezer_name text not null unique collate NOCASE,
           created_at datetime
         )"))

    ;; Create items table with foreign key
    (when-not (util/tbl-exists? db :items)
      (sql/db-do-commands db
        "create table items (
           item_id integer primary key autoincrement,
           freezer_id integer not null,
           item_name text not null,
           quantity real not null default 1,
           unit text,
           added_date datetime not null,
           expiry_date datetime,
           notes text,
           removed_date datetime,
           foreign key (freezer_id) references freezers(freezer_id) on delete cascade
         )")
      (sql/execute! db "create index item_freezer_idx on items(freezer_id)"))

    ;; Create user_context table with foreign key
    ;; Tracks each user's default freezer preference
    (when-not (util/tbl-exists? db :user_context)
      (sql/db-do-commands db
        "create table user_context (
           user_id text primary key,
           default_freezer_id integer,
           last_command_time datetime,
           foreign key (default_freezer_id) references freezers(freezer_id) on delete set null
         )"))

    (catch Exception e
      (println "Error creating freezer database:" (.getMessage e)))))

(defn migrate-db
  "Migrate database from old schema to new schema"
  []
  (try
    ;; Check if we have the old schema (has user_id column)
    (let [old-schema? (try
                        (sql/query db ["SELECT user_id FROM freezers LIMIT 1"])
                        true
                        (catch Exception _ false))]
      (when old-schema?
        (println "Migrating freezer database from old schema to new schema...")

        ;; Backup old data
        (let [old-freezers (sql/query db ["SELECT * FROM freezers"])
              old-items (try (sql/query db ["SELECT * FROM items"]) (catch Exception _ []))]

          ;; Drop old tables
          (sql/execute! db "DROP TABLE IF EXISTS items")
          (sql/execute! db "DROP TABLE IF EXISTS freezers")
          (sql/execute! db "DROP TABLE IF EXISTS user_context")
          (sql/execute! db "DROP INDEX IF EXISTS freezer_user_name_idx")

          ;; Create new schema
          (create-db)

          ;; Migrate freezers (make them global by removing duplicates)
          (doseq [freezer old-freezers]
            (try
              (sql/execute! db ["INSERT OR IGNORE INTO freezers (freezer_name, created_at) VALUES (?, ?)"
                               (:freezer_name freezer)
                               (:created_at freezer)])
              (catch Exception e
                (println "Error migrating freezer:" (:freezer_name freezer)))))

          (println "Migration complete. Old per-user freezers are now global shared freezers."))))
    (catch Exception e
      (println "Error during migration:" (.getMessage e)))))

;; ============================================================================
;; Freezer Operations
;; ============================================================================

(defn get-freezers
  "Get all freezers (global, not user-specific)"
  []
  (try
    (sql/query db ["select * from freezers
                    order by freezer_name asc"])
    (catch Exception e
      ;; If tables don't exist yet, create them and retry
      (when (or (re-find #"no such table" (.getMessage e))
                (re-find #"database is locked" (.getMessage e)))
        (create-db)
        (sql/query db ["select * from freezers
                        order by freezer_name asc"])))))

(defn get-freezer-by-id
  "Get a freezer by its ID"
  [freezer-id]
  (-> (sql/query db ["select * from freezers
                      where freezer_id=?" freezer-id])
      first))

(defn get-default-freezer
  "Get the default freezer for a user, or nil if none set"
  [user-id]
  (let [context (-> (sql/query db ["select * from user_context
                                    where user_id=? collate NOCASE" user-id])
                    first)]
    (when-let [freezer-id (:default_freezer_id context)]
      (get-freezer-by-id freezer-id))))

(defn add-freezer
  "Create a new global freezer. Returns the freezer-id of the created freezer."
  [freezer-name]
  (try
    (sql/db-transaction* db (fn [t-con]
      (sql/execute! t-con
        ["insert into freezers (freezer_name, created_at)
          values (?, ?)"
         freezer-name (System/currentTimeMillis)])
      (-> (sql/query t-con ["select last_insert_rowid() as id"])
          first
          :id)))
    (catch Exception e
      (println "Error adding freezer:" (.getMessage e))
      nil)))

(defn delete-freezer
  "Delete a freezer and all its items (cascade)"
  [freezer-id]
  (sql/with-db-connection [conn db]
    (sql/execute! conn ["PRAGMA foreign_keys = ON"])
    (sql/delete! conn :freezers ["freezer_id=?" freezer-id])))

(defn rename-freezer
  "Rename a freezer"
  [freezer-id new-name]
  (sql/execute! db ["update freezers
                     set freezer_name=?
                     where freezer_id=?" new-name freezer-id]))

(defn set-default-freezer
  "Set a freezer as the default for a specific user"
  [user-id freezer-id]
  (try
    (sql/execute! db ["insert into user_context (user_id, default_freezer_id, last_command_time)
                       values(?,?,?)
                       on conflict(user_id)
                       do update set
                         default_freezer_id=excluded.default_freezer_id,
                         last_command_time=excluded.last_command_time"
                      user-id freezer-id (System/currentTimeMillis)])
    true
    (catch Exception e
      (println "Error setting default freezer:" (.getMessage e))
      false)))

(defn get-freezer-by-name
  "Find a freezer by name (case-insensitive, global)"
  [freezer-name]
  (-> (sql/query db ["select * from freezers
                      where freezer_name=? collate NOCASE" freezer-name])
      first))

;; ============================================================================
;; Item Operations
;; ============================================================================

(defn add-item
  "Add a new item to a freezer. Returns the item-id of the created item.
   quantity defaults to 1, unit, notes, and expiry-date are optional."
  ([freezer-id item-name]
   (add-item freezer-id item-name 1 nil nil nil))
  ([freezer-id item-name quantity]
   (add-item freezer-id item-name quantity nil nil nil))
  ([freezer-id item-name quantity unit]
   (add-item freezer-id item-name quantity unit nil nil))
  ([freezer-id item-name quantity unit notes]
   (add-item freezer-id item-name quantity unit notes nil))
  ([freezer-id item-name quantity unit notes expiry-date]
   (try
     (sql/db-transaction* db (fn [t-con]
       (sql/execute! t-con
         ["insert into items (freezer_id, item_name, quantity, unit, added_date, notes, expiry_date)
           values (?, ?, ?, ?, ?, ?, ?)"
          freezer-id item-name quantity unit (System/currentTimeMillis) notes expiry-date])
       (-> (sql/query t-con ["select last_insert_rowid() as id"])
           first
           :id)))
     (catch Exception e
       (println "Error adding item:" (.getMessage e))
       nil))))

(defn get-items
  "Get all active (non-removed) items in a freezer, ordered by item_id.
   Filters out items with quantity <= 0 or removed_date set."
  [freezer-id]
  (sql/query db ["select * from items
                  where freezer_id=? and quantity > 0 and removed_date is null
                  order by item_id asc" freezer-id]))

(defn get-all-items-including-removed
  "Get ALL items in a freezer including removed ones, ordered by item_id.
   Useful for viewing freezer history and past meal ideas."
  [freezer-id]
  (sql/query db ["select * from items
                  where freezer_id=?
                  order by item_id asc" freezer-id]))

(defn get-removed-items
  "Get only removed items from a freezer, ordered by removed_date (most recent first).
   Useful for browsing meal history."
  [freezer-id]
  (sql/query db ["select * from items
                  where freezer_id=? and removed_date is not null
                  order by removed_date desc" freezer-id]))

(defn get-all-removed-items
  "Get all removed items across all freezers, ordered by removed_date (most recent first).
   Useful for browsing global meal history and finding recipe ideas."
  []
  (sql/query db ["select i.*, f.freezer_name
                  from items i
                  join freezers f on i.freezer_id = f.freezer_id
                  where i.removed_date is not null
                  order by i.removed_date desc"]))

(defn get-item-by-id
  "Get a specific item by its ID"
  [item-id]
  (-> (sql/query db ["select * from items
                      where item_id=?" item-id])
      first))

(defn remove-item
  "Remove a quantity of an item. If quantity >= item quantity, mark as removed (soft delete).
   Items are kept in the database with quantity=0 and removed_date set for history tracking.
   Returns the remaining quantity (0 if fully removed)."
  [item-id quantity]
  (let [item (get-item-by-id item-id)]
    (when item
      (let [current-qty (:quantity item)
            new-qty (- current-qty quantity)]
        (if (<= new-qty 0)
          ;; Soft delete: set quantity to 0 and record removed_date
          (do
            (sql/execute! db ["update items
                               set quantity=0, removed_date=?
                               where item_id=?" (System/currentTimeMillis) item-id])
            0)
          ;; Update the quantity
          (do
            (sql/execute! db ["update items
                               set quantity=?
                               where item_id=?" new-qty item-id])
            new-qty))))))

(defn search-items
  "Search for active (non-removed) items across all freezers (global).
   Returns items with their freezer information."
  [search-term]
  (sql/query db ["select i.*, f.freezer_name
                  from items i
                  join freezers f on i.freezer_id = f.freezer_id
                  where i.item_name like ? collate NOCASE
                    and i.quantity > 0 and i.removed_date is null
                  order by f.freezer_name, i.item_id"
                 (str "%" search-term "%")]))

(defn get-item-age
  "Get the age of an item in days"
  [item-id]
  (let [item (get-item-by-id item-id)]
    (when item
      (let [added-date (:added_date item)
            now (System/currentTimeMillis)
            age-ms (- now added-date)
            age-days (quot age-ms (* 1000 60 60 24))]
        age-days))))

;; ============================================================================
;; Helper Functions for Smart Item Management
;; ============================================================================

(defn find-matching-item
  "Find an existing item in a freezer by name and unit (case-insensitive).
   Returns the item or nil if not found."
  [freezer-id item-name unit]
  (let [items (get-items freezer-id)
        normalized-name (str/lower-case (str/trim item-name))
        normalized-unit (when unit (str/lower-case (str/trim unit)))]
    (first (filter (fn [item]
                     (and (= normalized-name (str/lower-case (:item_name item)))
                          (= normalized-unit (when (:unit item)
                                              (str/lower-case (:unit item))))))
                   items))))

(defn update-item-quantity
  "Update the quantity of an existing item"
  [item-id quantity-to-add]
  (let [item (get-item-by-id item-id)]
    (when item
      (let [new-qty (+ (:quantity item) quantity-to-add)]
        (sql/execute! db ["update items
                           set quantity=?
                           where item_id=?" new-qty item-id])
        new-qty))))

(defn update-item
  "Update an existing item's fields (quantity, unit, expiry_date, item_name).
   Does NOT update: freezer_id, added_date, notes, removed_date.
   Takes item-id and a map of updates.
   Returns true on success, false on error."
  [item-id updates]
  (try
    (let [item (get-item-by-id item-id)]
      (when item
        ;; Build update map with only provided fields
        (let [update-map (cond-> {}
                          (contains? updates :quantity)
                          (assoc :quantity (:quantity updates))

                          (contains? updates :unit)
                          (assoc :unit (:unit updates))

                          (contains? updates :expiry_date)
                          (assoc :expiry_date (:expiry_date updates))

                          (contains? updates :item_name)
                          (assoc :item_name (:item_name updates)))]
          (when (seq update-map)
            (sql/update! db :items update-map ["item_id=?" item-id])
            true))))
    (catch Exception e
      (println "Error updating item:" (.getMessage e))
      false)))

;; Initialize database on load
(create-db)
