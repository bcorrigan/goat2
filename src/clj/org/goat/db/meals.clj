(ns org.goat.db.meals
  "SQLite storage for meal memories.

   Schema:
     meals             - one row per meal with title/score/description/added_by
     meal_ingredients  - many rows per meal, one ingredient each (for search)

   Photos live on disk under mealpics/ keyed by meal_id, not in the DB."
  (:require [clojure.java.jdbc :as sql]
            [clojure.string :as str]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/meals.db"})

(defn create-db
  "Create meals tables if they don't already exist."
  []
  (try
    (sql/execute! db "PRAGMA foreign_keys = ON")

    (when-not (util/tbl-exists? db :meals)
      (sql/db-do-commands db
        "create table meals (
           meal_id     integer primary key autoincrement,
           title       text not null,
           score       integer,
           description text,
           added_by    text not null,
           chat_id     integer,
           added_date  integer not null
         )"))

    (when-not (util/tbl-exists? db :meal_ingredients)
      (sql/db-do-commands db
        "create table meal_ingredients (
           meal_id    integer not null,
           ingredient text not null collate NOCASE,
           foreign key (meal_id) references meals(meal_id) on delete cascade
         )")
      (sql/execute! db "create index meal_ing_idx on meal_ingredients(ingredient)")
      (sql/execute! db "create index meal_ing_meal_idx on meal_ingredients(meal_id)"))

    (catch Exception e
      (println "Error creating meals database:" (.getMessage e)))))

(defn- with-tables
  "Run f, retrying once after create-db if the tables don't yet exist."
  [f]
  (try (f)
       (catch Exception e
         (if (and (.getMessage e)
                  (re-find #"no such table" (.getMessage e)))
           (do (create-db) (f))
           (throw e)))))

(defn add-meal
  "Insert a meal row and return its newly-assigned meal_id."
  [{:keys [title score description added-by chat-id]}]
  (with-tables
    #(sql/db-transaction* db
       (fn [t-con]
         (sql/execute! t-con
           ["insert into meals (title, score, description, added_by, chat_id, added_date)
             values (?, ?, ?, ?, ?, ?)"
            title score description added-by chat-id (System/currentTimeMillis)])
         (-> (sql/query t-con ["select last_insert_rowid() as id"])
             first
             :id)))))

(defn add-ingredients
  "Insert one row per ingredient for the given meal-id.
   Trims and drops blank entries."
  [meal-id ingredients]
  (with-tables
    #(sql/db-transaction* db
       (fn [t-con]
         (doseq [ing (->> ingredients
                          (map (fn [s] (when s (str/trim s))))
                          (remove str/blank?))]
           (sql/execute! t-con
             ["insert into meal_ingredients (meal_id, ingredient) values (?, ?)"
              meal-id ing]))))))

(defn get-ingredients
  "Return the list of ingredient strings for a meal in insertion order."
  [meal-id]
  (with-tables
    #(->> (sql/query db ["select ingredient from meal_ingredients
                          where meal_id=? order by rowid asc" meal-id])
          (map :ingredient)
          vec)))

(defn get-meal
  "Fetch a meal by id, with :ingredients attached. Returns nil if not found."
  [meal-id]
  (with-tables
    #(when-let [m (first (sql/query db ["select * from meals where meal_id=?" meal-id]))]
       (assoc m :ingredients (get-ingredients meal-id)))))

(defn random-meal
  "Return one random meal (with ingredients) or nil if the DB is empty."
  []
  (with-tables
    #(when-let [m (first (sql/query db ["select * from meals order by random() limit 1"]))]
       (assoc m :ingredients (get-ingredients (:meal_id m))))))

(defn count-meals
  "Total number of meals stored."
  []
  (with-tables
    #(-> (sql/query db ["select count(*) as c from meals"]) first :c)))

(defn search-meals
  "Case-insensitive search across title, description, and ingredients.
   Returns a vector of meal maps (each with :ingredients) ordered by meal_id desc.
   Empty/blank term returns all meals (capped by the caller if needed)."
  [term]
  (with-tables
    #(let [pattern (str "%" (str/lower-case (or term "")) "%")
           rows (sql/query db
                  ["select distinct m.* from meals m
                    left join meal_ingredients i on i.meal_id = m.meal_id
                    where lower(m.title) like ?
                       or lower(coalesce(m.description,'')) like ?
                       or lower(coalesce(i.ingredient,'')) like ?
                    order by m.meal_id desc"
                   pattern pattern pattern])]
       (mapv (fn [m] (assoc m :ingredients (get-ingredients (:meal_id m))))
             rows))))

(defn list-meals
  "Return meals ordered by meal_id desc, with optional limit and offset.
   Each meal map includes :ingredients."
  [& {:keys [limit offset] :or {limit 20 offset 0}}]
  (with-tables
    #(let [rows (sql/query db
                  ["select * from meals order by meal_id desc limit ? offset ?"
                   limit offset])]
       (mapv (fn [m] (assoc m :ingredients (get-ingredients (:meal_id m))))
             rows))))

(defn search-meals-paged
  "Like search-meals but with limit/offset for pagination."
  [term limit offset]
  (with-tables
    #(let [pattern (str "%" (str/lower-case (or term "")) "%")
           rows (sql/query db
                  ["select distinct m.* from meals m
                    left join meal_ingredients i on i.meal_id = m.meal_id
                    where lower(m.title) like ?
                       or lower(coalesce(m.description,'')) like ?
                       or lower(coalesce(i.ingredient,'')) like ?
                    order by m.meal_id desc
                    limit ? offset ?"
                   pattern pattern pattern limit offset])]
       (mapv (fn [m] (assoc m :ingredients (get-ingredients (:meal_id m))))
             rows))))

(defn count-search-results
  "Count meals matching a search term."
  [term]
  (with-tables
    #(let [pattern (str "%" (str/lower-case (or term "")) "%")]
       (-> (sql/query db
             ["select count(distinct m.meal_id) as c from meals m
               left join meal_ingredients i on i.meal_id = m.meal_id
               where lower(m.title) like ?
                  or lower(coalesce(m.description,'')) like ?
                  or lower(coalesce(i.ingredient,'')) like ?"
              pattern pattern pattern])
           first :c))))

(defn delete-meal
  "Delete a meal and its ingredients (cascade). Returns true."
  [meal-id]
  (with-tables
    #(do
       (sql/with-db-connection [conn db]
         (sql/execute! conn ["PRAGMA foreign_keys = ON"])
         (sql/delete! conn :meals ["meal_id=?" meal-id]))
       true)))

;; Initialize on load
(create-db)
