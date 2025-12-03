(ns org.goat.module.freezer.csv
  "CSV import/export functionality for freezer inventory"
  (:require [clojure.string :as str]
            [clojure.java.jdbc :as sql]
            [org.goat.db.freezer :as db])
  (:import [java.time Instant LocalDate ZoneId]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; CSV Formatting and Escaping
;; ============================================================================

(defn escape-csv-field
  "Escape a field for CSV output following RFC 4180.
   - Wrap in quotes if contains comma, quote, or newline
   - Escape internal quotes by doubling them"
  [s]
  (if (nil? s)
    ""
    (let [s (str s)]
      (if (or (str/includes? s ",")
              (str/includes? s "\"")
              (str/includes? s "\n")
              (str/includes? s "\r"))
        (str "\"" (str/replace s "\"" "\"\"") "\"")
        s))))

(defn format-date
  "Format a timestamp (epoch millis) to DD/MM/YYYY format"
  [timestamp]
  (if (nil? timestamp)
    ""
    (try
      (let [instant (Instant/ofEpochMilli timestamp)
            date (.atZone instant (ZoneId/systemDefault))
            local-date (.toLocalDate date)
            formatter (DateTimeFormatter/ofPattern "dd/MM/yyyy")]
        (.format local-date formatter))
      (catch Exception e
        (println "Error formatting date:" (.getMessage e))
        ""))))

(defn format-quantity
  "Format a quantity, showing as integer if it's a whole number"
  [qty]
  (if (nil? qty)
    ""
    (if (== qty (Math/floor qty))
      (str (int qty))
      (str qty))))

;; ============================================================================
;; CSV Generation
;; ============================================================================

(defn item->csv-row
  "Convert an item map to a CSV row (as a string)"
  [item]
  (let [id (escape-csv-field (:item_id item))
        freezer (escape-csv-field (str/capitalize (:freezer_name item)))
        name (escape-csv-field (:item_name item))
        quantity (format-quantity (:quantity item))
        unit (escape-csv-field (:unit item))
        date-added (format-date (:added_date item))
        expiry (format-date (:expiry_date item))]
    (str/join "," [id freezer name quantity unit date-added expiry])))

(defn get-all-active-items
  "Query all active items from all freezers"
  []
  (sql/query db/db
    ["SELECT i.item_id, f.freezer_name, i.item_name, i.quantity,
             i.unit, i.added_date, i.expiry_date
      FROM items i
      JOIN freezers f ON i.freezer_id = f.freezer_id
      WHERE i.quantity > 0 AND i.removed_date IS NULL
      ORDER BY f.freezer_name, i.item_id"]))

(defn generate-csv-data
  "Generate CSV data for all active items.
   Returns a string containing the complete CSV file."
  []
  (let [items (get-all-active-items)
        header "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date"
        rows (map item->csv-row items)]
    (str/join "\n" (cons header rows))))

(defn csv-bytes
  "Convert a CSV string to UTF-8 byte array"
  [csv-string]
  (.getBytes csv-string "UTF-8"))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn count-items
  "Count the number of data rows (not including header) in CSV data"
  [csv-data]
  (let [lines (str/split-lines csv-data)]
    (max 0 (dec (count lines))))) ; subtract 1 for header
