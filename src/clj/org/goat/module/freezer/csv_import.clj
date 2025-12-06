(ns org.goat.module.freezer.csv-import
  "CSV import functionality for freezer inventory"
  (:require [clojure.string :as str]
            [clojure.java.jdbc :as sql]
            [org.goat.db.freezer :as db]
            [org.goat.module.freezer.parser :as parser])
  (:import [java.io BufferedReader StringReader]))

;; ============================================================================
;; CSV Parsing (RFC 4180 compliant)
;; ============================================================================

(defn parse-csv-line
  "Parse a single CSV line respecting RFC 4180.
   Handles quoted fields with embedded commas and escaped quotes (doubled quotes).
   Returns a vector of field values."
  [line]
  (loop [chars (seq line)
         fields []
         current-field []
         in-quotes false]
    (if (empty? chars)
      ;; End of line - add final field
      (conj fields (str/join current-field))
      (let [c (first chars)
            next-c (second chars)]
        (cond
          ;; Quote character
          (= c \")
          (cond
            ;; Escaped quote (two quotes in a row)
            (and in-quotes (= next-c \"))
            (recur (rest (rest chars)) fields (conj current-field \") true)

            ;; End quote
            in-quotes
            (recur (rest chars) fields current-field false)

            ;; Start quote
            :else
            (recur (rest chars) fields current-field true))

          ;; Comma - field separator (only if not in quotes)
          (and (= c \,) (not in-quotes))
          (recur (rest chars) (conj fields (str/join current-field)) [] false)

          ;; Regular character
          :else
          (recur (rest chars) fields (conj current-field c) in-quotes))))))

(defn parse-csv
  "Parse entire CSV string into {:header [...] :rows [...]}.
   Each row is a map of normalized column names to values.
   Returns {:header [col-names] :rows [{:id \"1\" :freezer \"Garage\" ...}]}"
  [csv-string]
  (let [lines (str/split-lines csv-string)
        non-empty-lines (remove str/blank? lines)]
    (if (empty? non-empty-lines)
      {:header [] :rows []}
      (let [header-line (first non-empty-lines)
            header (parse-csv-line header-line)
            data-lines (rest non-empty-lines)

            ;; Normalize column names for map keys
            normalized-keys (mapv #(-> %
                                      str/lower-case
                                      (str/replace " " "-")
                                      keyword)
                                 header)

            ;; Parse each data line into a map
            rows (mapv (fn [line]
                        (let [values (parse-csv-line line)]
                          (zipmap normalized-keys values)))
                      data-lines)]
        {:header header :rows rows}))))

;; ============================================================================
;; Validation Functions
;; ============================================================================

(defn validate-header
  "Validate CSV header has required columns.
   Expected: ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date
   Returns {:valid true} or {:valid false :error \"message\"}"
  [header]
  (let [expected ["ID" "Freezer" "Item" "Quantity" "Unit" "Date Added" "Expiry Date"]
        normalized-header (mapv str/trim header)
        normalized-expected (mapv str/trim expected)]
    (if (= normalized-header normalized-expected)
      {:valid true}
      {:valid false
       :error (str "Invalid CSV header.\n\n"
                  "Expected:\n"
                  (str/join "," expected) "\n\n"
                  "Found:\n"
                  (str/join "," header) "\n\n"
                  "Please use the export command to get the correct format.")})))

(defn parse-quantity
  "Parse quantity string to double.
   Returns [value nil] on success or [nil error-msg] on failure."
  [qty-str]
  (let [qty-str (str/trim qty-str)]
    (if (str/blank? qty-str)
      [nil "Missing quantity"]
      (try
        (let [qty (Double/parseDouble qty-str)]
          (if (>= qty 0)
            [qty nil]
            [nil "Quantity cannot be negative"]))
        (catch NumberFormatException _
          [nil (str "Invalid quantity \"" qty-str "\" - must be a number")])))))

(defn parse-date-field
  "Parse date field (DD/MM/YYYY) to timestamp.
   Returns timestamp or nil if empty/invalid."
  [date-str]
  (when-not (str/blank? date-str)
    (parser/parse-expiry-date date-str)))

(defn parse-item-id
  "Parse item ID field to integer.
   Returns ID or nil if empty/invalid."
  [id-str]
  (when-not (str/blank? id-str)
    (try
      (Integer/parseInt (str/trim id-str))
      (catch NumberFormatException _ nil))))

(defn levenshtein-distance
  "Calculate Levenshtein distance between two strings (for typo detection)"
  [s1 s2]
  (let [s1 (str/lower-case s1)
        s2 (str/lower-case s2)
        m (count s1)
        n (count s2)]
    (cond
      (zero? m) n
      (zero? n) m
      :else
      (let [prev-row (vec (range (inc n)))]
        (loop [i 0
               prev-row prev-row]
          (if (= i m)
            (last prev-row)
            (let [curr-row (reduce
                            (fn [row j]
                              (conj row
                                    (min
                                     (inc (nth prev-row (inc j)))      ; deletion
                                     (inc (nth row j))                  ; insertion
                                     (+ (nth prev-row j)               ; substitution
                                        (if (= (nth s1 i) (nth s2 j)) 0 1)))))
                            [(inc i)]  ; first element of curr-row
                            (range n))]
              (recur (inc i) curr-row))))))))

(defn find-similar-freezer
  "Find a freezer with similar name (for typo suggestions).
   Returns freezer name or nil if no close match."
  [typo]
  (let [freezers (db/get-freezers)
        typo-lower (str/lower-case typo)]
    (->> freezers
         (map (fn [f]
                (let [fname (:freezer_name f)
                      distance (levenshtein-distance typo fname)]
                  {:name fname :distance distance})))
         (filter #(<= (:distance %) 2))  ; Max edit distance of 2
         (sort-by :distance)
         first
         :name)))

(defn validate-freezer-name
  "Validate that a freezer name exists in the database.
   Returns {:valid true :freezer-id N} or {:valid false :error \"...\" :suggestion \"...\"}"
  [freezer-name]
  (if (str/blank? freezer-name)
    {:valid false :error "Missing freezer name"}
    (if-let [freezer (db/get-freezer-by-name freezer-name)]
      {:valid true :freezer-id (:freezer_id freezer)}
      (let [suggestion (find-similar-freezer freezer-name)]
        {:valid false
         :error (str "Unknown freezer \"" freezer-name "\"")
         :suggestion suggestion}))))

(defn validate-row
  "Validate a single row.
   Returns {:valid true :data {...}} or {:valid false :error \"...\"}"
  [row-map row-number]
  (let [id-str (:id row-map "")
        freezer-name (str/trim (:freezer row-map ""))
        item-name (str/trim (:item row-map ""))
        qty-str (:quantity row-map "")
        unit (when-not (str/blank? (:unit row-map)) (str/trim (:unit row-map)))
        date-added-str (:date-added row-map "")
        expiry-str (:expiry-date row-map "")]

    (cond
      ;; Check freezer name
      (str/blank? freezer-name)
      {:valid false :error "Missing freezer name"}

      ;; Check item name
      (str/blank? item-name)
      {:valid false :error "Missing item name"}

      :else
      (let [;; Validate freezer exists
            freezer-validation (validate-freezer-name freezer-name)]
        (if-not (:valid freezer-validation)
          {:valid false
           :error (:error freezer-validation)
           :suggestion (:suggestion freezer-validation)}

          ;; Parse quantity
          (let [[qty qty-error] (parse-quantity qty-str)]
            (if qty-error
              {:valid false :error qty-error}

              ;; Parse dates
              (let [item-id (parse-item-id id-str)
                    date-added (parse-date-field date-added-str)
                    expiry-date (parse-date-field expiry-str)]

                ;; Check date formats
                (cond
                  (and (not (str/blank? date-added-str)) (nil? date-added))
                  {:valid false :error (str "Invalid date \"" date-added-str "\" (date does not exist or wrong format - use DD/MM/YYYY)")}

                  (and (not (str/blank? expiry-str)) (nil? expiry-date))
                  {:valid false :error (str "Invalid date \"" expiry-str "\" (date does not exist or wrong format - use DD/MM/YYYY)")}

                  :else
                  {:valid true
                   :data {:item-id item-id
                          :freezer-id (:freezer-id freezer-validation)
                          :freezer-name freezer-name
                          :item-name item-name
                          :quantity qty
                          :unit unit
                          :date-added (or date-added (System/currentTimeMillis))
                          :expiry-date expiry-date}})))))))))

;; ============================================================================
;; Date Comparison Helpers
;; ============================================================================

(defn same-month-year?
  "Check if two timestamps represent dates in the same month and year.
   For freezer inventory, we only care about month-level precision.
   Returns true if both dates are in the same month, false otherwise.
   Handles nil values - returns true only if both are nil."
  [timestamp1 timestamp2]
  (cond
    ;; Both nil - consider them the same
    (and (nil? timestamp1) (nil? timestamp2))
    true

    ;; One is nil, other isn't - different
    (or (nil? timestamp1) (nil? timestamp2))
    false

    ;; Both have values - compare year and month
    :else
    (let [date1 (java.time.LocalDate/ofEpochDay (quot timestamp1 (* 24 60 60 1000)))
          date2 (java.time.LocalDate/ofEpochDay (quot timestamp2 (* 24 60 60 1000)))]
      (and (= (.getYear date1) (.getYear date2))
           (= (.getMonthValue date1) (.getMonthValue date2))))))

(defn item-data-changed?
  "Check if item data has actually changed between database and CSV row.
   Uses month-level precision for dates (ignoring day/time).
   Returns true if any field differs, false if all are the same."
  [db-item csv-data]
  (let [db-qty (:quantity db-item)
        csv-qty (:quantity csv-data)
        db-unit (:unit db-item)
        csv-unit (:unit csv-data)
        db-name (:item_name db-item)
        csv-name (:item-name csv-data)
        db-expiry (:expiry_date db-item)
        csv-expiry (:expiry-date csv-data)]
    (or
     ;; Quantity changed
     (not= db-qty csv-qty)
     ;; Unit changed (accounting for nil/"" equivalence)
     (not= (or db-unit "") (or csv-unit ""))
     ;; Name changed
     (not= db-name csv-name)
     ;; Expiry month/year changed
     (not (same-month-year? db-expiry csv-expiry)))))

;; ============================================================================
;; Import Logic & Row Processing
;; ============================================================================

(defn classify-row
  "Classify a row as :new-item, :update-item, :no-change, or :remove-item.
   Returns {:action :new-item/:update-item/:no-change/:remove-item :data {...}}"
  [validated-data]
  (let [{:keys [item-id quantity]} validated-data
        db-item (when item-id (db/get-item-by-id item-id))]
    (cond
      ;; No ID or ID doesn't exist -> new item
      (or (nil? item-id) (nil? db-item))
      {:action :new-item :data validated-data}

      ;; Quantity is 0 -> remove item
      (zero? quantity)
      {:action :remove-item :data validated-data}

      ;; Item exists but data hasn't changed -> no action needed
      (not (item-data-changed? db-item validated-data))
      {:action :no-change :data validated-data}

      ;; Item exists and data has changed -> update
      :else
      {:action :update-item :data validated-data})))

(defn process-new-item
  "Add a new item to database.
   Returns {:success true :item-id N} or {:success false :error \"...\"}"
  [row-data]
  (try
    (let [{:keys [freezer-id item-name quantity unit expiry-date]} row-data
          ;; Apply 1-year default expiry if not specified (same as chat-based adds)
          final-expiry (or expiry-date (parser/default-expiry-date))
          item-id (db/add-item freezer-id item-name quantity unit nil final-expiry)]
      (if item-id
        {:success true :item-id item-id}
        {:success false :error "Failed to add item to database"}))
    (catch Exception e
      {:success false :error (str "Database error: " (.getMessage e))})))

(defn process-update-item
  "Update an existing item in database.
   Returns {:success true} or {:success false :error \"...\"}"
  [row-data]
  (try
    (let [{:keys [item-id quantity unit expiry-date item-name]} row-data
          success (db/update-item item-id {:quantity quantity
                                           :unit unit
                                           :expiry_date expiry-date
                                           :item_name item-name})]
      (if success
        {:success true}
        {:success false :error "Failed to update item"}))
    (catch Exception e
      {:success false :error (str "Database error: " (.getMessage e))})))

(defn process-remove-item
  "Remove an item (soft delete: quantity=0, removed_date=now).
   Returns {:success true} or {:success false :error \"...\"}"
  [row-data]
  (try
    (let [{:keys [item-id]} row-data
          item (db/get-item-by-id item-id)]
      (if item
        (do
          ;; Remove full quantity to trigger soft delete
          (db/remove-item item-id (:quantity item))
          {:success true})
        {:success false :error (str "Item #" item-id " not found")}))
    (catch Exception e
      {:success false :error (str "Database error: " (.getMessage e))})))

(defn process-row
  "Process a single row after validation.
   Returns result map with :success, :action, :row-number, and optionally :error"
  [row-number validated-data]
  (let [classified (classify-row validated-data)
        action (:action classified)
        data (:data classified)]
    (merge
     {:row-number row-number :action action}
     (case action
       :new-item (process-new-item data)
       :update-item (process-update-item data)
       :remove-item (process-remove-item data)
       :no-change {:success true}))))

;; ============================================================================
;; Batch Import with Transaction
;; ============================================================================

(defn collect-unknown-freezers
  "Collect all unknown freezers from validation errors.
   Returns list of {:name \"...\" :suggestion \"...\"}]"
  [validation-results]
  (->> validation-results
       (filter #(and (not (:valid %))
                     (str/includes? (:error %) "Unknown freezer")))
       (map (fn [result]
              {:name (second (re-find #"Unknown freezer \"(.+?)\"" (:error result)))
               :suggestion (:suggestion result)}))
       (filter :name)
       distinct))

(defn import-csv-data
  "Import CSV data with transaction safety.
   Returns {:success true :stats {...}} or {:success false :error \"...\" :details [...]}"
  [csv-string]
  (try
    ;; Step 1: Parse CSV
    (let [parsed (parse-csv csv-string)]
      (if (empty? (:header parsed))
        {:success false :error "Empty CSV file"}

        ;; Step 2: Validate header
        (let [header-validation (validate-header (:header parsed))]
          (if-not (:valid header-validation)
            {:success false :error (:error header-validation)}

            ;; Step 3: Validate all rows
            (let [rows (:rows parsed)
                  validations (map-indexed
                               (fn [idx row]
                                 (assoc (validate-row row (+ idx 2)) ; +2 for header and 1-based
                                        :row-number (+ idx 2)))
                               rows)

                  validation-errors (filter #(not (:valid %)) validations)
                  unknown-freezers (collect-unknown-freezers validation-errors)]

              ;; Step 4: If validation errors, return them
              (if (seq validation-errors)
                {:success false
                 :error (if (seq unknown-freezers)
                         "Unknown freezers found in CSV"
                         "Validation errors in CSV")
                 :unknown-freezers unknown-freezers
                 :validation-errors (take 10 validation-errors)  ; Limit to 10 errors
                 :total-errors (count validation-errors)}

                ;; Step 5: Process rows in transaction
                (sql/db-transaction* db/db
                  (fn [t-con]
                    (let [results (mapv (fn [validation]
                                         (process-row (:row-number validation)
                                                     (:data validation)))
                                       validations)

                          failures (filter #(not (:success %)) results)]

                      ;; If any failures, transaction will rollback
                      (if (seq failures)
                        {:success false
                         :error "Import failed during processing"
                         :failures failures}

                        ;; Success - collect statistics
                        (let [added (count (filter #(= :new-item (:action %)) results))
                              updated (count (filter #(= :update-item (:action %)) results))
                              removed (count (filter #(= :remove-item (:action %)) results))
                              unchanged (count (filter #(= :no-change (:action %)) results))]
                          {:success true
                           :stats {:total-rows (count rows)
                                  :added added
                                  :updated updated
                                  :unchanged unchanged
                                  :removed removed}})))))))))))
    (catch Exception e
      {:success false :error (str "Error processing CSV: " (.getMessage e))})))
