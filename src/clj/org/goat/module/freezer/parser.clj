(ns org.goat.module.freezer.parser
  (:require [clojure.string :as str]
            [org.goat.util.date :as date-util])
  (:import [java.time LocalDate YearMonth]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; Date/Expiry Helpers
;; ============================================================================

(def month-names
  {"jan" 1 "january" 1
   "feb" 2 "february" 2
   "mar" 3 "march" 3
   "apr" 4 "april" 4
   "may" 5
   "jun" 6 "june" 6
   "jul" 7 "july" 7
   "aug" 8 "august" 8
   "sep" 9 "september" 9 "sept" 9
   "oct" 10 "october" 10
   "nov" 11 "november" 11
   "dec" 12 "december" 12})

(defn parse-month-name
  "Parse month name to number (1-12). Case insensitive."
  [month-str]
  (get month-names (str/lower-case month-str)))

(defn parse-expiry-date
  "Parse expiry date from various formats using Hawking ML library (UK date format DD/MM/YYYY).
   Also supports legacy formats:
   - UK dates: '5/5/2025', '25/12/2025' (DD/MM/YYYY)
   - Natural language: 'tomorrow', 'next week', 'in 3 months'
   - Month/year: 'Aug 2026' or 'August 2026'
   - Numeric month/year: '10/2027' or '10-2027'
   Returns a timestamp (milliseconds), or nil if invalid."
  [date-str]
  (let [date-str (str/trim date-str)]
    (try
      ;; First try Hawking for maximum flexibility (UK dates, natural language)
      (if-let [timestamp (date-util/parse-date-timestamp date-str)]
        timestamp
        ;; Fallback to legacy month/year parsing
        (cond
          ;; Month name + year: "Aug 2026" or "August 2026"
          (re-matches #"(?i)([a-z]+)\s+(\d{4})" date-str)
          (let [match (re-matches #"(?i)([a-z]+)\s+(\d{4})" date-str)
                month-str (nth match 1)
                year (Integer/parseInt (nth match 2))
                month (parse-month-name month-str)]
            (when month
              (let [year-month (YearMonth/of year month)
                    last-day (.atEndOfMonth year-month)]
                (* (.toEpochDay last-day) 24 60 60 1000))))

          ;; Numeric format: "10/2027" or "10-2027" (month/year only)
          (re-matches #"(\d{1,2})[/-](\d{4})" date-str)
          (let [match (re-matches #"(\d{1,2})[/-](\d{4})" date-str)
                month (Integer/parseInt (nth match 1))
                year (Integer/parseInt (nth match 2))]
            (when (<= 1 month 12)
              (let [year-month (YearMonth/of year month)
                    last-day (.atEndOfMonth year-month)]
                (* (.toEpochDay last-day) 24 60 60 1000))))

          :else nil))
      (catch Exception e
        nil))))

(defn extract-expiry
  "Extract expiry date from text like:
   - 'expires 5/5/2025' (UK date format DD/MM/YYYY)
   - 'expires Aug 2026' (month/year)
   - 'expires tomorrow' (natural language)
   Returns [expiry-timestamp remaining-text] or [nil text] if not found."
  [text]
  (let [text (str/trim text)
        ;; Match "expires <date>" at the end
        expiry-pattern #"(?i)(.+?)\s+expires?\s+(.+?)$"
        match (re-matches expiry-pattern text)]
    (if match
      (let [remaining-text (str/trim (nth match 1))
            expiry-str (str/trim (nth match 2))
            expiry-ts (parse-expiry-date expiry-str)]
        (if expiry-ts
          [expiry-ts remaining-text]
          [nil text])) ; Invalid date format, return original
      [nil text])))

(defn default-expiry-date
  "Get default expiry date (1 year from now)"
  []
  (let [now (LocalDate/now)
        one-year-later (.plusYears now 1)
        year-month (YearMonth/from one-year-later)
        last-day (.atEndOfMonth year-month)]
    (* (.toEpochDay last-day) 24 60 60 1000)))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn extract-quantity
  "Extract quantity from text. Returns [quantity remaining-text] or [1 text] if none found.
   Handles patterns like '2', '2.5', '1/2', etc."
  [text]
  (let [text (str/trim text)
        ;; Try to match a number at the start
        number-pattern #"^(\d+\.?\d*|\d*\.\d+)\s*(.*)"
        match (re-matches number-pattern text)]
    (if match
      [(Double/parseDouble (nth match 1)) (str/trim (nth match 2))]
      [1.0 text])))

(defn extract-unit
  "Extract unit from text. Returns [unit remaining-text] or [nil text] if none found.
   Common units: bags, boxes, tubs, pieces, fillets, loaves, etc."
  [text]
  (let [text (str/trim text)
        ;; Common units pattern - match singular or plural
        unit-pattern #"^(bags?|boxes?|tubs?|pieces?|fillets?|loaves|loaf|containers?|packages?|jars?|bottles?|cans?)\s+(of\s+)?(.*)"
        match (re-matches unit-pattern (str/lower-case text))]
    (if match
      [(nth match 1) (str/trim (nth match 3))]
      [nil text])))

(defn extract-item-id
  "Extract item ID from text like '#1', '#23', or just '1', '23', etc.
   Returns the numeric ID or nil if not found."
  [text]
  (let [text (str/trim text)]
    (cond
      ;; Try #ID format first (e.g., #1, #23)
      (re-matches #"#(\d+)" text)
      (Integer/parseInt (second (re-matches #"#(\d+)" text)))

      ;; Try just a number (e.g., 1, 23)
      (re-matches #"(\d+)" text)
      (Integer/parseInt text)

      ;; Otherwise nil
      :else nil)))

(defn normalize-freezer-name
  "Normalize freezer name: trim, lowercase for comparison"
  [name]
  (when name
    (-> name
        str/trim
        str/lower-case)))

(defn extract-freezer-name
  "Extract freezer name from text after 'to', 'in', 'from' keywords.
   Returns [freezer-name remaining-text] or [nil text] if none found."
  [text]
  (let [text (str/trim text)
        ;; Match patterns like "to garage", "in kitchen", "from upstairs"
        ;; Use \b word boundaries to avoid matching "to" inside "tomato"
        freezer-pattern #"(?i)\b(to|in|from)\s+(?:the\s+)?([a-zA-Z][\w\s-]*?)(?:\s+freezer)?\s*$"
        match (re-find freezer-pattern text)]
    (if match
      (let [freezer-name (str/trim (nth match 2))
            ;; Remove the freezer part from text
            text-without-freezer (str/trim (str/replace text (re-pattern (str "(?i)\\b" (nth match 0) "$")) ""))]
        [freezer-name text-without-freezer])
      [nil text])))

;; ============================================================================
;; Command Parsers
;; ============================================================================

(defn parse-add-command
  "Parse an add command with optional expiry date:
   - 'add 2 bags of peas' (no expiry)
   - 'add 2 bags of peas expires 5/5/2025' (UK date format)
   - 'add 2 bags of peas expires Aug 2026' (month/year)
   - 'add 2 bags of peas to garage expires tomorrow' (natural language)
   - 'add 3 of 5' or 'add 2 to 5' (add to existing item by ID)
   Returns a map with :quantity, :unit, :item-name/:item-id, :freezer-name, :expiry-date
   or nil if not a valid add command."
  [text]
  (let [text (str/trim text)
        ;; Check for add keywords at start
        add-pattern #"^(?i)(add|put|store|freeze)\s+(.+)"
        match (re-matches add-pattern text)]
    (when match
      (let [rest-text (nth match 2)
            ;; Extract expiry first (before freezer name since both are at the end)
            [expiry-ts text-without-expiry] (extract-expiry rest-text)
            ;; Extract freezer name (from the end)
            [freezer-name text-without-freezer] (extract-freezer-name text-without-expiry)
            ;; Extract quantity
            [quantity text-after-qty] (extract-quantity text-without-freezer)
            ;; Extract unit
            [unit text-after-unit] (extract-unit text-after-qty)
            ;; Remove "of" or "to" if present (both work for adding by ID)
            item-text (str/trim (str/replace text-after-unit #"^(of|to)\s+" ""))
            ;; Check if it's an item ID (like #5 or 5)
            item-id (extract-item-id item-text)
            ;; If not an ID, what's left is the item name
            item-name (when (and (not item-id) (not (str/blank? item-text))) item-text)]
        (when (or item-id item-name)
          (merge
            {:quantity quantity
             :unit unit
             :freezer-name freezer-name}
            (if item-id
              {:item-id item-id}
              {:item-name item-name})
            (when expiry-ts
              {:expiry-date expiry-ts})))))))

(defn parse-remove-command
  "Parse a remove command like 'take 2 of #1' or 'remove chicken from garage'.
   Returns a map with :quantity, :item-id or :item-name, :freezer-name
   or nil if not a valid remove command."
  [text]
  (let [text (str/trim text)
        ;; Check for remove keywords at start
        remove-pattern #"^(?i)(take|remove|use|get)\s+(.+)"
        match (re-matches remove-pattern text)]
    (when match
      (let [rest-text (nth match 2)
            ;; Extract freezer name first (from the end)
            [freezer-name text-without-freezer] (extract-freezer-name rest-text)
            ;; Extract quantity
            [quantity text-after-qty] (extract-quantity text-without-freezer)
            ;; Remove "of" if present
            item-text (str/trim (str/replace text-after-qty #"^of\s+" ""))
            ;; Check if it's an item ID
            item-id (extract-item-id item-text)
            ;; If not an ID, it's an item name
            item-name (when (and (not item-id) (not (str/blank? item-text))) item-text)]
        (when (or item-id item-name)
          (merge
            {:quantity quantity
             :freezer-name freezer-name}
            (if item-id
              {:item-id item-id}
              {:item-name item-name})))))))

(defn parse-inventory-command
  "Parse an inventory command like 'inventory garage' or 'list all'.
   Returns a map with :freezer-name or :all true, or nil if not valid."
  [text]
  (let [text (str/trim text)
        ;; Check for inventory keywords at start
        inv-pattern #"^(?i)(inventory|list|show|check|what'?s?\s+in)\s*(.*)"
        match (re-matches inv-pattern text)]
    (when match
      (let [rest-text (str/trim (nth match 2))]
        (cond
          ;; Check for "all"
          (re-matches #"(?i)all" rest-text)
          {:all true}

          ;; Check for freezer name
          (not (str/blank? rest-text))
          (let [;; Remove "freezer" suffix if present
                freezer-name (str/trim (str/replace rest-text #"(?i)\s+freezer\s*$" ""))]
            {:freezer-name freezer-name})

          ;; No specific freezer - use default/context
          :else
          {})))))

(defn parse-freezer-command
  "Parse freezer management commands like:
   - 'add freezer garage' / 'create freezer kitchen'
   - 'delete freezer garage' / 'remove freezer kitchen'
   - 'rename freezer garage to big-garage'
   - 'list freezers'
   - 'set default freezer garage'

   Returns a map with :action and relevant parameters."
  [text]
  (let [text (str/trim text)]
    (or
      ;; List freezers
      (when (re-matches #"(?i)list\s+freezers?" text)
        {:action :list-freezers})

      ;; Add/create freezer
      (when-let [match (re-matches #"(?i)(add|create)\s+freezer\s+(.+)" text)]
        {:action :add-freezer
         :freezer-name (str/trim (nth match 2))})

      ;; Delete/remove freezer
      (when-let [match (re-matches #"(?i)(delete|remove)\s+freezer\s+(.+)" text)]
        {:action :delete-freezer
         :freezer-name (str/trim (nth match 2))})

      ;; Rename freezer
      (when-let [match (re-matches #"(?i)rename\s+freezer\s+(.+?)\s+to\s+(.+)" text)]
        {:action :rename-freezer
         :old-name (str/trim (nth match 1))
         :new-name (str/trim (nth match 2))})

      ;; Set default freezer
      (when-let [match (re-matches #"(?i)set\s+default\s+freezer\s+(.+)" text)]
        {:action :set-default-freezer
         :freezer-name (str/trim (nth match 1))}))))

(defn parse-search-command
  "Parse a search command like 'find chicken' or 'search peas'.
   Returns a map with :search-term or nil if not valid."
  [text]
  (let [text (str/trim text)
        ;; Check for search keywords at start
        search-pattern #"^(?i)(find|search)\s+(.+)"
        match (re-matches search-pattern text)]
    (when match
      (let [search-term (str/trim (nth match 2))]
        (when (not (str/blank? search-term))
          {:search-term search-term})))))

(defn parse-export-command
  "Parse an export command like 'export csv' or 'export' or 'download csv'.
   Returns an empty map (just a flag) or nil if not valid."
  [text]
  (let [text (str/trim text)]
    (when (re-matches #"(?i)^(export|download)(\s+(csv|inventory))?$" text)
      {})))

(defn parse-import-command
  "Parse an import command like 'import csv' or 'import' or 'upload csv'.
   Returns an empty map (just a flag) or nil if not valid."
  [text]
  (let [text (str/trim text)]
    (when (re-matches #"(?i)^import(\s+(csv|inventory|freezer))?$" text)
      {})))

(defn parse-move-command
  "Parse a move command like 'move 3 to garage' or 'move 5 of 3 to kitchen'.
   - 'move 3 to garage' - Move entire item #3 to garage
   - 'move 5 of 3 to kitchen' - Move 5 portions from item #3 to kitchen
   Returns a map with :item-id, :freezer-name, and optionally :quantity
   or nil if not a valid move command."
  [text]
  (let [text (str/trim text)
        ;; Check for move keyword at start
        move-pattern #"^(?i)move\s+(.+)"
        match (re-matches move-pattern text)]
    (when match
      (let [rest-text (nth match 1)
            ;; Extract freezer name first (from the end, after "to")
            [freezer-name text-without-freezer] (extract-freezer-name rest-text)]
        (when freezer-name
          ;; Check if there's an "of" keyword to distinguish patterns
          (if (re-find #"(?i)\s+of\s+" text-without-freezer)
            ;; Pattern: "move 5 of 3" - quantity then item ID
            (let [[quantity text-after-qty] (extract-quantity text-without-freezer)
                  item-text (str/trim (str/replace text-after-qty #"^of\s+" ""))
                  item-id (extract-item-id item-text)]
              (when item-id
                {:item-id item-id
                 :quantity quantity
                 :freezer-name freezer-name}))
            ;; Pattern: "move 3" - just item ID, no quantity
            (let [item-id (extract-item-id (str/trim text-without-freezer))]
              (when item-id
                {:item-id item-id
                 :freezer-name freezer-name}))))))))

;; ============================================================================
;; Main Parser
;; ============================================================================

(defn parse-command
  "Main parser that tries all command types and returns the first match.
   Returns a map with :type and command-specific data, or nil if no match."
  [text]
  (let [text (str/trim text)]
    (or
      ;; Try freezer management commands first (more specific)
      (when-let [result (parse-freezer-command text)]
        (assoc result :type :freezer-management))

      ;; Try export command
      (when-let [result (parse-export-command text)]
        (assoc result :type :export-csv))

      ;; Try import command
      (when-let [result (parse-import-command text)]
        (assoc result :type :import-csv))

      ;; Try add command
      (when-let [result (parse-add-command text)]
        (assoc result :type :add-item))

      ;; Try remove command
      (when-let [result (parse-remove-command text)]
        (assoc result :type :remove-item))

      ;; Try move command
      (when-let [result (parse-move-command text)]
        (assoc result :type :move-item))

      ;; Try inventory command
      (when-let [result (parse-inventory-command text)]
        (assoc result :type :inventory))

      ;; Try search command
      (when-let [result (parse-search-command text)]
        (assoc result :type :search)))))
