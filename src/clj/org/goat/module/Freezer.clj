(ns org.goat.module.Freezer
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.freezer :as db]
            [org.goat.module.freezer.parser :as parser]
            [org.goat.module.freezer.csv :as csv]
            [org.goat.module.freezer.csv-import :as csv-import]
            [org.goat.util.emoji :as emoji]
            [clojure.string :as str])
  (:import [java.time Instant LocalDate ZoneId]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; Response Formatting
;; ============================================================================

(defn format-success
  "Format a success message with emoji"
  [text]
  (str "✅ " text))

(defn format-error
  "Format an error message"
  [text]
  (str "❌ " text))

(defn format-quantity
  "Format a quantity, showing as integer if it's a whole number"
  [qty]
  (if (== qty (Math/floor qty))
    (str (int qty))
    (str qty)))

(defn format-item-age
  "Format item age for display"
  [days]
  (cond
    (= 0 days) "frozen today"
    (= 1 days) "frozen yesterday"
    :else (str "frozen " days " days ago")))

(defn format-expiry-date
  "Format expiry date for display"
  [expiry-ts]
  (when expiry-ts
    (let [instant (Instant/ofEpochMilli expiry-ts)
          date (.atZone instant (ZoneId/systemDefault))
          local-date (.toLocalDate date)
          formatter (DateTimeFormatter/ofPattern "MMM yyyy")]
      (.format local-date formatter))))

(defn days-until-expiry
  "Calculate days until expiry from timestamp"
  [expiry-ts]
  (when expiry-ts
    (let [instant (Instant/ofEpochMilli expiry-ts)
          expiry-date (.atZone instant (ZoneId/systemDefault))
          expiry-local (.toLocalDate expiry-date)
          now (LocalDate/now)
          days (.until now expiry-local java.time.temporal.ChronoUnit/DAYS)]
      days)))

(defn format-item-display
  "Format a single item for inventory display.
   Returns a string like '#1: 2x bags of peas 🫛 (frozen 3 days ago, expires Aug 2026)'
   Uses the actual database item_id for the # reference.
   Adds relevant emojis based on item name."
  [item]
  (let [quantity (:quantity item)
        unit (:unit item)
        name (:item_name item)
        notes (:notes item)
        expiry (:expiry_date item)
        item-id (:item_id item)
        age-days (or (db/get-item-age item-id) 0)
        age-str (format-item-age age-days)
        ;; Build the quantity+unit+name part
        item-desc (str (format-quantity quantity) "x"
                      (when unit (str " " unit " of"))
                      " " name)
        ;; Add notes if present
        full-desc (if notes
                    (str item-desc " (" notes ")")
                    item-desc)
        ;; Add emojis based on item name (emojification layer)
        emojified-desc (emoji/emojify full-desc)
        ;; Build metadata string
        metadata-parts [(str age-str)]
        metadata-parts (if expiry
                        (let [days-left (days-until-expiry expiry)
                              expiry-str (format-expiry-date expiry)]
                          (conj metadata-parts
                                (cond
                                  (< days-left 0) (str "⚠️ EXPIRED " expiry-str)
                                  (< days-left 30) (str "⚠️ expires " expiry-str " (" days-left " days)")
                                  :else (str "expires " expiry-str))))
                        metadata-parts)
        metadata (str/join ", " metadata-parts)]
    (str "#" item-id ": " emojified-desc " (" metadata ")")))

(defn format-inventory-single
  "Format inventory for a single freezer"
  [freezer items]
  (let [freezer-name (:freezer_name freezer)
        header (str "🧊 <b>" (str/capitalize freezer-name) " Freezer</b>")
        lines (if (empty? items)
                ["The freezer is empty! 🎉"]
                (map format-item-display items))]
    (str header "\n" (str/join "\n" lines))))

(defn format-inventory-all
  "Format inventory for all freezers"
  []
  (let [freezers (db/get-freezers)]
    (if (empty? freezers)
      "❄️ No freezers found. Add one with 'add freezer &lt;name&gt;'."
      (let [sections (for [freezer freezers]
                      (let [items (db/get-items (:freezer_id freezer))]
                        (format-inventory-single freezer items)))]
        (str/join "\n---\n" sections)))))

(defn format-freezer-list
  "Format list of freezers"
  [freezers user-default-id]
  (if (empty? freezers)
    "No freezers found."
    (let [lines (for [f freezers]
                  (str "• " (str/capitalize (:freezer_name f))
                       (when (= (:freezer_id f) user-default-id) " (your default)")))]
      (str "<b>🧊 Freezers:</b>\n" (str/join "\n" lines)))))

;; ============================================================================
;; Command Handlers
;; ============================================================================

(defn handle-add-item
  "Handle adding an item to a freezer.
   Supports adding by name (creates new or finds existing) or by ID (adds to specific item)."
  [m parsed]
  (let [user (msg/sender m)
        quantity (:quantity parsed)
        unit (:unit parsed)
        item-name (:item-name parsed)
        item-id (:item-id parsed)
        expiry-error (:expiry-error parsed)
        expiry-date (or (:expiry-date parsed) (parser/default-expiry-date))
        freezer-name (when (:freezer-name parsed)
                      (parser/normalize-freezer-name (:freezer-name parsed)))]

    (cond
      ;; Check for expiry parse error first
      expiry-error
      (msg/reply m (format-error
                    (str "I couldn't understand the expiry date \"" expiry-error "\". "
                         "Please use one of these formats:\n"
                         "  • UK date: 5/5/2025 (DD/MM/YYYY)\n"
                         "  • Month/Year: Aug 2026 or 2/26 (mm/yy)\n"
                         "  • Natural language: tomorrow, next week, in 3 months")))

      ;; Defensive check: prevent items with "expires" in the name
      (and item-name (re-find #"(?i)expires?" item-name))
      (msg/reply m (format-error
                    (str "Item names cannot contain the word 'expires'. "
                         "Did you mean to specify an expiry date? "
                         "Use format: add <item> expires <date>")))

      ;; Case 1: Adding by item ID
      item-id
      (let [item (db/get-item-by-id item-id)]
        (cond
          (not item)
          (msg/reply m (format-error (str "Item #" item-id " not found. Use 'inventory' to see all items.")))

          (:removed_date item)
          (msg/reply m (format-error (str "Item #" item-id " was previously removed and cannot be added to.")))

          :else
          (let [new-qty (db/update-item-quantity item-id quantity)]
            (msg/reply m (format-success
                          (str "Added " (format-quantity quantity)
                               (when (:unit item) (str " " (:unit item)))
                               " to " (emoji/emojify (:item_name item))
                               " (ID #" item-id "). "
                               "You now have " (format-quantity new-qty) "."))))))

      ;; Case 2: Adding by item name
      item-name
      (let [freezer (cond
                      freezer-name
                      (db/get-freezer-by-name freezer-name)

                      :else
                      (db/get-default-freezer user))]

        (cond
          (and freezer-name (not freezer))
          (msg/reply m (format-error (str "I can't find a freezer named \"" freezer-name "\". "
                                         "Use 'list freezers' to see all freezers.")))

          (not freezer)
          (msg/reply m (format-error (str "You don't have a default freezer set. "
                                       "Set one with 'set default freezer <name>'.")))

          :else
          (let [freezer-id (:freezer_id freezer)
                ;; Always create a new item when adding by name
                ;; (only add to existing item when using explicit ID like "add 1 to 2")
                item-id (db/add-item freezer-id item-name quantity unit nil expiry-date)]
            (if item-id
              (msg/reply m (format-success
                            (str "Added " (format-quantity quantity)
                                 (when unit (str " " unit " of"))
                                 " " (emoji/emojify item-name)
                                 " to the " (str/capitalize (:freezer_name freezer)) " Freezer.")))
              (msg/reply m (format-error "Failed to add item. Please try again."))))))

      ;; Case 3: Neither item-id nor item-name provided
      :else
      (msg/reply m (format-error "Please specify an item name or ID.")))))


(defn handle-remove-item
  "Handle removing an item from a freezer.
   Phase 4: Supports removal by ID or by name."
  [m parsed]
  (let [quantity (:quantity parsed)
        item-id (:item-id parsed)
        item-name (:item-name parsed)
        freezer-name (when (:freezer-name parsed)
                      (parser/normalize-freezer-name (:freezer-name parsed)))]

    (cond
      ;; Remove by ID
      item-id
      (let [item (db/get-item-by-id item-id)]
        (cond
          ;; Item not found
          (not item)
          (msg/reply m (format-error (str "I couldn't find item #" item-id ". "
                                         "Use 'inventory' to see available items.")))

          ;; Remove the item
          :else
          (let [freezer (db/get-freezer-by-id (:freezer_id item))
                current-qty (:quantity item)
                item-display-name (:item_name item)
                unit (:unit item)
                remaining (db/remove-item item-id quantity)]
            (cond
              ;; Removed all
              (= 0 remaining)
              (msg/reply m (str "👌 Removed the last "
                               (when unit (str unit " of "))
                               item-display-name
                               " from the " (str/capitalize (:freezer_name freezer)) " Freezer."))

              ;; Partial removal
              :else
              (msg/reply m (str "👌 Removed " (format-quantity quantity)
                               (when unit (str " " unit " of"))
                               " " item-display-name
                               " from the " (str/capitalize (:freezer_name freezer)) " Freezer. "
                               (format-quantity remaining) " remaining."))))))

      ;; Remove by name
      item-name
      (let [freezer (if freezer-name
                      (db/get-freezer-by-name freezer-name)
                      (db/get-default-freezer (msg/sender m)))]
        (cond
          (not freezer)
          (msg/reply m (format-error "Please specify which freezer, or set a default freezer."))

          :else
          (let [items (db/get-items (:freezer_id freezer))
                ;; Find matching items by name (case-insensitive)
                matches (filter #(= (str/lower-case item-name)
                                   (str/lower-case (:item_name %)))
                               items)]
            (cond
              ;; No matches
              (empty? matches)
              (msg/reply m (format-error (str "I couldn't find \"" item-name "\" in the "
                                             (str/capitalize (:freezer_name freezer)) " Freezer.")))

              ;; Multiple matches (different units)
              (> (count matches) 1)
              (msg/reply m (format-error (str "I found multiple items called \"" item-name "\". "
                                             "Please use the item ID from 'inventory' to be more specific.")))

              ;; Single match - remove it
              :else
              (let [item (first matches)
                    remaining (db/remove-item (:item_id item) quantity)]
                (cond
                  (= 0 remaining)
                  (msg/reply m (str "👌 Removed the last "
                                   (when (:unit item) (str (:unit item) " of "))
                                   item-name
                                   " from the " (str/capitalize (:freezer_name freezer)) " Freezer."))

                  :else
                  (msg/reply m (str "👌 Removed " (format-quantity quantity)
                                   (when (:unit item) (str " " (:unit item) " of"))
                                   " " item-name
                                   " from the " (str/capitalize (:freezer_name freezer)) " Freezer. "
                                   (format-quantity remaining) " remaining."))))))))

      ;; Neither ID nor name
      :else
      (msg/reply m (format-error "Please specify an item ID (e.g., #1) or item name.")))))

(defn handle-move-item
  "Handle moving an item from one freezer to another.
   Supports full move (move 3 to garage) and partial move (move 5 of 3 to kitchen)."
  [m parsed]
  (let [item-id (:item-id parsed)
        quantity (:quantity parsed)
        freezer-name (parser/normalize-freezer-name (:freezer-name parsed))]

    (cond
      ;; No item ID specified
      (not item-id)
      (msg/reply m (format-error "Please specify an item ID (e.g., 'move 3 to garage')."))

      ;; No freezer specified
      (not freezer-name)
      (msg/reply m (format-error "Please specify a target freezer (e.g., 'move 3 to garage')."))

      ;; Process the move
      :else
      (let [item (db/get-item-by-id item-id)
            target-freezer (db/get-freezer-by-name freezer-name)]
        (cond
          ;; Item not found
          (not item)
          (msg/reply m (format-error (str "Item #" item-id " not found. Use 'inventory' to see all items.")))

          ;; Item was previously removed
          (:removed_date item)
          (msg/reply m (format-error (str "Item #" item-id " was previously removed and cannot be moved.")))

          ;; Target freezer not found
          (not target-freezer)
          (msg/reply m (format-error (str "I can't find a freezer named \"" freezer-name "\". "
                                         "Use 'list freezers' to see all freezers.")))

          ;; Move entire item (no quantity specified)
          (not quantity)
          (let [source-freezer (db/get-freezer-by-id (:freezer_id item))]
            (if (= (:freezer_id source-freezer) (:freezer_id target-freezer))
              (msg/reply m (format-error (str "Item #" item-id " is already in the "
                                             (str/capitalize (:freezer_name target-freezer)) " Freezer.")))
              (do
                (db/move-item item-id (:freezer_id target-freezer))
                (msg/reply m (format-success
                              (str "Moved " (:item_name item)
                                   " (ID #" item-id ", "
                                   (format-quantity (:quantity item))
                                   (when (:unit item) (str " " (:unit item)))
                                   ") from " (str/capitalize (:freezer_name source-freezer))
                                   " to " (str/capitalize (:freezer_name target-freezer)) " Freezer."))))))

          ;; Move partial quantity
          :else
          (let [source-freezer (db/get-freezer-by-id (:freezer_id item))
                result (db/move-item-partial item-id quantity (:freezer_id target-freezer))]
            (if (:success result)
              (msg/reply m (format-success
                            (str "Moved " (format-quantity quantity)
                                 (when (:unit item) (str " " (:unit item)))
                                 " of " (:item_name item)
                                 " from " (str/capitalize (:freezer_name source-freezer))
                                 " to " (str/capitalize (:freezer_name target-freezer)) " Freezer. "
                                 (if (> (:remaining-qty result) 0)
                                   (str (format-quantity (:remaining-qty result)) " remaining in "
                                        (str/capitalize (:freezer_name source-freezer)) ".")
                                   (str "All moved from " (str/capitalize (:freezer_name source-freezer)) ".")))))
              (msg/reply m (format-error (:error result))))))))))

(defn handle-inventory
  "Handle inventory display.
   Phase 4: Always shows all freezers."
  [m parsed]
  (let [inventory-text (format-inventory-all)]
    (msg/reply m inventory-text)))

(defn handle-freezer-management
  "Handle freezer management commands (add/delete/rename/list/set default)"
  [m parsed]
  (let [action (:action parsed)
        user (msg/sender m)]

    (case action
      ;; Add freezer
      :add-freezer
      (let [freezer-name (parser/normalize-freezer-name (:freezer-name parsed))
            existing (db/get-freezer-by-name freezer-name)]
        (if existing
          (msg/reply m (format-error (str "A freezer named \"" freezer-name "\" already exists.")))
          (let [freezer-id (db/add-freezer freezer-name)]
            (if freezer-id
              (msg/reply m (format-success (str "New freezer \"" (str/capitalize freezer-name) "\" created.")))
              (msg/reply m (format-error "Failed to create freezer. Please try again."))))))

      ;; Delete freezer
      :delete-freezer
      (let [freezer-name (parser/normalize-freezer-name (:freezer-name parsed))
            freezer (db/get-freezer-by-name freezer-name)]
        (if (not freezer)
          (msg/reply m (format-error (str "I can't find a freezer named \"" freezer-name "\".")))
          (do
            (db/delete-freezer (:freezer_id freezer))
            (msg/reply m (format-success (str "Deleted freezer \"" (str/capitalize freezer-name)
                                             "\" and all its contents."))))))

      ;; Rename freezer
      :rename-freezer
      (let [old-name (parser/normalize-freezer-name (:old-name parsed))
            new-name (parser/normalize-freezer-name (:new-name parsed))
            freezer (db/get-freezer-by-name old-name)
            existing-new (db/get-freezer-by-name new-name)]
        (cond
          (not freezer)
          (msg/reply m (format-error (str "I can't find a freezer named \"" old-name "\".")))

          existing-new
          (msg/reply m (format-error (str "A freezer named \"" new-name "\" already exists.")))

          :else
          (do
            (db/rename-freezer (:freezer_id freezer) new-name)
            (msg/reply m (format-success (str "Renamed freezer \"" old-name "\" to \"" new-name "\"."))))))

      ;; List freezers
      :list-freezers
      (let [freezers (db/get-freezers)
            default (db/get-default-freezer user)
            default-id (when default (:freezer_id default))
            formatted (format-freezer-list freezers default-id)]
        (msg/reply m formatted))

      ;; Set default freezer
      :set-default-freezer
      (let [freezer-name (parser/normalize-freezer-name (:freezer-name parsed))
            freezer (db/get-freezer-by-name freezer-name)]
        (if (not freezer)
          (msg/reply m (format-error (str "I can't find a freezer named \"" freezer-name "\".")))
          (do
            (db/set-default-freezer user (:freezer_id freezer))
            (msg/reply m (format-success (str "\"" (str/capitalize freezer-name) "\" is now your default freezer."))))))

      ;; Default
      (msg/reply m (format-error "Unknown freezer management command.")))))

(defn handle-search
  "Handle search across all freezers.
   Uses the same emojified format as inventory."
  [m parsed]
  (let [search-term (:search-term parsed)
        results (db/search-items search-term)]
    (if (empty? results)
      (msg/reply m (str "🔍 No items found matching \"" search-term "\"."))
      (let [;; Group results by freezer for organized display
            grouped (group-by :freezer_name results)
            sections (for [[freezer-name items] (sort-by key grouped)]
                      (let [item-lines (for [item items]
                                        (str "  " (format-item-display item)))]
                        (str "<b>" (str/capitalize freezer-name) " Freezer:</b>\n"
                             (str/join "\n" item-lines))))
            result-text (str "🔍 <b>Found \"" search-term "\":</b>\n\n"
                            (str/join "\n\n" sections))]
        (msg/reply m result-text)))))

(defn handle-export-csv
  "Export all freezer inventory to CSV file"
  [m]
  (try
    (let [csv-data (csv/generate-csv-data)
          csv-byte-array (csv/csv-bytes csv-data)
          item-count (csv/count-items csv-data)
          filename (str "freezer-inventory-"
                       (.format (LocalDate/now)
                               (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
                       ".csv")]
      (msg/reply-document m csv-byte-array filename)
      (msg/reply m (format-success (str "Exported " item-count " items to " filename))))
    (catch Exception e
      (println "Error exporting CSV:" (.getMessage e))
      (.printStackTrace e)
      (msg/reply m (format-error (str "Failed to export CSV: " (.getMessage e)))))))

(defn format-import-success
  "Format successful import results"
  [stats]
  (let [{:keys [total-rows added updated removed]} stats]
    (str "✅ <b>Import Complete!</b>\n\n"
         "📊 Processed " total-rows " rows:\n"
         "• " added " items added\n"
         "• " updated " items updated\n"
         "• " removed " items removed\n\n"
         "Use <code>inventory</code> to see updated freezers.")))

(defn format-import-errors
  "Format import errors with detailed feedback"
  [result]
  (let [{:keys [error unknown-freezers validation-errors total-errors]} result
        all-freezers (map :freezer_name (db/get-freezers))]
    (str "❌ <b>Import Failed</b>\n\n"
         error "\n\n"
         ;; Special handling for unknown freezers
         (when (seq unknown-freezers)
           (str "<b>Unknown freezers found:</b>\n"
                (str/join "\n"
                  (map (fn [{:keys [name suggestion]}]
                         (if suggestion
                           (str "• \"" name "\" (did you mean \"" suggestion "\"?)")
                           (str "• \"" name "\"")))
                       unknown-freezers))
                "\n\n"
                "Please check for typos or create freezers first using:\n"
                "<code>add freezer &lt;name&gt;</code>\n\n"
                (when (seq all-freezers)
                  (str "Available freezers: " (str/join ", " (map str/capitalize all-freezers)) "\n\n"))))
         ;; Show validation errors
         (when (seq validation-errors)
           (str "<b>Row Errors:</b>\n"
                (str/join "\n"
                  (map #(str "Row " (:row-number %) ": " (:error %))
                       (take 10 validation-errors)))
                (when (> total-errors 10)
                  (str "\n... and " (- total-errors 10) " more errors")))))))

(defn handle-import-csv
  "Handle CSV import from uploaded file or prompt for file upload.
   Two modes:
   1. User uploads .csv file - auto-detect and import
   2. User types 'import csv' - prompt for file upload"
  [m]
  (cond
    ;; Mode 1: File uploaded
    (msg/has-document? m)
    (let [filename (msg/document-filename m)
          content-bytes (msg/document-bytes m)]
      (cond
        ;; Check file size (5MB limit)
        (> (count content-bytes) (* 5 1024 1024))
        (msg/reply m (format-error "CSV file too large. Maximum 5MB."))

        ;; Check file extension
        (not (str/ends-with? (str/lower-case filename) ".csv"))
        (msg/reply m (format-error "Please upload a CSV file (.csv extension)."))

        ;; Import the CSV
        :else
        (try
          (let [csv-string (String. content-bytes "UTF-8")
                result (csv-import/import-csv-data csv-string)]
            (if (:success result)
              (msg/reply m (format-import-success (:stats result)))
              (msg/reply m (format-import-errors result))))
          (catch Exception e
            (println "Error importing CSV:" (.getMessage e))
            (.printStackTrace e)
            (msg/reply m (format-error (str "Failed to import CSV: " (.getMessage e))))))))

    ;; Mode 2: Command without file - show instructions
    :else
    (msg/reply m (str "📤 <b>CSV Import Instructions</b>\n\n"
                     "Please upload a CSV file to import.\n\n"
                     "<b>CSV Format:</b>\n"
                     "<code>ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date</code>\n\n"
                     "<b>Import Rules:</b>\n"
                     "• No ID or unknown ID → Add as new item\n"
                     "• Existing ID + quantity > 0 → Update item\n"
                     "• Existing ID + quantity = 0 → Remove item\n"
                     "• Unknown freezer name → Reject (prevents typos)\n\n"
                     "💡 <b>Tip:</b> Export your current inventory first with:\n"
                     "<code>export csv</code>\n\n"
                     "Then edit the CSV and upload it to update your freezers."))))

(defn handle-help
  "Show help message for freezer commands"
  [m]
  (msg/reply m
    (str "<b>🧊 Freezer Management Help</b>\n\n"
         "<b>Basic Commands:</b>\n"
         "• <code>add 2 bags of peas</code> - Add items\n"
         "• <code>add 3 bags of peas to garage</code> - Add to specific freezer\n"
         "• <code>add 5 portions of stew expires 3/5/26</code> - Add with expiry date (UK format)\n"
         "• <code>add 3 of 5</code> or <code>add 3 to 5</code> - Add to existing item by ID\n"
         "• <code>inventory</code> - See all freezers\n"
         "• <code>take 1 of 2</code> or <code>take 1 of #2</code> - Remove by ID\n"
         "• <code>remove peas</code> - Remove by name\n"
         "• <code>move 3 to garage</code> - Move entire item to another freezer\n"
         "• <code>move 5 of 3 to kitchen</code> - Move partial quantity to another freezer\n"
         "• <code>find chicken</code> - Search across freezers (also try: <i>fish</i>, <i>dinners</i>, <i>bakery</i>)\n\n"
         "<b>CSV Import/Export:</b>\n"
         "• <code>export csv</code> - Export all items to CSV file\n"
         "• <code>import csv</code> - Import items from CSV (or just upload a .csv file)\n\n"
         "<b>Freezer Management:</b>\n"
         "• <code>add freezer garage</code> - Create new freezer\n"
         "• <code>delete freezer garage</code> - Delete freezer\n"
         "• <code>rename freezer garage to big-garage</code> - Rename\n"
         "• <code>list freezers</code> - Show all freezers\n"
         "• <code>set default freezer garage</code> - Set your default\n\n"
         "<i>All freezers are shared across the family!</i>")))

;; ============================================================================
;; Module Definition
;; ============================================================================

(defmodule Freezer
  :commands [:freezer]
  :receive-messages :all
  :wants-private true

  (defn process-channel-message [m]
    (let [command (msg/command m)
          text (if (= :freezer command)
                 (msg/mod-text m)
                 (msg/text m))]

      (if (= :freezer command)
        (handle-help m)
        (if (msg/has-document? m)
          (handle-import-csv m)
          (when text
            (let [parsed (parser/parse-command text)]
              (when parsed
                (case (:type parsed)
                  :add-item (handle-add-item m parsed)
                  :remove-item (handle-remove-item m parsed)
                  :move-item (handle-move-item m parsed)
                  :inventory (handle-inventory m parsed)
                  :freezer-management (handle-freezer-management m parsed)
                  :search (handle-search m parsed)
                  :export-csv (handle-export-csv m)
                  :import-csv (handle-import-csv m)
                  nil)))))))))
