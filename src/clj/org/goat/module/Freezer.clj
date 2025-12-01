(ns org.goat.module.Freezer
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.freezer :as db]
            [org.goat.module.freezer.parser :as parser]
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
   Returns a string like '#1: 2x bags of peas (frozen 3 days ago, expires Aug 2026)'
   Uses the actual database item_id for the # reference."
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
    (str "#" item-id ": " full-desc " (" metadata ")")))

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
      "❄️ No freezers found. Add one with 'add freezer <name>'."
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
   Phase 5: Expiry date support with 1-year default."
  [m parsed]
  (let [user (msg/sender m)
        quantity (:quantity parsed)
        unit (:unit parsed)
        item-name (:item-name parsed)
        expiry-date (or (:expiry-date parsed) (parser/default-expiry-date))
        freezer-name (when (:freezer-name parsed)
                      (parser/normalize-freezer-name (:freezer-name parsed)))]

    ;; Determine which freezer to use
    (let [freezer (cond
                    ;; Explicit freezer name provided
                    freezer-name
                    (db/get-freezer-by-name freezer-name)

                    ;; Use default freezer
                    :else
                    (db/get-default-freezer user))]

      (cond
        ;; No freezer found and name was specified
        (and freezer-name (not freezer))
        (msg/reply m (format-error (str "I can't find a freezer named \"" freezer-name "\". "
                                       "Use 'list freezers' to see all freezers.")))

        ;; No default freezer
        (not freezer)
        (msg/reply m (format-error (str "You don't have a default freezer set. "
                                     "Set one with 'set default freezer <name>'.")))

        ;; Add or update the item
        :else
        (let [freezer-id (:freezer_id freezer)
              ;; Check if item already exists
              existing-item (db/find-matching-item freezer-id item-name unit)]

          (if existing-item
            ;; Update existing item quantity
            (let [new-qty (db/update-item-quantity (:item_id existing-item) quantity)]
              (msg/reply m (format-success
                            (str "Added " (format-quantity quantity)
                                 (when unit (str " " unit " of"))
                                 " " item-name
                                 " to the " (str/capitalize (:freezer_name freezer)) " Freezer. "
                                 "You now have " (format-quantity new-qty) "."))))

            ;; Create new item
            (let [item-id (db/add-item freezer-id item-name quantity unit nil expiry-date)]
              (if item-id
                (msg/reply m (format-success
                              (str "Added " (format-quantity quantity)
                                   (when unit (str " " unit " of"))
                                   " " item-name
                                   " to the " (str/capitalize (:freezer_name freezer)) " Freezer.")))
                (msg/reply m (format-error "Failed to add item. Please try again."))))))))))

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
  "Handle search across all freezers"
  [m parsed]
  (let [search-term (:search-term parsed)
        results (db/search-items search-term)]
    (if (empty? results)
      (msg/reply m (str "🔍 No items found matching \"" search-term "\"."))
      (let [lines (for [item results]
                    (str "• " (:item_name item)
                         (when (:unit item) (str " (" (:unit item) ")"))
                         " in <b>" (str/capitalize (:freezer_name item)) "</b>"
                         " - " (format-quantity (:quantity item)) "x"))]
        (msg/reply m (str "🔍 <b>Found \"" search-term "\":</b>\n" (str/join "\n" lines)))))))

(defn handle-help
  "Show help message for freezer commands"
  [m]
  (msg/reply m
    (str "<b>🧊 Freezer Management Help</b>\n\n"
         "<b>Basic Commands:</b>\n"
         "• <code>add 2 bags of peas</code> - Add items\n"
         "• <code>add 3 bags of peas to garage</code> - Add to specific freezer\n"
         "• <code>add 5 portions of stew expires 3/5/26</code> - Add with expiry date (UK format)\n"
         "• <code>inventory</code> - See all freezers\n"
         "• <code>take 1 of #2</code> - Remove by ID\n"
         "• <code>remove peas</code> - Remove by name\n"
         "• <code>find chicken</code> - Search across freezers\n\n"
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
          ;; Use mod-text only if command is :freezer, otherwise use full text
          text (if (= :freezer command)
                 (msg/mod-text m)
                 (msg/text m))]

      ;; Check for help command first
      (if (= :freezer command)
        (handle-help m)

        ;; Try to parse natural language
        (when text
          (let [parsed (parser/parse-command text)]
            (when parsed
              (case (:type parsed)
                :add-item (handle-add-item m parsed)

                :remove-item (handle-remove-item m parsed)

                :inventory (handle-inventory m parsed)

                :freezer-management (handle-freezer-management m parsed)

                :search (handle-search m parsed)

                ;; Default
                nil))))))))
