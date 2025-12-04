(ns org.goat.module.Freezer-test
  "Integration tests for Freezer module using message mocking"
  (:require [org.goat.module.Freezer :as sut]
            [org.goat.db.freezer :as db]
            [org.goat.core.message :as core-msg]
            [org.goat.testutils.message :as msg-utils]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as sql])
  (:import [java.io File]))

;; ============================================================================
;; Test Database Setup
;; ============================================================================

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-freezer.db"})

(defn setup-test-db []
  (let [db-file (File. "test/resources/test-freezer.db")]
    (when (.exists db-file) (.delete db-file)))

  ;; Enable foreign key constraints
  (sql/execute! test-db "PRAGMA foreign_keys = ON")

  ;; Create freezers table
  (sql/db-do-commands test-db
    "create table freezers (
       freezer_id integer primary key autoincrement,
       freezer_name text not null unique collate NOCASE,
       created_at datetime
     )")

  ;; Create items table
  (sql/db-do-commands test-db
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
  (sql/execute! test-db "create index item_freezer_idx on items(freezer_id)")

  ;; Create user_context table
  (sql/db-do-commands test-db
    "create table user_context (
       user_id text primary key,
       default_freezer_id integer,
       last_command_time datetime,
       foreign key (default_freezer_id) references freezers(freezer_id) on delete set null
     )"))

(defn teardown-test-db []
  (let [db-file (File. "test/resources/test-freezer.db")]
    (when (.exists db-file) (.delete db-file))))

(defn db-fixture [f]
  (setup-test-db)
  (msg-utils/clear-replies!)
  (with-redefs [db/db test-db]
    (f))
  (teardown-test-db))

(use-fixtures :each db-fixture)

;; ============================================================================
;; Freezer Management Tests
;; ============================================================================

(deftest test-add-freezer-command
  (testing "Adding a freezer creates it in database"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "add freezer garage" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        ;; Verify success response
        (is (msg-utils/replied-with? "✅"))
        (is (msg-utils/replied-with? "Garage"))
        (is (msg-utils/replied-with? "created"))

        ;; Verify database
        (let [freezers (db/get-freezers)]
          (is (= 1 (count freezers)))
          (is (= "garage" (:freezer_name (first freezers)))))))))

(deftest test-add-duplicate-freezer-fails
  (testing "Adding duplicate freezer returns error"
    (msg-utils/with-clean-replies
      (db/add-freezer "garage")
      (let [msg (msg-utils/mock-message {:text "add freezer garage" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❌"))
        (is (msg-utils/replied-with? "already exists"))))))

(deftest test-list-freezers-empty
  (testing "Listing freezers when none exist"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "list freezers" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "No freezers found"))))))

(deftest test-list-freezers-with-data
  (testing "Listing freezers shows all freezers"
    (msg-utils/with-clean-replies
      (db/add-freezer "garage")
      (db/add-freezer "kitchen")
      (let [msg (msg-utils/mock-message {:text "list freezers" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "🧊 Freezers"))
        (is (msg-utils/replied-with? "Garage"))
        (is (msg-utils/replied-with? "Kitchen"))))))

(deftest test-set-default-freezer
  (testing "Setting default freezer for user"
    (msg-utils/with-clean-replies
      (db/add-freezer "garage")
      (let [msg (msg-utils/mock-message {:text "set default freezer garage" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "✅"))
        (is (msg-utils/replied-with? "default freezer"))

        ;; Verify in database
        (let [default (db/get-default-freezer "alice")]
          (is (some? default))
          (is (= "garage" (:freezer_name default))))))))

(deftest test-delete-freezer
  (testing "Deleting a freezer removes it and its items"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/add-item freezer-id "peas" 2 "bags" nil)

        (let [msg (msg-utils/mock-message {:text "delete freezer garage" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "✅"))
          (is (msg-utils/replied-with? "Deleted"))

          ;; Verify freezer is gone
          (is (nil? (db/get-freezer-by-name "garage")))
          ;; Verify items are gone (cascade delete)
          (is (empty? (db/get-items freezer-id))))))))

(deftest test-rename-freezer
  (testing "Renaming a freezer"
    (msg-utils/with-clean-replies
      (db/add-freezer "garage")
      (let [msg (msg-utils/mock-message {:text "rename freezer garage to big-garage" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "✅"))
        (is (msg-utils/replied-with? "Renamed"))

        ;; Verify old name is gone
        (is (nil? (db/get-freezer-by-name "garage")))
        ;; Verify new name exists
        (is (some? (db/get-freezer-by-name "big-garage")))))))

;; ============================================================================
;; Add Item Tests
;; ============================================================================

(deftest test-add-item-to-default-freezer
  (testing "Adding an item to default freezer"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/set-default-freezer "alice" freezer-id)

        (let [msg (msg-utils/mock-message {:text "add 2 bags of peas" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "✅"))
          (is (msg-utils/replied-with? "Added 2"))
          (is (msg-utils/replied-with? "bags of peas"))
          (is (msg-utils/replied-with? "Garage Freezer"))

          ;; Verify database
          (let [items (db/get-items freezer-id)]
            (is (= 1 (count items)))
            (let [item (first items)]
              (is (= "peas" (:item_name item)))
              (is (= 2.0 (:quantity item)))
              (is (= "bags" (:unit item)))
              ;; Should have default expiry (1 year from now)
              (is (some? (:expiry_date item))))))))))

(deftest test-add-item-to-specific-freezer
  (testing "Adding an item to a specific freezer"
    (msg-utils/with-clean-replies
      (db/add-freezer "garage")
      (db/add-freezer "kitchen")

      (let [msg (msg-utils/mock-message {:text "add 3 bags of peas to kitchen" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "✅"))
        (is (msg-utils/replied-with? "Kitchen Freezer"))

        ;; Verify in correct freezer
        (let [kitchen (db/get-freezer-by-name "kitchen")
              items (db/get-items (:freezer_id kitchen))]
          (is (= 1 (count items)))
          (is (= "peas" (:item_name (first items)))))))))

(deftest test-add-item-with-expiry-month-name
  (testing "Adding an item with month-name expiry"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/set-default-freezer "alice" freezer-id)

        (let [msg (msg-utils/mock-message {:text "add 2 bags of peas expires Aug 2026" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "✅"))

          ;; Verify expiry date in database
          (let [items (db/get-items freezer-id)
                item (first items)]
            (is (some? (:expiry_date item)))
            ;; Verify it's August 2026 (timestamp for Aug 31, 2026)
            (is (= 1788134400000 (:expiry_date item)))))))))

(deftest test-add-item-with-expiry-numeric
  (testing "Adding an item with numeric expiry format"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/set-default-freezer "alice" freezer-id)

        (let [msg (msg-utils/mock-message {:text "add chicken expires 10/2027" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "✅"))

          ;; Verify expiry date
          (let [items (db/get-items freezer-id)
                item (first items)]
            (is (some? (:expiry_date item)))
            ;; Verify it's October 2027
            (is (= 1824940800000 (:expiry_date item)))))))))

(deftest test-add-item-updates-quantity
  (testing "Adding duplicate item updates quantity"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/set-default-freezer "alice" freezer-id)
        (db/add-item freezer-id "peas" 2 "bags" nil)

        (let [msg (msg-utils/mock-message {:text "add 3 bags of peas" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "✅"))
          (is (msg-utils/replied-with? "You now have 5"))

          ;; Verify only one item with updated quantity
          (let [items (db/get-items freezer-id)]
            (is (= 1 (count items)))
            (is (= 5.0 (:quantity (first items))))))))))

(deftest test-add-item-no-default-freezer-fails
  (testing "Adding item without default freezer fails"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "add peas" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❌"))
        (is (msg-utils/replied-with? "don't have a default freezer"))))))

;; ============================================================================
;; Inventory Tests
;; ============================================================================

(deftest test-inventory-empty
  (testing "Inventory with no freezers"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "inventory" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❄️"))
        (is (msg-utils/replied-with? "No freezers found"))))))

(deftest test-inventory-empty-freezer
  (testing "Inventory with empty freezer"
    (msg-utils/with-clean-replies
      (db/add-freezer "garage")
      (let [msg (msg-utils/mock-message {:text "inventory" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "🧊"))
        (is (msg-utils/replied-with? "Garage Freezer"))
        (is (msg-utils/replied-with? "empty"))))))

(deftest test-inventory-with-items
  (testing "Inventory shows all items across all freezers"
    (msg-utils/with-clean-replies
      (let [garage-id (db/add-freezer "garage")
            kitchen-id (db/add-freezer "kitchen")]
        (db/add-item garage-id "peas" 2 "bags" nil)
        (db/add-item kitchen-id "chicken" 4 nil nil)

        (let [msg (msg-utils/mock-message {:text "inventory" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "🧊"))
          (is (msg-utils/replied-with? "Garage Freezer"))
          (is (msg-utils/replied-with? "Kitchen Freezer"))
          (is (msg-utils/replied-with? "2x bags of peas"))
          (is (msg-utils/replied-with? "4x chicken"))
          (is (msg-utils/replied-with? "#1:")))))))  ; Each freezer numbers items from #1

(deftest test-inventory-shows-expiry
  (testing "Inventory displays expiry dates"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")
            expiry-ts 1788134400000] ; Aug 2026
        (db/add-item freezer-id "peas" 2 "bags" nil expiry-ts)

        (let [msg (msg-utils/mock-message {:text "inventory" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "expires"))
          (is (msg-utils/replied-with? "Aug 2026")))))))

;; ============================================================================
;; Remove Item Tests
;; ============================================================================

(deftest test-remove-item-by-id
  (testing "Removing an item by ID"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")
            item-id (db/add-item freezer-id "peas" 5 "bags" nil)]

        (let [msg (msg-utils/mock-message {:text "take 2 of #1" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "👌"))
          (is (msg-utils/replied-with? "Removed 2"))
          (is (msg-utils/replied-with? "3 remaining"))

          ;; Verify database
          (let [item (db/get-item-by-id item-id)]
            (is (= 3.0 (:quantity item)))))))))

(deftest test-remove-all-of-item
  (testing "Removing all of an item deletes it"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")
            item-id (db/add-item freezer-id "peas" 2 "bags" nil)]

        (let [msg (msg-utils/mock-message {:text "take 2 of #1" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "👌"))
          (is (msg-utils/replied-with? "Removed the last"))

          ;; Verify item is deleted
          (is (nil? (db/get-item-by-id item-id))))))))

(deftest test-remove-item-by-name
  (testing "Removing an item by name"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/set-default-freezer "alice" freezer-id)
        (db/add-item freezer-id "peas" 5 "bags" nil)

        (let [msg (msg-utils/mock-message {:text "remove 2 peas" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "👌"))
          (is (msg-utils/replied-with? "Removed 2"))
          (is (msg-utils/replied-with? "3 remaining")))))))

(deftest test-remove-item-not-found
  (testing "Removing non-existent item fails gracefully"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "take #99" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❌"))
        (is (msg-utils/replied-with? "couldn't find"))))))

;; ============================================================================
;; Search Tests
;; ============================================================================

(deftest test-search-finds-items
  (testing "Search finds items across freezers"
    (msg-utils/with-clean-replies
      (let [garage-id (db/add-freezer "garage")
            kitchen-id (db/add-freezer "kitchen")]
        (db/add-item garage-id "chicken breasts" 4 nil nil)
        (db/add-item kitchen-id "chicken wings" 2 "bags" nil)

        (let [msg (msg-utils/mock-message {:text "find chicken" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "🔍"))
          (is (msg-utils/replied-with? "chicken breasts"))
          (is (msg-utils/replied-with? "chicken wings"))
          (is (msg-utils/replied-with? "Garage"))
          (is (msg-utils/replied-with? "Kitchen"))
          ;; Verify IDs are displayed
          (is (msg-utils/replied-with? "#1:") "Search results should include item IDs")
          (is (msg-utils/replied-with? "#2:") "Search results should include item IDs"))))))

(deftest test-search-no-results
  (testing "Search with no results"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        (db/add-item freezer-id "peas" 2 "bags" nil)

        (let [msg (msg-utils/mock-message {:text "search pizza" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "🔍"))
          (is (msg-utils/replied-with? "No items found")))))))

;; ============================================================================
;; Help Command Tests
;; ============================================================================

(deftest test-help-command
  (testing "Help command shows usage"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "freezer" nil)]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "🧊 Freezer Management Help"))
        (is (msg-utils/replied-with? "add 2 bags of peas"))
        (is (msg-utils/replied-with? "inventory"))
        (is (msg-utils/replied-with? "list freezers"))))))

;; ============================================================================
;; Complex Integration Scenarios
;; ============================================================================

(deftest test-full-workflow
  (testing "Complete workflow: create freezer, add items, check inventory"
    (msg-utils/with-clean-replies
      ;; 1. Create freezer
      (let [msg1 (msg-utils/mock-message {:text "add freezer garage" :sender "alice"})]
        (sut/-processChannelMessage nil msg1)
        (is (msg-utils/replied-with? "created")))

      (msg-utils/clear-replies!)

      ;; 2. Set as default
      (let [msg2 (msg-utils/mock-message {:text "set default freezer garage" :sender "alice"})]
        (sut/-processChannelMessage nil msg2)
        (is (msg-utils/replied-with? "default freezer")))

      (msg-utils/clear-replies!)

      ;; 3. Add items
      (let [msg3 (msg-utils/mock-message {:text "add 2 bags of peas expires Aug 2026" :sender "alice"})]
        (sut/-processChannelMessage nil msg3)
        (is (msg-utils/replied-with? "Added 2")))

      (msg-utils/clear-replies!)

      (let [msg4 (msg-utils/mock-message {:text "add 4 chicken breasts" :sender "alice"})]
        (sut/-processChannelMessage nil msg4)
        (is (msg-utils/replied-with? "Added 4")))

      (msg-utils/clear-replies!)

      ;; 4. Check inventory
      (let [msg5 (msg-utils/mock-message {:text "inventory" :sender "alice"})]
        (sut/-processChannelMessage nil msg5)
        (is (msg-utils/replied-with? "Garage Freezer"))
        (is (msg-utils/replied-with? "2x bags of peas"))
        (is (msg-utils/replied-with? "4x chicken breasts"))
        (is (msg-utils/replied-with? "Aug 2026")))

      (msg-utils/clear-replies!)

      ;; 5. Remove some
      (let [msg6 (msg-utils/mock-message {:text "take 1 of #1" :sender "alice"})]
        (sut/-processChannelMessage nil msg6)
        (is (msg-utils/replied-with? "1 remaining")))

      (msg-utils/clear-replies!)

      ;; 6. Verify final state
      (let [garage (db/get-freezer-by-name "garage")
            items (db/get-items (:freezer_id garage))]
        (is (= 2 (count items)))
        (is (= 1.0 (:quantity (first items))))
        (is (= 4.0 (:quantity (second items))))))))

(deftest test-multi-user-scenario
  (testing "Multiple users with different default freezers"
    (msg-utils/with-clean-replies
      (let [garage-id (db/add-freezer "garage")
            kitchen-id (db/add-freezer "kitchen")]

        ;; Alice uses garage
        (db/set-default-freezer "alice" garage-id)
        (let [msg1 (msg-utils/mock-message {:text "add peas" :sender "alice"})]
          (sut/-processChannelMessage nil msg1)
          (is (msg-utils/replied-with? "Garage Freezer")))

        (msg-utils/clear-replies!)

        ;; Bob uses kitchen
        (db/set-default-freezer "bob" kitchen-id)
        (let [msg2 (msg-utils/mock-message {:text "add chicken" :sender "bob"})]
          (sut/-processChannelMessage nil msg2)
          (is (msg-utils/replied-with? "Kitchen Freezer")))

        ;; Both see all items in inventory
        (msg-utils/clear-replies!)
        (let [msg3 (msg-utils/mock-message {:text "inventory" :sender "alice"})]
          (sut/-processChannelMessage nil msg3)
          (is (msg-utils/replied-with? "Garage Freezer"))
          (is (msg-utils/replied-with? "Kitchen Freezer"))
          (is (msg-utils/replied-with? "peas"))
          (is (msg-utils/replied-with? "chicken")))))))

;; ============================================================================
;; CSV Export Tests
;; ============================================================================

(deftest test-csv-export-empty-database
  (testing "Exporting CSV from empty database returns header only"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "export csv" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        ;; Verify document was sent
        (is (msg-utils/replied-with-document?) "Should send document")
        (is (= 1 (msg-utils/document-reply-count)) "Should send exactly one document")

        ;; Verify success message
        (is (msg-utils/replied-with? "✅") "Should show success")
        (is (msg-utils/replied-with? "Exported") "Should mention export")

        ;; Verify CSV content
        (let [csv-bytes (msg-utils/get-document-content)
              csv-string (String. csv-bytes "UTF-8")
              lines (clojure.string/split-lines csv-string)]
          (is (= 1 (count lines)) "Should have only header row")
          (is (= "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date" (first lines))
              "Header should be correct"))

        ;; Verify filename
        (let [filename (msg-utils/get-document-filename)]
          (is (clojure.string/starts-with? filename "freezer-inventory-"))
          (is (clojure.string/ends-with? filename ".csv")))))))

(deftest test-csv-export-with-items
  (testing "Exporting CSV with items includes all active items"
    (msg-utils/with-clean-replies
      ;; Setup: Create freezers and add items
      (let [garage-id (db/add-freezer "garage")
            kitchen-id (db/add-freezer "kitchen")]

        ;; Add items to garage
        (db/add-item garage-id "Peas" 2.0 "bags" nil nil)
        (db/add-item garage-id "Ice Cream" 1.0 nil "Chocolate" nil)

        ;; Add items to kitchen
        (db/add-item kitchen-id "Chicken Breasts" 4.0 nil nil nil)

        ;; Export CSV
        (let [msg (msg-utils/mock-message {:text "export" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          ;; Verify document was sent
          (is (msg-utils/replied-with-document?) "Should send document")

          ;; Verify success message mentions correct count
          (is (msg-utils/replied-with? "3 items") "Should export 3 items")

          ;; Parse and verify CSV content
          (let [csv-bytes (msg-utils/get-document-content)
                csv-string (String. csv-bytes "UTF-8")
                lines (clojure.string/split-lines csv-string)]

            ;; Should have header + 3 data rows
            (is (= 4 (count lines)) "Should have header + 3 items")

            ;; Verify header
            (is (= "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date" (first lines)))

            ;; Verify all items are present (order: garage, kitchen alphabetically)
            (let [data-lines (rest lines)
                  csv-content (clojure.string/join "\n" data-lines)]

              ;; Check for garage items
              (is (clojure.string/includes? csv-content "Garage") "Should include Garage freezer")
              (is (clojure.string/includes? csv-content "Peas") "Should include Peas")
              (is (clojure.string/includes? csv-content "bags") "Should include bags unit")
              (is (clojure.string/includes? csv-content "Ice Cream") "Should include Ice Cream")

              ;; Check for kitchen items
              (is (clojure.string/includes? csv-content "Kitchen") "Should include Kitchen freezer")
              (is (clojure.string/includes? csv-content "Chicken Breasts") "Should include Chicken Breasts")

              ;; Verify quantities are formatted correctly
              (is (clojure.string/includes? csv-content "2,bags") "Peas should show quantity 2")
              (is (clojure.string/includes? csv-content "1,") "Ice cream should show quantity 1")
              (is (clojure.string/includes? csv-content "4,") "Chicken should show quantity 4"))))))))

(deftest test-csv-export-escaping
  (testing "CSV export properly escapes special characters"
    (msg-utils/with-clean-replies
      ;; Add item with special characters that need escaping
      (let [freezer-id (db/add-freezer "test")]
        ;; Item with comma in name
        (db/add-item freezer-id "Beans, Baked" 1.0 nil nil nil)
        ;; Item with quotes in name (if this ever happens)
        (db/add-item freezer-id "Mom's \"Special\" Sauce" 1.0 nil nil nil)

        (let [msg (msg-utils/mock-message {:text "export csv" :sender "alice"})]
          (sut/-processChannelMessage nil msg)

          (let [csv-bytes (msg-utils/get-document-content)
                csv-string (String. csv-bytes "UTF-8")
                lines (clojure.string/split-lines csv-string)]

            ;; Should have header + 2 items
            (is (= 3 (count lines)))

            ;; Check that items with commas are quoted
            (let [beans-line (first (filter #(clojure.string/includes? % "Beans") lines))]
              (is (some? beans-line) "Should find beans line")
              (is (clojure.string/includes? beans-line "\"Beans, Baked\"")
                  "Item with comma should be quoted"))

            ;; Check that items with quotes have doubled quotes
            (let [sauce-line (first (filter #(clojure.string/includes? % "Special") lines))]
              (is (some? sauce-line) "Should find sauce line")
              (is (clojure.string/includes? sauce-line "\"\"Special\"\"")
                  "Quotes should be escaped by doubling"))))))))

(deftest test-csv-export-excludes-removed-items
  (testing "CSV export only includes active items, not removed ones"
    (msg-utils/with-clean-replies
      (let [freezer-id (db/add-freezer "garage")]
        ;; Add items
        (let [peas-id (db/add-item freezer-id "Peas" 2.0 "bags" nil nil)
              chicken-id (db/add-item freezer-id "Chicken" 1.0 nil nil nil)]

          ;; Remove peas completely
          (db/remove-item peas-id 2.0)

          ;; Export CSV
          (let [msg (msg-utils/mock-message {:text "download csv" :sender "alice"})]
            (sut/-processChannelMessage nil msg)

            (let [csv-bytes (msg-utils/get-document-content)
                  csv-string (String. csv-bytes "UTF-8")]

              ;; Should only include chicken, not peas
              (is (clojure.string/includes? csv-string "Chicken") "Should include active chicken")
              (is (not (clojure.string/includes? csv-string "Peas")) "Should NOT include removed peas")

              ;; Verify count
              (is (msg-utils/replied-with? "1 item") "Should export only 1 active item"))))))))

;; ============================================================================
;; CSV Import Tests
;; ============================================================================

(deftest test-csv-import-command-without-file
  (testing "Import command without file shows instructions"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "import csv" :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "📤") "Should show import icon")
        (is (msg-utils/replied-with? "CSV Import Instructions") "Should show instructions")
        (is (msg-utils/replied-with? "ID,Freezer,Item,Quantity") "Should show CSV format")))))

(deftest test-csv-import-reject-non-csv-file
  (testing "Import rejects non-CSV files"
    (msg-utils/with-clean-replies
      (let [text-bytes (.getBytes "some text content" "UTF-8")
            msg (msg-utils/mock-message {:document-bytes text-bytes
                                         :document-filename "data.txt"
                                         :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❌") "Should show error")
        (is (msg-utils/replied-with? "CSV file") "Should mention CSV file requirement")))))

(deftest test-csv-import-reject-unknown-freezer
  (testing "Import rejects CSV with unknown freezer names"
    (msg-utils/with-clean-replies
      ;; Create only garage freezer
      (db/add-freezer "garage")

      ;; Try to import CSV with unknown freezer
      (let [csv-content "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date\n,UnknownFreezer,Peas,2,bags,,\n"
            csv-bytes (.getBytes csv-content "UTF-8")
            msg (msg-utils/mock-message {:document-bytes csv-bytes
                                         :document-filename "import.csv"
                                         :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❌") "Should show error")
        (is (msg-utils/replied-with? "Unknown freezers found in CSV") "Should mention unknown freezers")
        (is (msg-utils/replied-with? "UnknownFreezer") "Should show the unknown freezer name")))))

(deftest test-csv-import-full-roundtrip
  (testing "Complete CSV export-modify-import roundtrip"
    (msg-utils/with-clean-replies
      ;; 1. Setup: Create freezers and add initial items
      (let [garage-id (db/add-freezer "garage")
            kitchen-id (db/add-freezer "kitchen")]

        ;; Add 3 initial items to garage
        (let [peas-id (db/add-item garage-id "Peas" 2.0 "bags" nil nil)
              chicken-id (db/add-item garage-id "Chicken Breasts" 4.0 nil nil nil)
              ice-cream-id (db/add-item garage-id "Ice Cream" 1.5 "tubs" nil nil)]

          ;; Add 1 item to kitchen
          (db/add-item kitchen-id "Pizza" 3.0 nil nil nil)

          (msg-utils/clear-replies!)

          ;; 2. Export CSV
          (let [export-msg (msg-utils/mock-message {:text "export csv" :sender "alice"})]
            (sut/-processChannelMessage nil export-msg)

            (is (msg-utils/replied-with-document?) "Should export document")

            ;; Get the exported CSV
            (let [original-csv-bytes (msg-utils/get-document-content)
                  original-csv-string (String. original-csv-bytes "UTF-8")
                  lines (clojure.string/split-lines original-csv-string)]

              ;; Verify export worked
              (is (= 5 (count lines)) "Should have header + 4 items")

              (msg-utils/clear-replies!)

              ;; 3. Modify the CSV in key ways:
              ;;    - Add new item (no ID)
              ;;    - Zero out ice cream (quantity = 0)
              ;;    - Modify chicken (change quantity from 4 to 6 and name to "Chicken")
              (let [header (first lines)
                    data-lines (rest lines)

                    ;; Parse lines to modify them
                    ;; Line format: ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date
                    peas-line (first (filter #(clojure.string/includes? % "Peas") data-lines))
                    chicken-line (first (filter #(clojure.string/includes? % "Chicken") data-lines))
                    ice-cream-line (first (filter #(clojure.string/includes? % "Ice Cream") data-lines))
                    pizza-line (first (filter #(clojure.string/includes? % "Pizza") data-lines))

                    ;; Modify chicken: change quantity 4 -> 6 and name "Chicken Breasts" -> "Chicken"
                    modified-chicken-line (-> chicken-line
                                             (clojure.string/replace "Chicken Breasts" "Chicken")
                                             (clojure.string/replace #",4," ",6,"))

                    ;; Zero out ice cream for removal
                    modified-ice-cream-line (clojure.string/replace ice-cream-line #",1\.5," ",0,")

                    ;; Add new item (no ID)
                    new-item-line ",garage,Frozen Vegetables,5,bags,,"

                    ;; Build modified CSV
                    modified-csv (clojure.string/join "\n"
                                   [header
                                    peas-line          ; Keep peas unchanged
                                    modified-chicken-line   ; Modified
                                    modified-ice-cream-line ; Zero'd out
                                    pizza-line         ; Keep pizza unchanged
                                    new-item-line])    ; New item

                    modified-csv-bytes (.getBytes modified-csv "UTF-8")]

                ;; 4. Import the modified CSV
                (let [import-msg (msg-utils/mock-message {:document-bytes modified-csv-bytes
                                                          :document-filename "freezer-import.csv"
                                                          :sender "alice"})]
                  (sut/-processChannelMessage nil import-msg)

                  ;; Verify success message
                  (is (msg-utils/replied-with? "✅") "Should show success")
                  (is (msg-utils/replied-with? "Import Complete") "Should show import complete")
                  (is (msg-utils/replied-with? "1 items added") "Should report 1 item added (new vegetables)")
                  (is (msg-utils/replied-with? "3 items updated") "Should report 3 items updated (peas, chicken, pizza - all have IDs)")
                  (is (msg-utils/replied-with? "1 items removed") "Should report 1 item removed (ice cream)")

                  ;; 5. Verify database state
                  (let [garage-items (db/get-items garage-id)
                        kitchen-items (db/get-items kitchen-id)]

                    ;; Garage should have 3 items now (peas, chicken, vegetables)
                    ;; Ice cream should be gone
                    (is (= 3 (count garage-items)) "Garage should have 3 items")

                    ;; Check peas unchanged
                    (let [peas (first (filter #(= "Peas" (:item_name %)) garage-items))]
                      (is (some? peas) "Peas should still exist")
                      (is (= 2.0 (:quantity peas)) "Peas quantity should be unchanged")
                      (is (= "bags" (:unit peas)) "Peas unit should be unchanged"))

                    ;; Check chicken was updated
                    (let [chicken (first (filter #(clojure.string/includes? (:item_name %) "Chicken") garage-items))]
                      (is (some? chicken) "Chicken should exist")
                      (is (= "Chicken" (:item_name chicken)) "Chicken name should be updated to 'Chicken'")
                      (is (= 6.0 (:quantity chicken)) "Chicken quantity should be updated to 6"))

                    ;; Check ice cream was removed (soft delete)
                    (let [ice-cream (db/get-item-by-id ice-cream-id)]
                      (is (some? ice-cream) "Ice cream record should still exist")
                      (is (= 0.0 (:quantity ice-cream)) "Ice cream quantity should be 0")
                      (is (some? (:removed_date ice-cream)) "Ice cream should have removed_date set"))

                    ;; Check new item was added
                    (let [vegetables (first (filter #(= "Frozen Vegetables" (:item_name %)) garage-items))]
                      (is (some? vegetables) "Frozen Vegetables should be added")
                      (is (= 5.0 (:quantity vegetables)) "Vegetables quantity should be 5")
                      (is (= "bags" (:unit vegetables)) "Vegetables unit should be 'bags'"))

                    ;; Kitchen should still have 1 item (pizza unchanged)
                    (is (= 1 (count kitchen-items)) "Kitchen should still have 1 item")
                    (let [pizza (first kitchen-items)]
                      (is (= "Pizza" (:item_name pizza)) "Pizza should still exist")
                      (is (= 3.0 (:quantity pizza)) "Pizza quantity should be unchanged"))))))))))))

(deftest test-csv-import-with-typo-suggestion
  (testing "Import with freezer typo shows helpful suggestion"
    (msg-utils/with-clean-replies
      ;; Create garage freezer
      (db/add-freezer "garage")

      ;; Try to import CSV with typo "Garag" (missing 'e')
      (let [csv-content "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date\n,Garag,Peas,2,bags,,\n"
            csv-bytes (.getBytes csv-content "UTF-8")
            msg (msg-utils/mock-message {:document-bytes csv-bytes
                                         :document-filename "import.csv"
                                         :sender "alice"})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "❌") "Should show error")
        (is (msg-utils/replied-with? "Unknown freezers") "Should mention unknown freezers")
        (is (msg-utils/replied-with? "Garag") "Should show the typo")
        (is (msg-utils/replied-with? "did you mean") "Should offer suggestion")
        (is (msg-utils/replied-with? "garage") "Should suggest 'garage'")))))
