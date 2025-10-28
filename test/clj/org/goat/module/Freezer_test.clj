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
          (is (msg-utils/replied-with? "Kitchen")))))))

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
