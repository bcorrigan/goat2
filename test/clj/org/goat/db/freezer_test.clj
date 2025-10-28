(ns org.goat.db.freezer-test
  (:require [clojure.test :refer :all]
            [org.goat.db.freezer :as freezer]
            [clojure.java.jdbc :as sql]
            [clojure.java.io :as io]))

;; Test database configuration
(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/freezer_test.db"})

(defn clean-test-db
  "Remove test database file"
  []
  (when (.exists (io/file "resources/freezer_test.db"))
    (io/delete-file "resources/freezer_test.db")))

(defn setup-test-db
  "Override the db var in freezer namespace with test db"
  []
  (clean-test-db)
  (alter-var-root #'freezer/db (constantly test-db))
  (freezer/create-db)
  ;; Enable foreign keys for all test operations
  (sql/execute! test-db "PRAGMA foreign_keys = ON"))

(defn teardown-test-db
  "Clean up after tests"
  []
  (clean-test-db))

(use-fixtures :each
  (fn [f]
    (setup-test-db)
    (f)
    (teardown-test-db)))

;; ============================================================================
;; Freezer Operations Tests
;; ============================================================================

(deftest test-add-freezer
  (testing "Adding a new global freezer"
    (let [freezer-id (freezer/add-freezer "garage")]
      (is (some? freezer-id))
      (is (number? freezer-id))

      (let [freezers (freezer/get-freezers)]
        (is (= 1 (count freezers)))
        (is (= "garage" (:freezer_name (first freezers))))))))

(deftest test-add-multiple-freezers
  (testing "Adding multiple global freezers"
    (freezer/add-freezer "garage")
    (freezer/add-freezer "kitchen")

    (let [freezers (freezer/get-freezers)]
      (is (= 2 (count freezers)))
      ;; Should be ordered by name
      (is (= "garage" (:freezer_name (first freezers)))))))

(deftest test-default-freezer-per-user
  (testing "Each user can have their own default freezer"
    (let [garage-id (freezer/add-freezer "garage")
          kitchen-id (freezer/add-freezer "kitchen")]

      ;; User1 sets garage as default
      (freezer/set-default-freezer "user1" garage-id)
      (is (= garage-id (:freezer_id (freezer/get-default-freezer "user1"))))

      ;; User2 sets kitchen as default
      (freezer/set-default-freezer "user2" kitchen-id)
      (is (= kitchen-id (:freezer_id (freezer/get-default-freezer "user2"))))

      ;; They should be independent
      (is (= garage-id (:freezer_id (freezer/get-default-freezer "user1"))))
      (is (= kitchen-id (:freezer_id (freezer/get-default-freezer "user2")))))))

(deftest test-default-freezer-switch
  (testing "User can switch their default freezer"
    (let [garage-id (freezer/add-freezer "garage")
          kitchen-id (freezer/add-freezer "kitchen")]

      ;; Set garage as default
      (freezer/set-default-freezer "testuser" garage-id)
      (is (= garage-id (:freezer_id (freezer/get-default-freezer "testuser"))))

      ;; Switch to kitchen
      (freezer/set-default-freezer "testuser" kitchen-id)
      (is (= kitchen-id (:freezer_id (freezer/get-default-freezer "testuser")))))))

(deftest test-get-freezer-by-name
  (testing "Finding freezer by name (case-insensitive)"
    (freezer/add-freezer "Garage")

    (is (some? (freezer/get-freezer-by-name "garage")))
    (is (some? (freezer/get-freezer-by-name "GARAGE")))
    (is (some? (freezer/get-freezer-by-name "Garage")))
    (is (nil? (freezer/get-freezer-by-name "kitchen")))))

(deftest test-get-freezer-by-id
  (testing "Finding freezer by ID"
    (let [freezer-id (freezer/add-freezer "garage")
          freezer (freezer/get-freezer-by-id freezer-id)]
      (is (some? freezer))
      (is (= "garage" (:freezer_name freezer))))))

(deftest test-rename-freezer
  (testing "Renaming a freezer"
    (let [freezer-id (freezer/add-freezer "garage")]
      (freezer/rename-freezer freezer-id "big-garage")

      (let [freezer (freezer/get-freezer-by-id freezer-id)]
        (is (= "big-garage" (:freezer_name freezer)))))))

(deftest test-delete-freezer
  (testing "Deleting a freezer"
    (let [freezer-id (freezer/add-freezer "garage")]
      (freezer/delete-freezer freezer-id)

      (is (nil? (freezer/get-freezer-by-id freezer-id)))
      (is (= 0 (count (freezer/get-freezers)))))))

(deftest test-freezers-are-global
  (testing "All users see all freezers (global)"
    (let [garage-id (freezer/add-freezer "garage")
          kitchen-id (freezer/add-freezer "kitchen")]

      ;; Both users see the same freezers
      (let [freezers (freezer/get-freezers)]
        (is (= 2 (count freezers)))

        ;; User1 can get both freezers
        (is (some? (freezer/get-freezer-by-id garage-id)))
        (is (some? (freezer/get-freezer-by-id kitchen-id)))

        ;; User2 can also get both freezers
        (is (some? (freezer/get-freezer-by-id garage-id)))
        (is (some? (freezer/get-freezer-by-id kitchen-id)))))))

;; ============================================================================
;; Item Operations Tests
;; ============================================================================

(deftest test-add-item-basic
  (testing "Adding a basic item"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "peas" 2 "bags" nil)]
      (is (some? item-id))
      (is (number? item-id))

      (let [item (freezer/get-item-by-id item-id)]
        (is (= "peas" (:item_name item)))
        (is (= 2.0 (:quantity item)))
        (is (= "bags" (:unit item)))))))

(deftest test-add-item-defaults
  (testing "Adding item with default quantity"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "bread")]

      (let [item (freezer/get-item-by-id item-id)]
        (is (= 1.0 (:quantity item)))))))

(deftest test-add-item-with-notes
  (testing "Adding item with notes"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "ice cream" 1 "tub" "chocolate")]

      (let [item (freezer/get-item-by-id item-id)]
        (is (= "chocolate" (:notes item)))))))

(deftest test-get-items
  (testing "Getting all items from a freezer"
    (let [freezer-id (freezer/add-freezer "garage")]
      (freezer/add-item freezer-id "peas" 2 "bags" nil)
      (freezer/add-item freezer-id "chicken" 4 "breasts" nil)
      (freezer/add-item freezer-id "ice cream" 1 "tub" "chocolate")

      (let [items (freezer/get-items freezer-id)]
        (is (= 3 (count items)))
        (is (= ["peas" "chicken" "ice cream"]
               (map :item_name items)))))))

(deftest test-remove-item-partial
  (testing "Removing partial quantity of an item"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "peas" 5 "bags" nil)
          remaining (freezer/remove-item item-id 2)]

      (is (= 3.0 remaining))

      (let [item (freezer/get-item-by-id item-id)]
        (is (= 3.0 (:quantity item)))))))

(deftest test-remove-item-complete
  (testing "Removing all of an item deletes it"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "peas" 5 "bags" nil)
          remaining (freezer/remove-item item-id 5)]

      (is (= 0 remaining))
      (is (nil? (freezer/get-item-by-id item-id))))))

(deftest test-remove-item-over-quantity
  (testing "Removing more than available deletes the item"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "peas" 3 "bags" nil)
          remaining (freezer/remove-item item-id 10)]

      (is (= 0 remaining))
      (is (nil? (freezer/get-item-by-id item-id))))))

(deftest test-delete-freezer-cascades-items
  (testing "Deleting a freezer removes all its items"
    (let [freezer-id (freezer/add-freezer "garage")
          item1-id (freezer/add-item freezer-id "peas" 2 "bags" nil)
          item2-id (freezer/add-item freezer-id "chicken" 4 "breasts" nil)]

      (freezer/delete-freezer freezer-id)

      ;; Items should be gone
      (is (nil? (freezer/get-item-by-id item1-id)))
      (is (nil? (freezer/get-item-by-id item2-id))))))

(deftest test-search-items
  (testing "Searching for items across all freezers (global)"
    (let [garage-id (freezer/add-freezer "garage")
          kitchen-id (freezer/add-freezer "kitchen")]

      (freezer/add-item garage-id "chicken breasts" 4 "pieces" nil)
      (freezer/add-item kitchen-id "chicken nuggets" 20 "pieces" nil)
      (freezer/add-item garage-id "peas" 2 "bags" nil)

      (let [results (freezer/search-items "chicken")]
        (is (= 2 (count results)))
        (is (every? #(re-find #"(?i)chicken" (:item_name %)) results))))))

(deftest test-search-items-case-insensitive
  (testing "Search is case-insensitive"
    (let [freezer-id (freezer/add-freezer "garage")]
      (freezer/add-item freezer-id "Chicken Breasts" 4 "pieces" nil)

      (let [results (freezer/search-items "chicken")]
        (is (= 1 (count results))))

      (let [results (freezer/search-items "CHICKEN")]
        (is (= 1 (count results)))))))

(deftest test-get-item-age
  (testing "Getting item age in days"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "peas" 2 "bags" nil)
          age (freezer/get-item-age item-id)]

      ;; Should be 0 days old (just added)
      (is (= 0 age)))))

(deftest test-find-matching-item
  (testing "Finding matching items by name and unit"
    (let [freezer-id (freezer/add-freezer "garage")
          _ (freezer/add-item freezer-id "peas" 2 "bags" nil)
          _ (freezer/add-item freezer-id "peas" 3 "boxes" nil)]

      ;; Should find bags of peas
      (let [match (freezer/find-matching-item freezer-id "peas" "bags")]
        (is (some? match))
        (is (= "bags" (:unit match)))
        (is (= 2.0 (:quantity match))))

      ;; Should find boxes of peas
      (let [match (freezer/find-matching-item freezer-id "peas" "boxes")]
        (is (some? match))
        (is (= "boxes" (:unit match)))
        (is (= 3.0 (:quantity match))))

      ;; Should not find tubs of peas
      (is (nil? (freezer/find-matching-item freezer-id "peas" "tubs"))))))

(deftest test-update-item-quantity
  (testing "Updating item quantity"
    (let [freezer-id (freezer/add-freezer "garage")
          item-id (freezer/add-item freezer-id "peas" 2 "bags" nil)
          new-qty (freezer/update-item-quantity item-id 3)]

      (is (= 5.0 new-qty))

      (let [item (freezer/get-item-by-id item-id)]
        (is (= 5.0 (:quantity item)))))))

;; ============================================================================
;; Default Freezer Tests
;; ============================================================================

(deftest test-set-and-get-default-freezer
  (testing "Setting and getting user's default freezer"
    (let [freezer-id (freezer/add-freezer "garage")]
      (freezer/set-default-freezer "testuser" freezer-id)

      (let [default (freezer/get-default-freezer "testuser")]
        (is (some? default))
        (is (= freezer-id (:freezer_id default)))))))

(deftest test-default-freezer-user-isolation
  (testing "Users have separate default freezers"
    (let [garage-id (freezer/add-freezer "garage")
          kitchen-id (freezer/add-freezer "kitchen")]

      (freezer/set-default-freezer "user1" garage-id)
      (freezer/set-default-freezer "user2" kitchen-id)

      (is (= garage-id (:freezer_id (freezer/get-default-freezer "user1"))))
      (is (= kitchen-id (:freezer_id (freezer/get-default-freezer "user2")))))))

(deftest test-default-freezer-null-on-delete
  (testing "Default freezer is set to null when freezer is deleted"
    (let [freezer-id (freezer/add-freezer "garage")]
      (freezer/set-default-freezer "testuser" freezer-id)

      (freezer/delete-freezer freezer-id)

      ;; Default should now return nil (due to ON DELETE SET NULL)
      (is (nil? (freezer/get-default-freezer "testuser"))))))

;; ============================================================================
;; Integration Tests
;; ============================================================================

(deftest test-full-workflow
  (testing "Complete workflow: create freezers, add items, remove items"
    ;; Create global freezers
    (let [garage-id (freezer/add-freezer "garage")
          kitchen-id (freezer/add-freezer "kitchen")]

      ;; Set default freezers for different users
      (freezer/set-default-freezer "user1" garage-id)
      (freezer/set-default-freezer "user2" kitchen-id)

      ;; Add items to garage
      (let [peas-id (freezer/add-item garage-id "peas" 5 "bags" nil)
            chicken-id (freezer/add-item garage-id "chicken" 4 "breasts" nil)]

        ;; Add items to kitchen
        (freezer/add-item kitchen-id "ice cream" 2 "tubs" "chocolate")

        ;; Check inventory
        (is (= 2 (count (freezer/get-items garage-id))))
        (is (= 1 (count (freezer/get-items kitchen-id))))

        ;; Remove some peas
        (freezer/remove-item peas-id 2)
        (is (= 3.0 (:quantity (freezer/get-item-by-id peas-id))))

        ;; Remove all chicken
        (freezer/remove-item chicken-id 4)
        (is (nil? (freezer/get-item-by-id chicken-id)))

        ;; Garage should now have 1 item
        (is (= 1 (count (freezer/get-items garage-id))))

        ;; Search for ice cream (global search)
        (let [results (freezer/search-items "ice cream")]
          (is (= 1 (count results)))
          (is (= "kitchen" (:freezer_name (first results)))))))))
