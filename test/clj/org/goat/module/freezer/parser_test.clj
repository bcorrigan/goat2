(ns org.goat.module.freezer.parser-test
  (:require [clojure.test :refer :all]
            [org.goat.module.freezer.parser :as parser]))

;; ============================================================================
;; Helper Function Tests
;; ============================================================================

(deftest test-extract-quantity
  (testing "Extracting quantity from text"
    (is (= [2.0 "bags of peas"] (parser/extract-quantity "2 bags of peas")))
    (is (= [1.0 "loaf of bread"] (parser/extract-quantity "1 loaf of bread")))
    (is (= [5.5 "kg of mince"] (parser/extract-quantity "5.5 kg of mince")))
    (is (= [1.0 "chicken"] (parser/extract-quantity "chicken")))
    (is (= [10.0 ""] (parser/extract-quantity "10")))))

(deftest test-extract-unit
  (testing "Extracting unit from text"
    (is (= ["bags" "peas"] (parser/extract-unit "bags of peas")))
    (is (= ["bag" "peas"] (parser/extract-unit "bag of peas")))
    (is (= ["tubs" "ice cream"] (parser/extract-unit "tubs of ice cream")))
    (is (= ["pieces" "chicken"] (parser/extract-unit "pieces chicken")))
    (is (= [nil "chicken breasts"] (parser/extract-unit "chicken breasts")))))

(deftest test-extract-item-id
  (testing "Extracting item ID from text"
    (is (= 1 (parser/extract-item-id "#1")))
    (is (= 23 (parser/extract-item-id "#23")))
    (is (= 5 (parser/extract-item-id "take #5")))
    (is (nil? (parser/extract-item-id "no id here")))))

(deftest test-normalize-freezer-name
  (testing "Normalizing freezer names"
    (is (= "garage" (parser/normalize-freezer-name "Garage")))
    (is (= "garage" (parser/normalize-freezer-name "  garage  ")))
    (is (= "big garage" (parser/normalize-freezer-name "Big Garage")))))

(deftest test-extract-freezer-name
  (testing "Extracting freezer name from text"
    (is (= ["garage" "2 bags of peas"] (parser/extract-freezer-name "2 bags of peas to garage")))
    (is (= ["kitchen" "4 chicken breasts"] (parser/extract-freezer-name "4 chicken breasts in kitchen")))
    (is (= ["garage" "1 loaf"] (parser/extract-freezer-name "1 loaf to the garage freezer")))
    (is (= ["upstairs" "peas"] (parser/extract-freezer-name "peas from upstairs")))
    (is (= [nil "just peas"] (parser/extract-freezer-name "just peas")))))

;; ============================================================================
;; Add Command Tests
;; ============================================================================

(deftest test-parse-add-command-full
  (testing "Parsing full add command with all parts"
    (let [result (parser/parse-add-command "add 2 bags of peas to garage")]
      (is (= 2.0 (:quantity result)))
      (is (= "bags" (:unit result)))
      (is (= "peas" (:item-name result)))
      (is (= "garage" (:freezer-name result))))))

(deftest test-parse-add-command-minimal
  (testing "Parsing minimal add command"
    (let [result (parser/parse-add-command "add strawberries")]
      (is (= 1.0 (:quantity result)))
      (is (nil? (:unit result)))
      (is (= "strawberries" (:item-name result)))
      (is (nil? (:freezer-name result))))))

(deftest test-parse-add-command-variations
  (testing "Parsing add command with different keywords"
    (is (some? (parser/parse-add-command "put 4 chicken breasts in kitchen")))
    (is (some? (parser/parse-add-command "store 1 loaf of bread")))
    (is (some? (parser/parse-add-command "freeze ice cream")))
    (is (some? (parser/parse-add-command "ADD 2 BAGS OF PEAS")))))

(deftest test-parse-add-command-no-unit
  (testing "Parsing add command without unit"
    (let [result (parser/parse-add-command "add 4 chicken breasts")]
      (is (= 4.0 (:quantity result)))
      (is (nil? (:unit result)))
      (is (= "chicken breasts" (:item-name result))))))

(deftest test-parse-add-command-with-notes
  (testing "Parsing add command with item description"
    (let [result (parser/parse-add-command "add 1 tub of ben & jerrys")]
      (is (= 1.0 (:quantity result)))
      (is (= "tub" (:unit result)))
      (is (= "ben & jerrys" (:item-name result))))))

(deftest test-parse-add-command-invalid
  (testing "Invalid add commands return nil"
    (is (nil? (parser/parse-add-command "remove 2 of #1")))
    (is (nil? (parser/parse-add-command "list freezers")))
    (is (nil? (parser/parse-add-command "add")))))

;; ============================================================================
;; Remove Command Tests
;; ============================================================================

(deftest test-parse-remove-command-by-id
  (testing "Parsing remove command with item ID"
    (let [result (parser/parse-remove-command "take 2 of #1")]
      (is (= 2.0 (:quantity result)))
      (is (= 1 (:item-id result)))
      (is (nil? (:item-name result)))
      (is (nil? (:freezer-name result))))))

(deftest test-parse-remove-command-by-name
  (testing "Parsing remove command with item name"
    (let [result (parser/parse-remove-command "remove chicken breasts from garage")]
      (is (= 1.0 (:quantity result)))
      (is (= "chicken breasts" (:item-name result)))
      (is (= "garage" (:freezer-name result)))
      (is (nil? (:item-id result))))))

(deftest test-parse-remove-command-variations
  (testing "Parsing remove command with different keywords"
    (is (some? (parser/parse-remove-command "take #2")))
    (is (some? (parser/parse-remove-command "use #1")))
    (is (some? (parser/parse-remove-command "get 1 chicken from kitchen")))
    (is (some? (parser/parse-remove-command "REMOVE 2 OF #5")))))

(deftest test-parse-remove-command-full-item
  (testing "Parsing remove command for full item (no quantity)"
    (let [result (parser/parse-remove-command "use #2")]
      (is (= 1.0 (:quantity result)))
      (is (= 2 (:item-id result))))))

(deftest test-parse-remove-command-invalid
  (testing "Invalid remove commands return nil"
    (is (nil? (parser/parse-remove-command "add 2 bags of peas")))
    (is (nil? (parser/parse-remove-command "take")))
    (is (nil? (parser/parse-remove-command "list freezers")))))

;; ============================================================================
;; Inventory Command Tests
;; ============================================================================

(deftest test-parse-inventory-command-specific
  (testing "Parsing inventory command for specific freezer"
    (let [result (parser/parse-inventory-command "inventory garage")]
      (is (= "garage" (:freezer-name result)))
      (is (nil? (:all result))))))

(deftest test-parse-inventory-command-all
  (testing "Parsing inventory command for all freezers"
    (let [result (parser/parse-inventory-command "list all")]
      (is (true? (:all result)))
      (is (nil? (:freezer-name result))))))

(deftest test-parse-inventory-command-default
  (testing "Parsing inventory command without freezer (use default)"
    (let [result (parser/parse-inventory-command "inventory")]
      (is (some? result))
      (is (nil? (:freezer-name result)))
      (is (nil? (:all result))))))

(deftest test-parse-inventory-command-variations
  (testing "Parsing inventory command with different keywords"
    (is (some? (parser/parse-inventory-command "show garage")))
    (is (some? (parser/parse-inventory-command "check kitchen")))
    (is (some? (parser/parse-inventory-command "what's in garage")))
    (is (some? (parser/parse-inventory-command "whats in the kitchen")))
    (is (some? (parser/parse-inventory-command "LIST ALL")))))

(deftest test-parse-inventory-command-with-freezer-suffix
  (testing "Parsing inventory with 'freezer' suffix"
    (let [result (parser/parse-inventory-command "inventory garage freezer")]
      (is (= "garage" (:freezer-name result))))))

(deftest test-parse-inventory-command-invalid
  (testing "Invalid inventory commands return nil"
    (is (nil? (parser/parse-inventory-command "add peas")))
    (is (nil? (parser/parse-inventory-command "remove #1")))))

;; ============================================================================
;; Freezer Management Command Tests
;; ============================================================================

(deftest test-parse-freezer-command-add
  (testing "Parsing add freezer command"
    (let [result (parser/parse-freezer-command "add freezer garage")]
      (is (= :add-freezer (:action result)))
      (is (= "garage" (:freezer-name result))))
    (let [result (parser/parse-freezer-command "create freezer kitchen")]
      (is (= :add-freezer (:action result)))
      (is (= "kitchen" (:freezer-name result))))))

(deftest test-parse-freezer-command-delete
  (testing "Parsing delete freezer command"
    (let [result (parser/parse-freezer-command "delete freezer garage")]
      (is (= :delete-freezer (:action result)))
      (is (= "garage" (:freezer-name result))))
    (let [result (parser/parse-freezer-command "remove freezer kitchen")]
      (is (= :delete-freezer (:action result)))
      (is (= "kitchen" (:freezer-name result))))))

(deftest test-parse-freezer-command-rename
  (testing "Parsing rename freezer command"
    (let [result (parser/parse-freezer-command "rename freezer garage to big-garage")]
      (is (= :rename-freezer (:action result)))
      (is (= "garage" (:old-name result)))
      (is (= "big-garage" (:new-name result))))))

(deftest test-parse-freezer-command-list
  (testing "Parsing list freezers command"
    (let [result (parser/parse-freezer-command "list freezers")]
      (is (= :list-freezers (:action result))))
    (let [result (parser/parse-freezer-command "list freezer")]
      (is (= :list-freezers (:action result))))))

(deftest test-parse-freezer-command-set-default
  (testing "Parsing set default freezer command"
    (let [result (parser/parse-freezer-command "set default freezer garage")]
      (is (= :set-default-freezer (:action result)))
      (is (= "garage" (:freezer-name result))))))

(deftest test-parse-freezer-command-case-insensitive
  (testing "Freezer commands are case insensitive"
    (is (some? (parser/parse-freezer-command "ADD FREEZER GARAGE")))
    (is (some? (parser/parse-freezer-command "Delete Freezer Kitchen")))
    (is (some? (parser/parse-freezer-command "LIST FREEZERS")))))

(deftest test-parse-freezer-command-invalid
  (testing "Invalid freezer commands return nil"
    (is (nil? (parser/parse-freezer-command "add 2 bags of peas")))
    (is (nil? (parser/parse-freezer-command "freezer garage")))
    (is (nil? (parser/parse-freezer-command "list")))))

;; ============================================================================
;; Search Command Tests
;; ============================================================================

(deftest test-parse-search-command-basic
  (testing "Parsing basic search command"
    (let [result (parser/parse-search-command "find chicken")]
      (is (= "chicken" (:search-term result))))
    (let [result (parser/parse-search-command "search peas")]
      (is (= "peas" (:search-term result))))))

(deftest test-parse-search-command-multi-word
  (testing "Parsing search command with multi-word term"
    (let [result (parser/parse-search-command "find chicken breasts")]
      (is (= "chicken breasts" (:search-term result))))))

(deftest test-parse-search-command-case-insensitive
  (testing "Search commands are case insensitive"
    (is (some? (parser/parse-search-command "FIND CHICKEN")))
    (is (some? (parser/parse-search-command "Search Peas")))))

(deftest test-parse-search-command-invalid
  (testing "Invalid search commands return nil"
    (is (nil? (parser/parse-search-command "find")))
    (is (nil? (parser/parse-search-command "search")))
    (is (nil? (parser/parse-search-command "add peas")))))

;; ============================================================================
;; Main Parser Tests
;; ============================================================================

(deftest test-parse-command-add
  (testing "Main parser recognizes add commands"
    (let [result (parser/parse-command "add 2 bags of peas to garage")]
      (is (= :add-item (:type result)))
      (is (= 2.0 (:quantity result)))
      (is (= "peas" (:item-name result))))))

(deftest test-parse-command-remove
  (testing "Main parser recognizes remove commands"
    (let [result (parser/parse-command "take 2 of #1")]
      (is (= :remove-item (:type result)))
      (is (= 2.0 (:quantity result)))
      (is (= 1 (:item-id result))))))

(deftest test-parse-command-inventory
  (testing "Main parser recognizes inventory commands"
    (let [result (parser/parse-command "inventory garage")]
      (is (= :inventory (:type result)))
      (is (= "garage" (:freezer-name result))))))

(deftest test-parse-command-freezer-mgmt
  (testing "Main parser recognizes freezer management commands"
    (let [result (parser/parse-command "add freezer garage")]
      (is (= :freezer-management (:type result)))
      (is (= :add-freezer (:action result))))))

(deftest test-parse-command-search
  (testing "Main parser recognizes search commands"
    (let [result (parser/parse-command "find chicken")]
      (is (= :search (:type result)))
      (is (= "chicken" (:search-term result))))))

(deftest test-parse-command-priority
  (testing "Main parser prioritizes freezer management over item commands"
    ;; "add freezer X" should be freezer-management, not add-item
    (let [result (parser/parse-command "add freezer garage")]
      (is (= :freezer-management (:type result)))
      (is (= :add-freezer (:action result))))))

(deftest test-parse-command-invalid
  (testing "Main parser returns nil for invalid commands"
    (is (nil? (parser/parse-command "")))
    (is (nil? (parser/parse-command "   ")))
    (is (nil? (parser/parse-command "random gibberish")))
    (is (nil? (parser/parse-command "hello world")))))

;; ============================================================================
;; Integration Tests - Real World Examples
;; ============================================================================

(deftest test-real-world-examples
  (testing "Real world command examples from spec"
    ;; From the spec document
    (let [cmd1 (parser/parse-command "add 2 bags of peas to garage freezer")]
      (is (= :add-item (:type cmd1)))
      (is (= 2.0 (:quantity cmd1)))
      (is (= "bags" (:unit cmd1)))
      (is (= "peas" (:item-name cmd1)))
      (is (= "garage" (:freezer-name cmd1))))

    (let [cmd2 (parser/parse-command "put 4 chicken breasts in kitchen")]
      (is (= :add-item (:type cmd2)))
      (is (= 4.0 (:quantity cmd2)))
      (is (= "chicken breasts" (:item-name cmd2)))
      (is (= "kitchen" (:freezer-name cmd2))))

    (let [cmd3 (parser/parse-command "freeze 1 loaf of bread")]
      (is (= :add-item (:type cmd3)))
      (is (= 1.0 (:quantity cmd3)))
      (is (= "loaf" (:unit cmd3)))
      (is (= "bread" (:item-name cmd3))))

    (let [cmd4 (parser/parse-command "take 2 of #1")]
      (is (= :remove-item (:type cmd4)))
      (is (= 2.0 (:quantity cmd4)))
      (is (= 1 (:item-id cmd4))))

    (let [cmd5 (parser/parse-command "use #2")]
      (is (= :remove-item (:type cmd5)))
      (is (= 1.0 (:quantity cmd5)))
      (is (= 2 (:item-id cmd5))))

    (let [cmd6 (parser/parse-command "inventory garage")]
      (is (= :inventory (:type cmd6)))
      (is (= "garage" (:freezer-name cmd6))))

    (let [cmd7 (parser/parse-command "list all")]
      (is (= :inventory (:type cmd7)))
      (is (true? (:all cmd7))))

    (let [cmd8 (parser/parse-command "find chicken")]
      (is (= :search (:type cmd8)))
      (is (= "chicken" (:search-term cmd8))))))

(run-tests)
