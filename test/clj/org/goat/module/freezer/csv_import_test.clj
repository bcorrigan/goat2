(ns org.goat.module.freezer.csv-import-test
  (:require [clojure.test :refer :all]
            [org.goat.module.freezer.csv-import :as csv-import]))

;; ============================================================================
;; CSV Parsing Tests
;; ============================================================================

(deftest test-parse-csv-line-simple
  (testing "Parse simple CSV line"
    (is (= ["1" "Garage" "Peas" "2" "bags" "03/12/2024" "03/12/2025"]
           (csv-import/parse-csv-line "1,Garage,Peas,2,bags,03/12/2024,03/12/2025")))))

(deftest test-parse-csv-line-quoted
  (testing "Parse CSV line with quoted field containing comma"
    (is (= ["1" "Garage" "Ice Cream, Vanilla" "1.5" "tubs" "" ""]
           (csv-import/parse-csv-line "1,Garage,\"Ice Cream, Vanilla\",1.5,tubs,,")))))

(deftest test-parse-csv-line-escaped-quotes
  (testing "Parse CSV line with escaped quotes"
    (is (= ["1" "Garage" "Chicken \"special\"" "3" "" "" ""]
           (csv-import/parse-csv-line "1,Garage,\"Chicken \"\"special\"\"\",3,,,")))))

(deftest test-parse-csv-line-empty-fields
  (testing "Parse CSV line with empty fields"
    (is (= ["" "Garage" "Peas" "2" "" "" ""]
           (csv-import/parse-csv-line ",Garage,Peas,2,,,")))))

(deftest test-parse-csv-full
  (testing "Parse full CSV document"
    (let [csv-str "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date\n1,Garage,Peas,2,bags,03/12/2024,03/12/2025\n,Kitchen,Chicken,3,,,\n"
          result (csv-import/parse-csv csv-str)]
      (is (= ["ID" "Freezer" "Item" "Quantity" "Unit" "Date Added" "Expiry Date"]
             (:header result)))
      (is (= 2 (count (:rows result))))
      (is (= "1" (:id (first (:rows result)))))
      (is (= "Garage" (:freezer (first (:rows result)))))
      (is (= "" (:id (second (:rows result))))))))

;; ============================================================================
;; Validation Tests
;; ============================================================================

(deftest test-validate-header-valid
  (testing "Validate correct header"
    (let [header ["ID" "Freezer" "Item" "Quantity" "Unit" "Date Added" "Expiry Date"]
          result (csv-import/validate-header header)]
      (is (:valid result)))))

(deftest test-validate-header-invalid
  (testing "Validate incorrect header"
    (let [header ["ID" "Name" "Qty" "Location"]
          result (csv-import/validate-header header)]
      (is (not (:valid result)))
      (is (string? (:error result))))))

(deftest test-parse-quantity-valid
  (testing "Parse valid quantities"
    (is (= [2.0 nil] (csv-import/parse-quantity "2")))
    (is (= [2.5 nil] (csv-import/parse-quantity "2.5")))
    (is (= [0.0 nil] (csv-import/parse-quantity "0")))))

(deftest test-parse-quantity-invalid
  (testing "Parse invalid quantities"
    (let [[val err] (csv-import/parse-quantity "abc")]
      (is (nil? val))
      (is (string? err)))
    (let [[val err] (csv-import/parse-quantity "-5")]
      (is (nil? val))
      (is (string? err)))
    (let [[val err] (csv-import/parse-quantity "")]
      (is (nil? val))
      (is (string? err)))))

(deftest test-parse-item-id
  (testing "Parse item IDs"
    (is (= 1 (csv-import/parse-item-id "1")))
    (is (= 123 (csv-import/parse-item-id "123")))
    (is (nil? (csv-import/parse-item-id "")))
    (is (nil? (csv-import/parse-item-id "abc")))))

(deftest test-levenshtein-distance
  (testing "Calculate edit distance for typo detection"
    (is (= 0 (csv-import/levenshtein-distance "garage" "garage")))
    (is (= 1 (csv-import/levenshtein-distance "garage" "garag")))    ; missing 'e' (1 deletion)
    (is (= 1 (csv-import/levenshtein-distance "garage" "garaje")))   ; 'g'->'j' (1 substitution)
    (is (= 2 (csv-import/levenshtein-distance "garage" "gara")))))   ; missing 'ge' (2 deletions)

;; ============================================================================
;; Classification Tests
;; ============================================================================

(deftest test-classify-row-new-item
  (testing "Classify row as new item"
    (let [data {:item-id nil :quantity 2.0 :item-name "Peas"}
          result (csv-import/classify-row data)]
      (is (= :new-item (:action result))))))

(deftest test-classify-row-remove-item
  (testing "Classify row as remove item (quantity = 0)"
    (let [data {:item-id 1 :quantity 0.0 :item-name "Peas"}
          result (csv-import/classify-row data)]
      (is (= :remove-item (:action result))))))

;; ============================================================================
;; Date Comparison Tests (Month-Level Precision)
;; ============================================================================

(deftest test-same-month-year-identical-dates
  (testing "Same date should be equal"
    (let [date1 (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)
          date2 (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)]
      (is (csv-import/same-month-year? date1 date2)))))

(deftest test-same-month-year-different-days-same-month
  (testing "Different days in same month should be equal"
    (let [date1 (* (.toEpochDay (java.time.LocalDate/of 2025 12 1)) 24 60 60 1000)
          date2 (* (.toEpochDay (java.time.LocalDate/of 2025 12 31)) 24 60 60 1000)]
      (is (csv-import/same-month-year? date1 date2)))))

(deftest test-same-month-year-different-months
  (testing "Different months should not be equal"
    (let [date1 (* (.toEpochDay (java.time.LocalDate/of 2025 11 15)) 24 60 60 1000)
          date2 (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)]
      (is (not (csv-import/same-month-year? date1 date2))))))

(deftest test-same-month-year-different-years
  (testing "Different years should not be equal"
    (let [date1 (* (.toEpochDay (java.time.LocalDate/of 2024 12 15)) 24 60 60 1000)
          date2 (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)]
      (is (not (csv-import/same-month-year? date1 date2))))))

(deftest test-same-month-year-both-nil
  (testing "Both nil dates should be equal"
    (is (csv-import/same-month-year? nil nil))))

(deftest test-same-month-year-one-nil
  (testing "One nil date should not be equal to a timestamp"
    (let [date1 (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)]
      (is (not (csv-import/same-month-year? date1 nil)))
      (is (not (csv-import/same-month-year? nil date1))))))

(deftest test-item-data-changed-expiry-same-month
  (testing "Items with expiry dates in same month should not be detected as changed"
    (let [db-item {:quantity 2.0 :unit "bags" :item_name "Peas"
                   :expiry_date (* (.toEpochDay (java.time.LocalDate/of 2025 12 1)) 24 60 60 1000)}
          csv-data {:quantity 2.0 :unit "bags" :item-name "Peas"
                    :expiry-date (* (.toEpochDay (java.time.LocalDate/of 2025 12 31)) 24 60 60 1000)}]
      (is (not (csv-import/item-data-changed? db-item csv-data))))))

(deftest test-item-data-changed-expiry-different-month
  (testing "Items with expiry dates in different months should be detected as changed"
    (let [db-item {:quantity 2.0 :unit "bags" :item_name "Peas"
                   :expiry_date (* (.toEpochDay (java.time.LocalDate/of 2025 11 15)) 24 60 60 1000)}
          csv-data {:quantity 2.0 :unit "bags" :item-name "Peas"
                    :expiry-date (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)}]
      (is (csv-import/item-data-changed? db-item csv-data)))))

(deftest test-item-data-changed-quantity
  (testing "Items with different quantities should be detected as changed"
    (let [db-item {:quantity 2.0 :unit "bags" :item_name "Peas" :expiry_date nil}
          csv-data {:quantity 3.0 :unit "bags" :item-name "Peas" :expiry-date nil}]
      (is (csv-import/item-data-changed? db-item csv-data)))))

(deftest test-item-data-changed-unit
  (testing "Items with different units should be detected as changed"
    (let [db-item {:quantity 2.0 :unit "bags" :item_name "Peas" :expiry_date nil}
          csv-data {:quantity 2.0 :unit "boxes" :item-name "Peas" :expiry-date nil}]
      (is (csv-import/item-data-changed? db-item csv-data)))))

(deftest test-item-data-changed-name
  (testing "Items with different names should be detected as changed"
    (let [db-item {:quantity 2.0 :unit "bags" :item_name "Peas" :expiry_date nil}
          csv-data {:quantity 2.0 :unit "bags" :item-name "Carrots" :expiry-date nil}]
      (is (csv-import/item-data-changed? db-item csv-data)))))

(deftest test-item-data-changed-no-changes
  (testing "Items with no changes should not be detected as changed"
    (let [db-item {:quantity 2.0 :unit "bags" :item_name "Peas"
                   :expiry_date (* (.toEpochDay (java.time.LocalDate/of 2025 12 15)) 24 60 60 1000)}
          csv-data {:quantity 2.0 :unit "bags" :item-name "Peas"
                    :expiry-date (* (.toEpochDay (java.time.LocalDate/of 2025 12 20)) 24 60 60 1000)}]
      (is (not (csv-import/item-data-changed? db-item csv-data))))))

;; ============================================================================
;; Integration Tests
;; ============================================================================

(deftest test-parse-and-validate-csv
  (testing "Full CSV parsing and basic validation"
    (let [csv-str "ID,Freezer,Item,Quantity,Unit,Date Added,Expiry Date\n,TestFreezer,TestItem,5,,,\n"
          parsed (csv-import/parse-csv csv-str)
          header-validation (csv-import/validate-header (:header parsed))]
      (is (:valid header-validation))
      (is (= 1 (count (:rows parsed)))))))

(deftest test-empty-csv
  (testing "Handle empty CSV"
    (let [result (csv-import/parse-csv "")]
      (is (= [] (:header result)))
      (is (= [] (:rows result))))))

(run-tests)
