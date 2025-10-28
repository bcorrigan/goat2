(ns org.goat.module.freezer.expiry-test
  (:require [clojure.test :refer :all]
            [org.goat.module.freezer.parser :as parser])
  (:import [java.time Instant LocalDate ZoneId YearMonth]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; Expiry Date Parsing Tests (Phase 5)
;; ============================================================================

(deftest test-parse-month-name
  (testing "Parsing month names to numbers"
    (is (= 1 (parser/parse-month-name "jan")))
    (is (= 1 (parser/parse-month-name "January")))
    (is (= 8 (parser/parse-month-name "aug")))
    (is (= 8 (parser/parse-month-name "August")))
    (is (= 12 (parser/parse-month-name "dec")))
    (is (= 12 (parser/parse-month-name "December")))
    (is (nil? (parser/parse-month-name "invalid")))))

(deftest test-parse-expiry-date-month-year
  (testing "Parsing expiry date from month name and year"
    (let [result (parser/parse-expiry-date "Aug 2026")]
      (is (some? result))
      (is (number? result))
      ;; Verify it's actually August 2026 (last day of month)
      (let [instant (Instant/ofEpochMilli result)
            date (.atZone instant (ZoneId/systemDefault))
            local-date (.toLocalDate date)]
        (is (= 2026 (.getYear local-date)))
        (is (= 8 (.getMonthValue local-date)))
        (is (= 31 (.getDayOfMonth local-date))))) ; August has 31 days

    ;; Test February (28 or 29 days)
    (let [result (parser/parse-expiry-date "February 2026")]
      (is (some? result))
      (let [instant (Instant/ofEpochMilli result)
            date (.atZone instant (ZoneId/systemDefault))
            local-date (.toLocalDate date)]
        (is (= 2026 (.getYear local-date)))
        (is (= 2 (.getMonthValue local-date)))
        (is (= 28 (.getDayOfMonth local-date)))))))  ; 2026 is not a leap year

(deftest test-parse-expiry-date-numeric
  (testing "Parsing expiry date from numeric format"
    (let [result (parser/parse-expiry-date "10/2027")]
      (is (some? result))
      (let [instant (Instant/ofEpochMilli result)
            date (.atZone instant (ZoneId/systemDefault))
            local-date (.toLocalDate date)]
        (is (= 2027 (.getYear local-date)))
        (is (= 10 (.getMonthValue local-date)))
        (is (= 31 (.getDayOfMonth local-date)))))  ; October has 31 days

    ;; Test with dash separator
    (let [result (parser/parse-expiry-date "3-2028")]
      (is (some? result))
      (let [instant (Instant/ofEpochMilli result)
            date (.atZone instant (ZoneId/systemDefault))
            local-date (.toLocalDate date)]
        (is (= 2028 (.getYear local-date)))
        (is (= 3 (.getMonthValue local-date)))
        (is (= 31 (.getDayOfMonth local-date)))))))

(deftest test-parse-expiry-date-invalid
  (testing "Invalid expiry date formats return nil"
    (is (nil? (parser/parse-expiry-date "invalid")))
    (is (nil? (parser/parse-expiry-date "13/2026")))  ; Invalid month
    (is (nil? (parser/parse-expiry-date "Foo 2026")))  ; Invalid month name
    (is (nil? (parser/parse-expiry-date "2026")))))    ; Missing month

(deftest test-extract-expiry
  (testing "Extracting expiry from text"
    ;; Test extraction with month name
    (let [[expiry-ts remaining] (parser/extract-expiry "2 bags of peas expires Aug 2026")]
      (is (some? expiry-ts))
      (is (= "2 bags of peas" remaining)))

    ;; Test extraction with numeric format
    (let [[expiry-ts remaining] (parser/extract-expiry "chicken expires 10/2027")]
      (is (some? expiry-ts))
      (is (= "chicken" remaining)))

    ;; Test no expiry
    (let [[expiry-ts remaining] (parser/extract-expiry "just peas")]
      (is (nil? expiry-ts))
      (is (= "just peas" remaining)))

    ;; Test invalid expiry format (should return nil and original text)
    (let [[expiry-ts remaining] (parser/extract-expiry "peas expires invalid")]
      (is (nil? expiry-ts))
      (is (= "peas expires invalid" remaining)))))

(deftest test-default-expiry-date
  (testing "Default expiry date is approximately 1 year from now"
    (let [default-expiry (parser/default-expiry-date)
          now (System/currentTimeMillis)
          one-year-ms (* 365 24 60 60 1000)
          diff (- default-expiry now)]
      (is (some? default-expiry))
      (is (> default-expiry now))  ; Should be in the future
      ;; Should be roughly 1 year away (within a month)
      (is (> diff (* 11 30 24 60 60 1000)))  ; More than 11 months
      (is (< diff (* 13 30 24 60 60 1000))))))  ; Less than 13 months

(deftest test-parse-add-command-with-expiry
  (testing "Parsing add command with expiry date"
    ;; Test with month name expiry
    (let [result (parser/parse-add-command "add 2 bags of peas expires Aug 2026")]
      (is (= 2.0 (:quantity result)))
      (is (= "bags" (:unit result)))
      (is (= "peas" (:item-name result)))
      (is (some? (:expiry-date result))))

    ;; Test with numeric expiry
    (let [result (parser/parse-add-command "add chicken expires 10/2027")]
      (is (= 1.0 (:quantity result)))
      (is (= "chicken" (:item-name result)))
      (is (some? (:expiry-date result))))

    ;; Test with both freezer and expiry
    (let [result (parser/parse-add-command "add 3 bags of peas to garage expires Aug 2026")]
      (is (= 3.0 (:quantity result)))
      (is (= "bags" (:unit result)))
      (is (= "peas" (:item-name result)))
      (is (= "garage" (:freezer-name result)))
      (is (some? (:expiry-date result))))))

(deftest test-parse-add-command-without-expiry
  (testing "Parsing add command without expiry date"
    (let [result (parser/parse-add-command "add strawberries")]
      (is (= "strawberries" (:item-name result)))
      (is (nil? (:expiry-date result))))

    (let [result (parser/parse-add-command "add 2 bags of peas to garage")]
      (is (= 2.0 (:quantity result)))
      (is (= "bags" (:unit result)))
      (is (= "peas" (:item-name result)))
      (is (= "garage" (:freezer-name result)))
      (is (nil? (:expiry-date result))))))

(run-tests)
