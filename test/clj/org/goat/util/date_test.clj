(ns org.goat.util.date-test
  (:require [clojure.test :refer :all]
            [org.goat.util.date :as date])
  (:import [java.util Date Calendar TimeZone]
           [java.time LocalDate ZoneId]))

(defn- date-to-local-date
  "Convert a Date to LocalDate for easier comparison"
  [^Date date]
  (when date
    (-> date
        .toInstant
        (.atZone (ZoneId/systemDefault))
        .toLocalDate)))

(defn- make-reference-date
  "Create a reference date for testing (1st January 2025)"
  []
  (let [cal (Calendar/getInstance (TimeZone/getTimeZone "Europe/London"))]
    (.set cal 2025 0 1 12 0 0)  ; Jan 1, 2025 at noon
    (.set cal Calendar/MILLISECOND 0)
    (.getTime cal)))

(deftest test-parse-uk-date-format
  (testing "UK date format DD/MM/YYYY with 4-digit year"
    (let [ref-date (make-reference-date)
          result (date/parse-date "25/12/2025" ref-date)
          local-date (date-to-local-date result)]
      (is (not (nil? result)) "Should parse UK date 25/12/2025")
      (is (= 2025 (.getYear local-date)))
      (is (= 12 (.getMonthValue local-date)))
      (is (= 25 (.getDayOfMonth local-date)))))

  (testing "UK date format with single digit day/month"
    (let [ref-date (make-reference-date)
          result (date/parse-date "5/5/2025" ref-date)
          local-date (date-to-local-date result)]
      (is (not (nil? result)) "Should parse UK date 5/5/2025")
      (is (= 2025 (.getYear local-date)))
      (is (= 5 (.getMonthValue local-date)))
      (is (= 5 (.getDayOfMonth local-date)))))

  (testing "UK date format DD/MM/YYYY for different year"
    (let [ref-date (make-reference-date)
          result (date/parse-date "1/3/2026" ref-date)
          local-date (date-to-local-date result)]
      (is (not (nil? result)) "Should parse UK date 1/3/2026")
      (is (= 2026 (.getYear local-date)))
      (is (= 3 (.getMonthValue local-date)))
      (is (= 1 (.getDayOfMonth local-date))))))

(deftest test-parse-natural-language
  (testing "Natural language date parsing"
    (let [ref-date (make-reference-date)]
      (is (not (nil? (date/parse-date "tomorrow" ref-date)))
          "Should parse 'tomorrow'")
      (is (not (nil? (date/parse-date "next week" ref-date)))
          "Should parse 'next week'")
      (is (not (nil? (date/parse-date "in 3 days" ref-date)))
          "Should parse 'in 3 days'"))))

(deftest test-parse-date-in-context
  (testing "Date parsing within longer text"
    (let [ref-date (make-reference-date)
          result (date/parse-date "Meeting on 5/5/2025" ref-date)
          local-date (date-to-local-date result)]
      (is (not (nil? result)) "Should find date in text 'Meeting on 5/5/2025'")
      (is (= 2025 (.getYear local-date)))
      (is (= 5 (.getMonthValue local-date)))
      (is (= 5 (.getDayOfMonth local-date))))))

(deftest test-invalid-inputs
  (testing "Invalid or unparseable inputs"
    (let [ref-date (make-reference-date)]
      (is (nil? (date/parse-date nil ref-date))
          "Should return nil for nil input")
      (is (nil? (date/parse-date "" ref-date))
          "Should return nil for empty string")
      (is (nil? (date/parse-date "not a date" ref-date))
          "Should return nil for non-date text")
      (is (nil? (date/parse-date "xyzabc123" ref-date))
          "Should return nil for gibberish"))))

(deftest test-parse-date-timestamp
  (testing "Parse date returning timestamp"
    (let [ref-date (make-reference-date)
          timestamp (date/parse-date-timestamp "5/5/2025" ref-date)]
      (is (not (nil? timestamp)) "Should return timestamp")
      (is (number? timestamp) "Should be a number")
      (is (> timestamp 0) "Should be positive")))

  (testing "Invalid input returns nil timestamp"
    (let [ref-date (make-reference-date)]
      (is (nil? (date/parse-date-timestamp "not a date" ref-date))
          "Should return nil for invalid date"))))

(deftest test-future-only-option
  (testing "Future-only allows future dates"
    (let [ref-date (make-reference-date)
          future-result (date/parse-date "5/5/2025" ref-date {:future-only? true})]
      (is (not (nil? future-result)) "Should return future date with future-only? true")))

  (testing "Future-only with natural language"
    (let [ref-date (make-reference-date)
          tomorrow-result (date/parse-date "tomorrow" ref-date {:future-only? true})]
      (is (not (nil? tomorrow-result)) "Should return tomorrow with future-only? true"))))

(deftest test-parse-all-dates
  (testing "Parse multiple dates from text with natural language"
    (let [ref-date (make-reference-date)
          results (date/parse-all-dates "tomorrow and next week" ref-date)]
      (is (>= (count results) 1) "Should find at least one date")
      (is (every? #(instance? Date %) results) "All results should be Date objects")))

  (testing "Parse date from text with explicit date"
    (let [ref-date (make-reference-date)
          results (date/parse-all-dates "Meeting on 5/5/2025" ref-date)]
      (is (= 1 (count results)) "Should find exactly one date")
      (is (instance? Date (first results)) "Result should be a Date object")))

  (testing "Parse all dates with no dates"
    (let [ref-date (make-reference-date)
          results (date/parse-all-dates "no dates here" ref-date)]
      (is (empty? results) "Should return empty sequence for text with no dates"))))

(deftest test-default-reference-date
  (testing "Parse date without reference date uses now"
    (let [result (date/parse-date "tomorrow")]
      (is (not (nil? result)) "Should parse with default reference date (now)"))))

(deftest test-two-digit-year-support
  (testing "2-digit years are expanded to 4-digit years"
    (let [ref-date (make-reference-date)]
      ;; Year 26 should become 2026
      (let [result (date/parse-date "3/5/26" ref-date)
            local-date (date-to-local-date result)]
        (is (not (nil? result)) "Should parse 2-digit year 3/5/26")
        (is (= 2026 (.getYear local-date)) "Year 26 should become 2026")
        (is (= 5 (.getMonthValue local-date)))
        (is (= 3 (.getDayOfMonth local-date))))

      ;; Year 99 should become 1999
      (let [result (date/parse-date "1/1/99" ref-date)
            local-date (date-to-local-date result)]
        (is (not (nil? result)) "Should parse 2-digit year 1/1/99")
        (is (= 1999 (.getYear local-date)) "Year 99 should become 1999"))

      ;; Year 00 should become 2000
      (let [result (date/parse-date "1/1/00" ref-date)
            local-date (date-to-local-date result)]
        (is (not (nil? result)) "Should parse 2-digit year 1/1/00")
        (is (= 2000 (.getYear local-date)) "Year 00 should become 2000")))))
