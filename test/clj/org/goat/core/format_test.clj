(ns org.goat.core.format-test
  (:require [clojure.test :refer :all]
            [org.goat.core.format :as fmt]))

;; =============================================================================
;; Formatter Multimethod Tests
;; =============================================================================

(deftest test-telegram-formatter
  (testing "Telegram formatter returns HTML tags"
    (let [f (fmt/formatter :telegram)]
      (is (= "<b>" (:bold f)))
      (is (= "</b>" (:end-bold f)))
      (is (= "<u>" (:underline f)))
      (is (= "</u>" (:end-underline f)))
      (is (= "<pre>" (:pre f)))
      (is (= "</pre>" (:end-pre f)))
      (is (= "" (:normal f)))
      (is (= "" (:reverse f))))))

(deftest test-cli-formatter
  (testing "CLI formatter returns empty strings (plain text)"
    (let [f (fmt/formatter :cli)]
      (is (= "" (:bold f)))
      (is (= "" (:end-bold f)))
      (is (= "" (:underline f)))
      (is (= "" (:end-underline f)))
      (is (= "" (:pre f)))
      (is (= "" (:end-pre f)))
      (is (= "" (:normal f)))
      (is (= "" (:reverse f))))))

(deftest test-default-formatter
  (testing "Unknown platform type uses default formatter (plain text)"
    (let [f (fmt/formatter :unknown-platform)]
      (is (= "" (:bold f)))
      (is (= "" (:end-bold f)))
      (is (= "" (:underline f)))
      (is (= "" (:end-underline f))))))

;; =============================================================================
;; Helper Function Tests
;; =============================================================================

(deftest test-bold-helper-telegram
  (testing "bold helper with Telegram formatter"
    (let [f (fmt/formatter :telegram)]
      (is (= "<b>Hello</b>" (fmt/bold f "Hello")))
      (is (= "<b>Test 123</b>" (fmt/bold f "Test 123"))))))

(deftest test-bold-helper-cli
  (testing "bold helper with CLI formatter (plain text)"
    (let [f (fmt/formatter :cli)]
      (is (= "Hello" (fmt/bold f "Hello")))
      (is (= "Test 123" (fmt/bold f "Test 123"))))))

(deftest test-underline-helper-telegram
  (testing "underline helper with Telegram formatter"
    (let [f (fmt/formatter :telegram)]
      (is (= "<u>Hello</u>" (fmt/underline f "Hello")))
      (is (= "<u>Underlined text</u>" (fmt/underline f "Underlined text"))))))

(deftest test-underline-helper-cli
  (testing "underline helper with CLI formatter (plain text)"
    (let [f (fmt/formatter :cli)]
      (is (= "Hello" (fmt/underline f "Hello"))))))

(deftest test-pre-helper-telegram
  (testing "pre helper with Telegram formatter"
    (let [f (fmt/formatter :telegram)]
      (is (= "<pre>code</pre>" (fmt/pre f "code")))
      (is (= "<pre>fn main() {}</pre>" (fmt/pre f "fn main() {}"))))))

(deftest test-pre-helper-cli
  (testing "pre helper with CLI formatter (plain text)"
    (let [f (fmt/formatter :cli)]
      (is (= "code" (fmt/pre f "code"))))))

(deftest test-bold-underline-helper-telegram
  (testing "bold-underline helper with Telegram formatter"
    (let [f (fmt/formatter :telegram)]
      (is (= "<b><u>Hello</u></b>" (fmt/bold-underline f "Hello")))
      (is (= "<b><u>Important</u></b>" (fmt/bold-underline f "Important"))))))

(deftest test-bold-underline-helper-cli
  (testing "bold-underline helper with CLI formatter (plain text)"
    (let [f (fmt/formatter :cli)]
      (is (= "Hello" (fmt/bold-underline f "Hello"))))))

(deftest test-get-formatter
  (testing "get-formatter is alias for formatter multimethod"
    (is (= (fmt/formatter :telegram) (fmt/get-formatter :telegram)))
    (is (= (fmt/formatter :cli) (fmt/get-formatter :cli)))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest test-complex-formatting-telegram
  (testing "Complex message formatting with Telegram"
    (let [f (fmt/formatter :telegram)
          username "Alice"
          dice-roll 6]
      (is (= "<b>Alice</b> rolled <b>6</b>"
             (str (fmt/bold f username) " rolled " (fmt/bold f dice-roll)))))))

(deftest test-complex-formatting-cli
  (testing "Complex message formatting with CLI (plain text)"
    (let [f (fmt/formatter :cli)
          username "Alice"
          dice-roll 6]
      (is (= "Alice rolled 6"
             (str (fmt/bold f username) " rolled " (fmt/bold f dice-roll)))))))

(deftest test-formatter-reusability
  (testing "Formatter can be extracted once and reused multiple times"
    (let [f (fmt/formatter :telegram)
          part1 (fmt/bold f "First")
          part2 (fmt/underline f "Second")
          part3 (fmt/pre f "Third")]
      (is (= "<b>First</b>" part1))
      (is (= "<u>Second</u>" part2))
      (is (= "<pre>Third</pre>" part3)))))
