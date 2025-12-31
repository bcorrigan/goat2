(ns org.goat.util.table-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.goat.util.table :as sut]
            [org.goat.core.format :as fmt]
            [clojure.string :as str]))

(deftest test-calculate-column-widths
  (testing "Calculates widths from headers and data"
    (let [headers ["Name" "Score"]
          rows [["Alice" 100] ["Bob" 9999]]
          formats [str str]
          widths (sut/calculate-column-widths headers rows formats 20)]
      (is (= [5 5] widths))))  ; "Alice" = 5, "9999" = 4 < "Score" = 5

  (testing "Respects max-width"
    (let [headers ["User"]
          rows [["verylongusername"]]
          formats [str]
          widths (sut/calculate-column-widths headers rows formats 10)]
      (is (= [10] widths))))

  (testing "Handles empty rows"
    (let [headers ["Name" "Score"]
          rows []
          formats [str str]
          widths (sut/calculate-column-widths headers rows formats 20)]
      (is (= [4 5] widths)))))  ; Just header widths

(deftest test-pad-cell
  (testing "Pads cells correctly with left alignment"
    (is (= "foo  " (sut/pad-cell "foo" 5 :left)))
    (is (= "foo" (sut/pad-cell "foo" 3 :left))))

  (testing "Pads cells correctly with right alignment"
    (is (= "  foo" (sut/pad-cell "foo" 5 :right)))
    (is (= "foo" (sut/pad-cell "foo" 3 :right))))

  (testing "Truncates long content"
    (is (= "verylon..." (sut/pad-cell "verylongtext" 10 :left)))
    (is (= "verylon..." (sut/pad-cell "verylongtext" 10 :right)))))

(deftest test-build-table-lines
  (testing "Builds table lines with header and separator"
    (let [headers ["Name" "Age"]
          rows [["Alice" 30] ["Bob" 25]]
          widths [5 3]
          aligns [:left :right]
          formats [str str]
          lines (sut/build-table-lines headers rows widths aligns formats)]
      (is (= 4 (count lines)))  ; header + separator + 2 data rows
      (is (= "Name  Age" (first lines)))
      (is (= "----- ---" (second lines)))
      (is (str/includes? (nth lines 2) "Alice"))
      (is (str/includes? (nth lines 3) "Bob")))))

(deftest test-format-table-simple
  (testing "Formats simple table correctly"
    (let [f (fmt/formatter :telegram)
          headers ["Name" "Age"]
          rows [["Alice" 30] ["Bob" 25]]
          result (sut/format-table f headers rows)]
      (is (str/starts-with? result "<pre>"))
      (is (str/ends-with? result "</pre>"))
      (is (str/includes? result "Alice"))
      (is (str/includes? result "Bob"))
      (is (str/includes? result "Name"))
      (is (str/includes? result "Age")))))

(deftest test-format-table-with-alignment
  (testing "Respects column alignment"
    (let [f (fmt/formatter :telegram)
          headers ["Name" "Score"]
          rows [["Alice" 100] ["Bob" 50]]
          result (sut/format-table f headers rows
                                   {:align [:left :right]
                                    :formats [str str]})]
      (is (str/includes? result "Alice"))
      (is (str/includes? result "100"))
      (is (str/includes? result "Bob"))
      (is (str/includes? result "50")))))

(deftest test-format-table-with-formats
  (testing "Applies format functions correctly"
    (let [f (fmt/formatter :telegram)
          headers ["Name" "Score"]
          rows [["Alice" 1000] ["Bob" 500]]
          result (sut/format-table f headers rows
                                   {:align [:left :right]
                                    :formats [str #(format "%,d" %)]})]
      (is (str/includes? result "1,000"))
      (is (str/includes? result "500")))))

(deftest test-format-table-empty-rows
  (testing "Handles empty rows gracefully"
    (let [f (fmt/formatter :telegram)
          headers ["Name" "Score"]
          rows []
          result (sut/format-table f headers rows)]
      (is (= "" result)))))

(deftest test-format-table-html-escaping
  (testing "Escapes HTML special characters"
    (let [f (fmt/formatter :telegram)
          headers ["Name" "Data"]
          rows [["<script>" "test&data"]]
          result (sut/format-table f headers rows {:max-width 30})]
      (is (str/includes? result "&lt;script&gt;"))
      (is (str/includes? result "test&amp;data")))))

(deftest test-format-table-cli-platform
  (testing "Works with CLI formatter (no pre tags)"
    (let [f (fmt/formatter :cli)
          headers ["Name" "Age"]
          rows [["Alice" 30]]
          result (sut/format-table f headers rows)]
      (is (not (str/includes? result "<pre>")))
      (is (str/includes? result "Alice")))))
