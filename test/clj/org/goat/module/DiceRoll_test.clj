(ns org.goat.module.DiceRoll-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [org.goat.module.DiceRoll :as sut]
            [org.goat.testutils.message :as msg-utils]))

;; ============================================================================
;; Parsing Tests
;; ============================================================================

(deftest test-parse-term-valid-single
  (testing "Parse valid single term with count and size"
    (let [result (#'sut/parse-term "3d6")]
      (is (= {:count 3 :size 6} result)))))

(deftest test-parse-term-default-count
  (testing "Parse term with default count (d20 → 1d20)"
    (let [result (#'sut/parse-term "d20")]
      (is (= {:count 1 :size 20} result)))))

(deftest test-parse-term-with-whitespace
  (testing "Parse term with whitespace"
    (let [result (#'sut/parse-term "  10d6  ")]
      (is (= {:count 10 :size 6} result)))))

(deftest test-parse-term-invalid-formats
  (testing "Invalid formats return nil"
    (is (nil? (#'sut/parse-term "abc")))
    (is (nil? (#'sut/parse-term "d")))
    (is (nil? (#'sut/parse-term "6d")))
    (is (nil? (#'sut/parse-term "6")))
    (is (nil? (#'sut/parse-term "")))))

(deftest test-parse-dice-notation-single
  (testing "Parse single dice notation"
    (let [result (#'sut/parse-dice-notation "3d6")]
      (is (= [{:count 3 :size 6}] result)))))

(deftest test-parse-dice-notation-multiple
  (testing "Parse multiple dice notation with +"
    (let [result (#'sut/parse-dice-notation "2d6 + 3d8 + d20")]
      (is (= [{:count 2 :size 6}
              {:count 3 :size 8}
              {:count 1 :size 20}] result)))))

(deftest test-parse-dice-notation-invalid
  (testing "Invalid notation returns nil"
    (is (nil? (#'sut/parse-dice-notation "abc")))
    (is (nil? (#'sut/parse-dice-notation "2d6 + invalid")))
    (is (nil? (#'sut/parse-dice-notation "")))))

(deftest test-validate-roll-ok
  (testing "Validate roll with acceptable dice count"
    (let [terms [{:count 10 :size 6} {:count 5 :size 20}]]
      (is (= :ok (#'sut/validate-roll terms))))))

(deftest test-validate-roll-too-many
  (testing "Validate roll with too many dice"
    (let [terms [{:count 101 :size 6}]]
      (is (= :too-many-dice (#'sut/validate-roll terms))))))

(deftest test-estimate-size
  (testing "Estimate size of roll"
    (is (= 15 (#'sut/estimate-size "10d6 + 5d20")))
    (is (= 1 (#'sut/estimate-size "d20")))
    (is (= 100 (#'sut/estimate-size "100d6")))))

;; ============================================================================
;; Rolling Tests
;; ============================================================================

(deftest test-roll-die
  (testing "Roll single die returns valid structure"
    (let [die (#'sut/roll-die 6)]
      (is (= 6 (:size die)))
      (is (<= 1 (:result die) 6)))))

(deftest test-roll-dice
  (testing "Roll multiple dice returns correct count"
    (let [dice (#'sut/roll-dice 10 6)]
      (is (= 10 (count dice)))
      (is (every? #(= 6 (:size %)) dice))
      (is (every? #(<= 1 (:result %) 6) dice)))))

(deftest test-roll-dice-d20
  (testing "Roll d20 produces valid results"
    (let [dice (#'sut/roll-dice 5 20)]
      (is (= 5 (count dice)))
      (is (every? #(= 20 (:size %)) dice))
      (is (every? #(<= 1 (:result %) 20) dice)))))

(deftest test-calculate-total
  (testing "Calculate total of dice results"
    (let [dice [{:size 6 :result 3}
                {:size 6 :result 5}
                {:size 6 :result 1}]]
      (is (= 9 (#'sut/calculate-total dice))))))

(deftest test-roll-groups
  (testing "Roll groups creates correct structure"
    (let [terms [{:count 3 :size 6} {:count 2 :size 20}]
          groups (#'sut/roll-groups terms)]
      (is (= 2 (count groups)))
      ;; First group
      (let [g1 (first groups)]
        (is (= 6 (:size g1)))
        (is (= 3 (:count g1)))
        (is (= 3 (count (:dice g1))))
        (is (number? (:total g1))))
      ;; Second group
      (let [g2 (second groups)]
        (is (= 20 (:size g2)))
        (is (= 2 (:count g2)))
        (is (= 2 (count (:dice g2))))
        (is (number? (:total g2)))))))

;; ============================================================================
;; Formatting Tests
;; ============================================================================

(deftest test-format-dice-results
  (testing "Format dice results as array"
    (let [dice [{:result 3} {:result 5} {:result 1}]]
      (is (= "[3, 5, 1]" (#'sut/format-dice-results dice))))))

(deftest test-format-group
  (testing "Format single group with emoji and structure"
    (let [group {:size 6 :count 3
                 :dice [{:result 3} {:result 5} {:result 1}]
                 :total 9}
          formatted (#'sut/format-group group)]
      (is (str/includes? formatted "•"))
      (is (str/includes? formatted "3d6"))
      (is (str/includes? formatted "[3, 5, 1]"))
      (is (str/includes? formatted "9")))))

(deftest test-format-roll
  (testing "Format complete roll with emoji and all groups"
    (let [groups [{:size 6 :count 3
                   :dice [{:result 3} {:result 5} {:result 1}]
                   :total 9}
                  {:size 20 :count 2
                   :dice [{:result 15} {:result 18}]
                   :total 33}]
          formatted (#'sut/format-roll "alice" groups)]
      (is (str/includes? formatted "🎲"))
      (is (str/includes? formatted "alice"))
      (is (str/includes? formatted "rolled:"))
      (is (str/includes? formatted "3d6"))
      (is (str/includes? formatted "2d20"))
      (is (str/includes? formatted "Total: 42")))))

(deftest test-format-coin-toss
  (testing "Format coin toss with emoji"
    (let [formatted (#'sut/format-coin-toss "bob" "Heads")]
      (is (str/includes? formatted "🪙"))
      (is (str/includes? formatted "bob"))
      (is (str/includes? formatted "Heads")))))

;; ============================================================================
;; Command Integration Tests
;; ============================================================================

(deftest test-roll-command-valid
  (testing "Roll command with valid input"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "roll" "3d6 + 2d20"
                  {:sender "alice"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "🎲"))
        (is (msg-utils/replied-with? "alice"))
        (is (msg-utils/replied-with? "rolled:"))
        (is (msg-utils/replied-with? "3d6"))
        (is (msg-utils/replied-with? "2d20"))
        (is (msg-utils/replied-with? "Total:"))))))

(deftest test-roll-command-single-die
  (testing "Roll command with single die"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "roll" "d20"
                  {:sender "bob"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "bob"))
        (is (msg-utils/replied-with? "1d20"))))))

(deftest test-roll-command-invalid-format
  (testing "Roll command with invalid format shows error"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "roll" "not-valid"
                  {:sender "alice"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Sorry, I don't know how to do that."))))))

(deftest test-roll-command-too-many-dice
  (testing "Roll command with >100 dice shows error"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "roll" "101d6"
                  {:sender "alice"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "I'm not rolling that many dice"))))))

(deftest test-roll-command-exactly-100-dice
  (testing "Roll command with exactly 100 dice should work"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "roll" "100d6"
                  {:sender "alice"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "100d6"))
        (is (msg-utils/replied-with? "Total:"))))))

(deftest test-toss-command-empty
  (testing "Toss command with no argument"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "toss" ""
                  {:sender "alice"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "🪙"))
        (is (msg-utils/replied-with? "alice"))
        (is (or (msg-utils/replied-with? "Heads")
                (msg-utils/replied-with? "Tails")))))))

(deftest test-toss-command-with-coin
  (testing "Toss command with 'coin' argument"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "toss" "coin"
                  {:sender "bob"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "🪙"))
        (is (msg-utils/replied-with? "bob"))
        (is (or (msg-utils/replied-with? "Heads")
                (msg-utils/replied-with? "Tails")))))))

(deftest test-toss-command-distribution
  (testing "Toss command produces both Heads and Tails over multiple runs"
    ;; Run 100 times and verify we get both results
    (let [results (atom #{})]
      (dotimes [_ 100]
        (msg-utils/with-clean-replies
          (let [msg (msg-utils/mock-command-message "toss" ""
                      {:sender "alice"})]
            (sut/process-channel-message nil msg)
            (cond
              (msg-utils/replied-with? "Heads") (swap! results conj :heads)
              (msg-utils/replied-with? "Tails") (swap! results conj :tails)))))
      ;; With 100 flips, we should get both at least once
      (is (contains? @results :heads))
      (is (contains? @results :tails)))))
