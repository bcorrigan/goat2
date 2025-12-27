(ns org.goat.module.flour-test
  (:require [clojure.test :refer :all]
            [org.goat.module.flour :as sut]
            [org.goat.testutils.message :as msg-utils]))

(deftest test-water-for-grain
  (testing "Calculate water for individual grain types"
    (is (= 150.0 (#'sut/water-for-grain [:white 200])))
    (is (= 155.0 (#'sut/water-for-grain [:wholemeal 200])))
    (is (= 177.77777599999998 (#'sut/water-for-grain [:rye 200])))))

(deftest test-water-for-single-flour
  (testing "Calculate water for single flour type"
    (is (= 150.0 (#'sut/water-for {:white 200})))
    (is (= 155.0 (#'sut/water-for {:wholemeal 200})))
    (is (= 133.3332 (#'sut/water-for {:spelt 200})))))

(deftest test-water-for-multiple-flours
  (testing "Calculate water for mixed flour types"
    (is (= 369.9992 (#'sut/water-for {:spelt 200 :wspelt 200 :wrye 100})))))

(deftest test-water-for-with-hydration
  (testing "Calculate water with hydration factor from example in code"
    (let [result (#'sut/water-for {:rye 50 :wholemeal 75 :white 275 :hydration 0.95})]
      (is (< (Math/abs (- result 293.374245)) 0.01)))))

(deftest test-water-for-default-hydration
  (testing "Default hydration is 1.0"
    (is (= 150.0 (#'sut/water-for {:white 200})))))

(deftest test-flour-command-help
  (testing "Flour command with no parameters shows help"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "" {:sender "alice"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Flour Hydration Calculator"))
        (is (msg-utils/replied-with? "Available flour types"))
        (is (msg-utils/replied-with? "white"))
        (is (msg-utils/replied-with? "wholemeal"))
        (is (msg-utils/replied-with? "spelt"))
        (is (msg-utils/replied-with? "very strong white flour"))
        (is (msg-utils/replied-with? "Usage"))))))

(deftest test-flour-command-single-flour
  (testing "Calculate water for single flour type"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=200" {:sender "Barry"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Barry"))
        (is (msg-utils/replied-with? "150ml"))
        (is (msg-utils/replied-with? "200g <b>white</b>"))))))

(deftest test-flour-command-multiple-flours
  (testing "Calculate water for multiple flour types"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "spelt=200 wspelt=200 wrye=100" {:sender "Barry"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Barry"))
        (is (msg-utils/replied-with? "370ml"))
        (is (msg-utils/replied-with? "200g"))
        (is (msg-utils/replied-with? "spelt"))
        (is (msg-utils/replied-with? "wspelt"))
        (is (msg-utils/replied-with? "wrye"))))))

(deftest test-flour-command-with-hydration
  (testing "Calculate water with hydration factor"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=200 hydration=0.9" {:sender "Alice"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Alice"))
        (is (msg-utils/replied-with? "135ml"))
        (is (msg-utils/replied-with? "hydration factor: 0.9"))))))

(deftest test-flour-command-with-hydration-shorthand
  (testing "Calculate water with hydration factor using h alias"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=200 h=0.8" {:sender "Bob"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "120ml"))
        (is (msg-utils/replied-with? "hydration factor: 0.8"))))))

(deftest test-flour-command-invalid-flour-type
  (testing "Invalid flour types show error with valid options"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "pizza=200" {:sender "Charlie"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Unknown flour type"))
        (is (msg-utils/replied-with? "pizza"))
        (is (msg-utils/replied-with? "Valid flour types"))))))

(deftest test-flour-command-mixed-valid-invalid
  (testing "Mixed valid and invalid flour types show error for invalid ones"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=200 pizza=100" {:sender "Dave"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Unknown flour type"))
        (is (msg-utils/replied-with? "pizza"))
        (is (msg-utils/replied-with? "Valid flour types"))))))

(deftest test-flour-command-zero-amount
  (testing "Zero amounts are ignored"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=0" {:sender "Eve"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "didn't understand those flour types"))))))

(deftest test-flour-command-negative-amount
  (testing "Negative amounts are ignored (parse-int returns 0)"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=-100" {:sender "Frank"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "didn't understand those flour types"))))))

(deftest test-flour-command-invalid-number
  (testing "Non-numeric amounts are ignored"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "white=abc" {:sender "Grace"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "didn't understand those flour types"))))))

(deftest test-flour-command-all-flour-types
  (testing "All flour types work correctly"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "rye=100 wrye=100 spelt=100 wspelt=100 white=100 wholemeal=100" {:sender "Helen"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Helen"))
        (is (msg-utils/replied-with? "ml"))
        (is (msg-utils/replied-with? "100g"))
        (is (msg-utils/replied-with? "rye"))
        (is (msg-utils/replied-with? "wrye"))
        (is (msg-utils/replied-with? "spelt"))
        (is (msg-utils/replied-with? "wspelt"))
        (is (msg-utils/replied-with? "white"))
        (is (msg-utils/replied-with? "wholemeal"))))))

(deftest test-flour-command-case-sensitive
  (testing "Flour types are case-sensitive (lowercase expected)"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "WHITE=200" {:sender "Ivan"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Unknown flour type"))
        (is (msg-utils/replied-with? "WHITE"))
        (is (msg-utils/replied-with? "Valid flour types"))))))

(deftest test-flour-command-example-from-code
  (testing "Example from code comments works correctly"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "rye=50 wholemeal=75 white=275 hydration=0.95" {:sender "Barry"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Barry"))
        (is (msg-utils/replied-with? "293ml"))))))

(deftest test-flour-command-multiple-invalid-types
  (testing "Multiple invalid flour types are listed"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "pizza=100 granary=200 ciabatta=50" {:sender "Joe"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Unknown flour type"))
        (is (msg-utils/replied-with? "pizza"))
        (is (msg-utils/replied-with? "granary"))
        (is (msg-utils/replied-with? "ciabatta"))
        (is (msg-utils/replied-with? "Valid flour types"))))))

(deftest test-flour-command-wspelt-with-granary
  (testing "User's specific example: wspelt=200 granary=100"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "flour" "wspelt=200 granary=100" {:sender "Barry"})]
        (sut/process-message msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Unknown flour type"))
        (is (msg-utils/replied-with? "granary"))
        (is (msg-utils/replied-with? "Valid flour types"))
        (is (msg-utils/replied-with? "wspelt"))))))
