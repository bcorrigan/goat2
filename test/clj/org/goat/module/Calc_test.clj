(ns org.goat.module.Calc-test
  (:require [clojure.test :refer :all]
            [org.goat.module.Calc :as sut]
            [org.goat.testutils.message :as msg-utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVALUATION TESTS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-evaluate-with-timeout-basic-arithmetic
  (testing "Basic arithmetic operations"
    (is (= "5" (#'sut/evaluate-with-timeout "2+3")))
    (is (= "10" (#'sut/evaluate-with-timeout "2*5")))
    (is (= "4" (#'sut/evaluate-with-timeout "12/3")))
    (is (= "7" (#'sut/evaluate-with-timeout "10-3")))))

(deftest test-evaluate-with-timeout-complex-expressions
  (testing "Complex mathematical expressions"
    (is (= "14" (#'sut/evaluate-with-timeout "(2+3)*2+4")))
    (is (= "25" (#'sut/evaluate-with-timeout "5^2")))
    (is (= "120" (#'sut/evaluate-with-timeout "5!")))))

(deftest test-evaluate-with-timeout-floating-point
  (testing "Floating point calculations"
    (let [result (#'sut/evaluate-with-timeout "10/3")]
      (is (re-find #"3\.3+" result)))))

(deftest test-evaluate-with-timeout-functions
  (testing "Mathematical functions work (sin uses radians)"
    ;; sin(90 radians) is approximately 0.894
    (let [result (#'sut/evaluate-with-timeout "sin(90)")]
      (is (string? result))
      (is (re-find #"^\-?0?\.\d+" result)))
    ;; cos(0) = 1
    (is (= "1" (#'sut/evaluate-with-timeout "cos(0)")))))

(deftest test-evaluate-with-timeout-invalid-expression
  (testing "Invalid expression returns error message"
    (let [result (#'sut/evaluate-with-timeout "2++3")]
      (is (string? result))
      (is (not (empty? result))))))

(deftest test-evaluate-with-timeout-division-by-zero
  (testing "Division by zero returns a result (Infinity or error)"
    (let [result (#'sut/evaluate-with-timeout "5/0")]
      ;; Calculator might return "Infinity" or an error message
      (is (string? result))
      (is (not (empty? result))))))

(deftest test-format-reply-short
  (testing "Short replies are returned as-is"
    (is (= "42" (#'sut/format-reply "42")))
    (is (= "The answer is 42" (#'sut/format-reply "The answer is 42")))))

(deftest test-format-reply-long
  (testing "Long replies get digit count prepended"
    (let [long-result (apply str (repeat 300 "1"))
          formatted (#'sut/format-reply long-result)]
      (is (re-find #"^300 digits:" formatted))
      (is (re-find #"111+$" formatted)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGRATION TESTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-calc-command-basic
  (testing "Basic calc command"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "2+2"
                                                 {:sender "alice" :chat-id 123})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "4"))))))

(deftest test-calc-command-complex
  (testing "Complex calculation"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "(5+3)*7"
                                                 {:sender "bob" :chat-id 456})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "56"))))))

(deftest test-calc-command-factorial
  (testing "Factorial calculation"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "6!"
                                                 {:sender "charlie" :chat-id 789})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "720"))))))

(deftest test-calc-command-power
  (testing "Power calculation"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "2^10"
                                                 {:sender "dave" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "1024"))))))

(deftest test-calc-command-empty
  (testing "Empty expression prompts for input"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" ""
                                                 {:sender "eve" :chat-id 111})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Please provide an expression"))))))

(deftest test-calc-command-invalid
  (testing "Invalid expression returns error"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "2++3"
                                                 {:sender "frank" :chat-id 222})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        ;; Should reply with some error message
        (is (pos? (count (first (msg-utils/get-replies)))))))))

(deftest test-calc-command-division-by-zero
  (testing "Division by zero is handled"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "1/0"
                                                 {:sender "grace" :chat-id 333})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        ;; Should reply with some message (error or infinity)
        (is (pos? (count (first (msg-utils/get-replies)))))))))

(deftest test-calc-command-very-large-result
  (testing "Very large results get digit count prepended"
    (msg-utils/with-clean-replies
      ;; 200! will definitely produce a very large number (> 256 chars)
      (let [msg (msg-utils/mock-command-message "calc" "200!"
                                                 {:sender "heidi" :chat-id 444})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        ;; Should include "digits:" in the reply for very large results
        (is (msg-utils/replied-with? "digits:"))))))

(deftest test-calc-command-trigonometry
  (testing "Trigonometric functions (uses radians)"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "cos(0)"
                                                 {:sender "ivan" :chat-id 555})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "1"))))))

(deftest test-calc-command-square-root
  (testing "Square root calculation"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "sqrt(16)"
                                                 {:sender "judy" :chat-id 666})]
        (#'sut/-processChannelMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "4"))))))

(deftest test-calc-private-message
  (testing "Calc works in private messages"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "calc" "3*3"
                                                 {:sender "karl" :chat-id 777 :is-private true})]
        (#'sut/-processPrivateMessage nil msg)

        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "9"))))))
