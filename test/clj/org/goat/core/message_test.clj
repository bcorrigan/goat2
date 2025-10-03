(ns org.goat.core.message-test
  (:require [clojure.test :refer :all]
            [org.goat.core.message :refer :all])
  (:import [org.goat.core Message]))

(deftest test-message-wrapper
  (testing "MessageWrapper protocol implementation"
    (let [;; Create a mock message - we'll use the existing Message constructor
          mock-msg (Message. 12345 "goat: test message" false "testuser")
          wrapped (wrap-message mock-msg)]

      (testing "get-command returns keyword"
        (is (= :test (get-command wrapped))))

      (testing "get-text returns full text"
        (is (= "goat: test message" (get-text wrapped))))

      (testing "get-mod-text returns text after command"
        (is (= "message" (get-mod-text wrapped))))

      (testing "get-sender returns sender"
        (is (= "testuser" (get-sender wrapped))))

      (testing "get-chat-id returns chat ID"
        (is (= 12345 (get-chat-id wrapped))))

      (testing "private? returns boolean"
        (is (= false (private? wrapped))))

      (testing "has-text? returns boolean"
        (is (= true (has-text? wrapped)))))))

(deftest test-helper-functions
  (testing "Helper function utilities"
    (let [mock-msg (Message. 12345 "goat: test message" false "testuser")
          wrapped (wrap-message mock-msg)]

      (testing "command-matches? works with keywords"
        (is (command-matches? wrapped :test :other))
        (is (not (command-matches? wrapped :wrong :bad))))

      (testing "unwrap extracts Java message"
        (is (identical? mock-msg (unwrap wrapped)))
        (is (identical? mock-msg (unwrap mock-msg))))

      (testing "ensure-wrapped handles both types"
        (is (instance? org.goat.core.message.MessageWrapper (ensure-wrapped mock-msg)))
        (is (identical? wrapped (ensure-wrapped wrapped)))))))

(deftest test-case-command
  (testing "case-command routing"
    (let [mock-msg (Message. 12345 "goat: test message" false "testuser")
          wrapped (wrap-message mock-msg)
          result (atom nil)]

      (case-command wrapped
        :test #(reset! result "matched!")
        :other #(reset! result "wrong"))

      (is (= "matched!" @result)))))