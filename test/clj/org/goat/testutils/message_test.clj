(ns org.goat.testutils.message-test
  "Example tests demonstrating the Message mocking utility"
  (:require [org.goat.testutils.message :as msg-utils]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]))

(use-fixtures :each (fn [f] (msg-utils/clear-replies!) (f)))

(deftest test-basic-message-creation
  (testing "Basic message creation works"
    (let [msg (msg-utils/mock-message {:text "hello world" 
                                       :sender "alice" 
                                       :chat-id 999})]
      (is (= "hello world" (.getText msg)))
      (is (= "alice" (.getSender msg)))
      (is (= 999 (.getChatId msg))))))

(deftest test-command-message-parsing
  (testing "Bot command messages are parsed correctly"
    (let [msg (msg-utils/mock-command-message "wordle" "5 hard")]
      (is (= "goat wordle 5 hard" (.getText msg)))
      (is (= "wordle" (.getModCommand msg)))
      (is (= "5 hard" (.getModText msg))))))

(deftest test-reply-capture
  (testing "Reply capture works"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-message {:text "test"})]
        (.reply msg "Hello there!")
        (.reply msg "Another message")
        (is (= 2 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Hello"))
        (is (msg-utils/replied-with? "Another"))
        (is (not (msg-utils/replied-with? "Missing")))))))

(deftest test-convenience-functions  
  (testing "Convenience functions work as expected"
    (let [guess-msg (msg-utils/mock-guess-message "HOUSE" {:sender "bob"})
          private-msg (msg-utils/mock-private-message "secret" {:sender "eve"})]
      
      (is (= "HOUSE" (.getText guess-msg)))
      (is (= "bob" (.getSender guess-msg)))
      
      (is (= "secret" (.getText private-msg)))
      (is (= "eve" (.getSender private-msg)))
      (is (.isPrivate private-msg)))))

(deftest test-no-reply-capture
  (testing "Can disable reply capture"
    (msg-utils/clear-replies!)
    (let [msg (msg-utils/mock-message {:text "test" :capture-replies false})]
      (.reply msg "This should not be captured")
      (is (= 0 (msg-utils/reply-count))))))

;; Example showing how this could be used for Wordle testing
(deftest test-wordle-usage-example
  (testing "Example of how this would be used for Wordle module testing"
    (msg-utils/with-clean-replies
      ;; Simulate starting a game
      (let [start-msg (msg-utils/mock-command-message "wordle" "5")]
        (is (= "wordle" (.getModCommand start-msg)))
        (is (= "5" (.getModText start-msg))))
      
      ;; Simulate a guess
      (let [guess-msg (msg-utils/mock-guess-message "CRANE")]
        (is (= "CRANE" (.getText guess-msg))))
      
      ;; Could test reply capture from module functions here
      ;; (.reply some-msg "Invalid word!")
      ;; (is (msg-utils/replied-with? "Invalid"))
      )))