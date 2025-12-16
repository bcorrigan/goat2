(ns org.goat.module.More-test
  (:require [clojure.test :refer :all]
            [org.goat.module.More :as sut]
            [org.goat.testutils.message :as msg-utils]
            [org.goat.core.pager :as pager]
            [org.goat.core.message-parse :as msg-parse]
            [org.goat.core.message :as msg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-mock-message-with-pages
  "Create a mock message with paging capabilities by pre-populating the pager cache."
  [command args has-next-page?]
  (let [base-msg (msg-utils/mock-command-message command args
                                                  {:sender "alice" :chat-id 123})]
    (when has-next-page?
      ;; Create a pager with remaining text to simulate next page
      (let [chat-key (keyword (str (:message/chat-id base-msg)))
            fake-pager (org.goat.util.Pager. "This is the next page content\n\nMore text here")]
        ;; Consume first page to have a "next" page available
        (.getPage fake-pager)
        (reset! pager/pager-cache {chat-key fake-pager})))
    base-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGRATION TESTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-more-command-with-next-page
  (testing "More command sends next page when available"
    (msg-utils/with-clean-replies
      (let [msg (create-mock-message-with-pages "more" "" true)]
        (#'sut/process-message msg)
        (is (= 1 (msg-utils/reply-count))
            "Should send the next page")
        (is (msg-utils/replied-with? "next page content")
            "Should contain next page text")))))

(deftest test-moar-command-with-next-page
  (testing "Moar command (alternative spelling) works"
    (msg-utils/with-clean-replies
      (let [msg (create-mock-message-with-pages "moar" "" true)]
        (#'sut/process-message msg)
        (is (= 1 (msg-utils/reply-count))
            "Should send the next page")))))

(deftest test-more-command-without-next-page
  (testing "More command does nothing when no next page available"
    (msg-utils/with-clean-replies
      (let [msg (create-mock-message-with-pages "more" "" false)]
        (#'sut/process-message msg)
        (is (= 0 (msg-utils/reply-count))
            "Should not send any message when no next page")))))

(deftest test-more-command-with-args-ignored
  (testing "More command with additional text is ignored"
    (msg-utils/with-clean-replies
      (let [msg (create-mock-message-with-pages "more" "some extra text" true)]
        (#'sut/process-message msg)
        (is (= 0 (msg-utils/reply-count))
            "Should ignore command with args")))))

(deftest test-more-command-with-whitespace-only
  (testing "More command with only whitespace still triggers"
    (msg-utils/with-clean-replies
      (let [msg (create-mock-message-with-pages "more" "   " true)]
        (#'sut/process-message msg)
        (is (= 1 (msg-utils/reply-count))
            "Should trigger with whitespace-only args")))))

(deftest test-more-private-message
  (testing "More works in private messages"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "more" ""
                                                 {:sender "bob" :chat-id 456 :private? true})]
        (when true  ; Simulate has-next-page
          (let [chat-key (keyword (str 456))
                fake-pager (org.goat.util.Pager. "Private next page content")]
            (.getPage fake-pager)
            (reset! pager/pager-cache {chat-key fake-pager})))

        (#'sut/process-message msg)
        (is (= 1 (msg-utils/reply-count))
            "Should send next page in private message")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-handle-more-directly
  (testing "handle-more function directly"
    (msg-utils/with-clean-replies
      (let [msg (create-mock-message-with-pages "more" "" true)]
        (#'sut/handle-more msg)
        (is (= 1 (msg-utils/reply-count))
            "Should send next page")
        (is (msg-utils/replied-with? "next page")
            "Should contain next page content")))))
