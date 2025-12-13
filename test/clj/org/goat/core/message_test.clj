(ns org.goat.core.message-test
  (:require [clojure.test :refer :all]
            [org.goat.core.message :refer :all]
            [org.goat.core.message-parse :as msg-parse]
            [org.goat.testutils.message :as msg-utils]))

(deftest test-message-protocol-with-maps
  (testing "MessageContext protocol implementation for maps"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test message"
               :sender "testuser"
               :private? false)]

      (testing "get-command returns keyword"
        (is (= :test (get-command msg))))

      (testing "get-text returns full text"
        (is (= "goat test message" (get-text msg))))

      (testing "get-mod-text returns text after command"
        (is (= "message" (get-mod-text msg))))

      (testing "get-sender returns sender"
        (is (= "testuser" (get-sender msg))))

      (testing "get-chat-id returns chat ID"
        (is (= 12345 (get-chat-id msg))))

      (testing "private? returns boolean"
        (is (= false (private? msg))))

      (testing "has-text? returns boolean"
        (is (= true (has-text? msg)))))))

(deftest test-message-reply
  (testing "Reply functionality with map-based messages"
    (msg-utils/with-clean-replies
      (let [msg (msg-parse/create-message
                 :chat-id 12345
                 :text "goat test"
                 :sender "alice"
                 :private? false)]

        (testing "reply sends message"
          (reply msg "Hello, alice!")
          (is (= 1 (msg-utils/reply-count)))
          (is (msg-utils/replied-with? "Hello, alice!")))))))

(deftest test-helper-functions
  (testing "Helper function utilities"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test message"
               :sender "testuser"
               :private? false)]

      (testing "command shorthand works"
        (is (= :test (command msg))))

      (testing "text shorthand works"
        (is (= "goat test message" (text msg))))

      (testing "sender shorthand works"
        (is (= "testuser" (sender msg))))

      (testing "chat-id shorthand works"
        (is (= 12345 (chat-id msg)))))))

(deftest test-case-command
  (testing "case-command routing with maps"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test message"
               :sender "testuser"
               :private? false)
          result (atom nil)]

      (case-command msg
        :test #(reset! result "matched!")
        :other #(reset! result "wrong"))

      (is (= "matched!" @result)))))

(deftest test-fmt-telegram
  (testing "fmt returns Telegram formatter for :telegram messages"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test"
               :sender "alice"
               :private? false
               :platform-type :telegram)
          f (fmt msg)]
      (is (map? f))
      (is (= "<b>" (:bold f)))
      (is (= "</b>" (:end-bold f)))
      (is (= "<u>" (:underline f)))
      (is (= "</u>" (:end-underline f))))))

(deftest test-fmt-cli
  (testing "fmt returns CLI formatter for :cli messages"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test"
               :sender "alice"
               :private? false
               :platform-type :cli)
          f (fmt msg)]
      (is (map? f))
      (is (= "" (:bold f)))
      (is (= "" (:end-bold f)))
      (is (= "" (:underline f)))
      (is (= "" (:end-underline f))))))

(deftest test-fmt-default-platform
  (testing "fmt defaults to :telegram when :platform/type is missing"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test"
               :sender "alice"
               :private? false)
          f (fmt msg)]
      (is (map? f))
      (is (= "<b>" (:bold f)))
      (is (= "</b>" (:end-bold f))))))

(deftest test-fmt-shorthand
  (testing "fmt shorthand function works"
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test"
               :sender "alice"
               :private? false
               :platform-type :telegram)]
      (is (= (fmt msg) (fmt msg)))
      (is (map? (fmt msg)))
      (is (= "<b>" (:bold (fmt msg)))))))

(deftest test-fmt-integration
  (testing "fmt integrates with format helpers"
    (require 'org.goat.core.format)
    (let [msg (msg-parse/create-message
               :chat-id 12345
               :text "goat test"
               :sender "alice"
               :private? false
               :platform-type :telegram)
          f (fmt msg)
          formatted ((resolve 'org.goat.core.format/bold) f "Hello")]
      (is (= "<b>Hello</b>" formatted)))))
