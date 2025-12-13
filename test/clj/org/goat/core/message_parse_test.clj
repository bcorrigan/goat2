(ns org.goat.core.message-parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.goat.core.message-parse :as sut]
            [org.goat.core.message-spec :as spec]))

(deftest parse-command-test
  (testing "Regular command parsing"
    (is (= {:command :wordle
            :command-args "5"
            :directly-addressed? false}
           (sut/parse-command "wordle 5" "goat")))

    (is (= {:command :stats
            :command-args ""
            :directly-addressed? false}
           (sut/parse-command "stats" "goat")))

    (is (= {:command :help
            :command-args "me please"
            :directly-addressed? false}
           (sut/parse-command "help me please" "goat"))))

  (testing "Direct addressing with comma"
    (is (= {:command :wordle
            :command-args "5 hard"
            :directly-addressed? true}
           (sut/parse-command "goat, wordle 5 hard" "goat"))))

  (testing "Direct addressing with space"
    (is (= {:command :wordle
            :command-args "5"
            :directly-addressed? true}
           (sut/parse-command "goat wordle 5" "goat"))))

  (testing "Direct addressing with exclamation"
    (is (= {:command :help
            :command-args ""
            :directly-addressed? true}
           (sut/parse-command "goat! help" "goat"))))

  (testing "Case insensitive bot name"
    (is (= {:command :stats
            :command-args ""
            :directly-addressed? true}
           (sut/parse-command "GOAT, stats" "goat")))

    (is (= {:command :stats
            :command-args ""
            :directly-addressed? true}
           (sut/parse-command "Goat, stats" "goat"))))

  (testing "Empty or nil text"
    (is (= {:command nil
            :command-args ""
            :directly-addressed? false}
           (sut/parse-command nil "goat")))

    (is (= {:command nil
            :command-args ""
            :directly-addressed? false}
           (sut/parse-command "" "goat")))

    (is (= {:command nil
            :command-args ""
            :directly-addressed? false}
           (sut/parse-command "   " "goat")))))

(deftest create-message-test
  (testing "Basic message creation"
    (let [msg (sut/create-message :chat-id 123
                                  :sender "alice"
                                  :private? false
                                  :text "hello world")]
      (is (= 123 (:message/chat-id msg)))
      (is (= "alice" (:message/sender msg)))
      (is (= false (:message/private? msg)))
      (is (= "hello world" (:message/text msg)))
      (is (uuid? (:message/id msg)))
      (is (pos-int? (:message/timestamp msg)))
      (is (= false (:message/authorized? msg)))))

  (testing "Message with command parsing"
    (let [msg (sut/create-message :chat-id 456
                                  :sender "bob"
                                  :private? true
                                  :text "wordle 5 hard")]
      (is (= "wordle 5 hard" (:message/text msg)))
      (is (= :wordle (:message/command msg)))
      (is (= "5 hard" (:message/command-args msg)))
      (is (= false (:message/directly-addressed? msg)))))

  (testing "Message with direct addressing"
    (let [msg (sut/create-message :chat-id 789
                                  :sender "charlie"
                                  :private? false
                                  :text "goat, help me")]
      (is (= "goat, help me" (:message/text msg)))
      (is (= :help (:message/command msg)))
      (is (= "me" (:message/command-args msg)))
      (is (= true (:message/directly-addressed? msg)))))

  (testing "Message with document attachment"
    (let [doc-bytes (.getBytes "test content")
          msg (sut/create-message :chat-id 111
                                  :sender "dave"
                                  :private? false
                                  :document-bytes doc-bytes
                                  :document-filename "test.csv")]
      (is (= doc-bytes (:message.attachment/document-bytes msg)))
      (is (= "test.csv" (:message.attachment/document-filename msg)))
      (is (contains? (:message.attachment/type msg) :document))))

  (testing "Authorized message"
    (let [msg (sut/create-message :chat-id 222
                                  :sender "owner"
                                  :private? true
                                  :text "admin command"
                                  :authorized? true)]
      (is (= true (:message/authorized? msg)))))

  (testing "Message validates with spec"
    (let [msg (sut/create-message :chat-id 333
                                  :sender "test"
                                  :private? false
                                  :text "test message")]
      (is (spec/valid-incoming? msg)
          "Created message should be valid incoming message"))))

(deftest create-reply-test
  (testing "Basic text reply"
    (let [source-msg (sut/create-message :chat-id 123
                                         :sender "alice"
                                         :private? false
                                         :text "hello")
          reply (sut/create-reply source-msg :text "hi alice!")]
      (is (= 123 (:message/chat-id reply)))
      (is (= "hi alice!" (:message/text reply)))
      (is (uuid? (:message/id reply)))
      (is (= {:chat-id 123 :is-private false :sender "alice"}
             (:reply/context reply)))))

  (testing "Image reply"
    (let [source-msg (sut/create-message :chat-id 456
                                         :sender "bob"
                                         :private? true)
          reply (sut/create-reply source-msg :image-bytes (.getBytes "fake image data"))]
      (is (= 456 (:message/chat-id reply)))
      (is (contains? (:message.attachment/type reply) :image))))

  (testing "Document reply"
    (let [source-msg (sut/create-message :chat-id 789
                                         :sender "charlie"
                                         :private? false)
          doc-bytes (.getBytes "document content")
          reply (sut/create-reply source-msg
                                  :document-bytes doc-bytes
                                  :document-filename "export.csv")]
      (is (= 789 (:message/chat-id reply)))
      (is (= doc-bytes (:message.attachment/document-bytes reply)))
      (is (= "export.csv" (:message.attachment/document-filename reply)))
      (is (contains? (:message.attachment/type reply) :document))))

  (testing "Reply validates with spec"
    (let [source-msg (sut/create-message :chat-id 111 :sender "test" :private? false)
          reply (sut/create-reply source-msg :text "reply text")]
      (is (spec/valid-outgoing? reply)
          "Created reply should be valid outgoing message"))))

(deftest utility-functions-test
  (testing "has-text?"
    (let [with-text (sut/create-message :chat-id 1 :sender "a" :private? false :text "hi")
          without-text (sut/create-message :chat-id 2 :sender "b" :private? false)]
      (is (true? (sut/has-text? with-text)))
      (is (false? (sut/has-text? without-text)))))

  (testing "has-document?"
    (let [with-doc (sut/create-message :chat-id 1 :sender "a" :private? false
                                       :document-bytes (.getBytes "test")
                                       :document-filename "test.txt")
          without-doc (sut/create-message :chat-id 2 :sender "b" :private? false)]
      (is (true? (sut/has-document? with-doc)))
      (is (false? (sut/has-document? without-doc)))))

  (testing "command-matches?"
    (let [wordle-msg (sut/create-message :chat-id 1 :sender "a" :private? false :text "wordle 5")
          stats-msg (sut/create-message :chat-id 2 :sender "b" :private? false :text "stats")
          no-cmd-msg (sut/create-message :chat-id 3 :sender "c" :private? false)]
      (is (true? (sut/command-matches? wordle-msg :wordle :stats)))
      (is (true? (sut/command-matches? stats-msg :wordle :stats)))
      (is (false? (sut/command-matches? wordle-msg :help :info)))
      (is (false? (sut/command-matches? no-cmd-msg :wordle))))))

(deftest edge-cases-test
  (testing "Multiple spaces in text"
    (let [parsed (sut/parse-command "wordle    5    hard" "goat")]
      (is (= :wordle (:command parsed)))
      (is (= "5 hard" (:command-args parsed)))))

  (testing "Only bot name with punctuation"
    (let [parsed (sut/parse-command "goat," "goat")]
      (is (= nil (:command parsed)))
      (is (= "" (:command-args parsed)))
      (is (= true (:directly-addressed? parsed)))))

  (testing "Bot name as part of another word should not match"
    (let [parsed (sut/parse-command "goats are nice" "goat")]
      ;; "goats" doesn't match "goat\\W*" pattern, so not directly addressed
      (is (= :goats (:command parsed)))
      (is (= false (:directly-addressed? parsed)))))

  (testing "Unicode and special characters in text"
    (let [msg (sut/create-message :chat-id 1 :sender "test" :private? false
                                  :text "stats 🎯 emoji")]
      (is (= :stats (:message/command msg)))
      (is (= "🎯 emoji" (:message/command-args msg))))))
