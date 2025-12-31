(ns org.goat.module.WordStats-test
  (:require [org.goat.module.WordStats :as sut]
            [org.goat.db.user-stats :as db]
            [org.goat.testutils.message :as msg-utils]
            [org.goat.core.message :as msg]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as sql])
  (:import [java.io File]))

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-wordstats.db"})

(defn setup-test-db []
  (let [db-file (File. "test/resources/test-wordstats.db")]
    (when (.exists db-file) (.delete db-file)))

  (sql/db-do-commands test-db
    (sql/create-table-ddl :message_stats
      [[:username :text]
       [:chatid :integer]
       [:total_messages :integer "DEFAULT 0"]
       [:total_words :integer "DEFAULT 0"]
       [:total_chars :integer "DEFAULT 0"]
       [:unique_words_count :integer "DEFAULT 0"]
       [:swear_words_count :integer "DEFAULT 0"]
       [:messages_since_last_swear :integer "DEFAULT 0"]
       [:last_swear_time :datetime]
       [:first_message_time :datetime]
       [:last_updated :datetime]
       ["PRIMARY KEY (username, chatid)"]]))

  (sql/db-do-commands test-db
    (sql/create-table-ddl :user_vocabulary
      [[:username :text]
       [:chatid :integer]
       [:word :text]
       [:frequency :integer "DEFAULT 1"]
       [:first_seen :datetime]
       [:last_seen :datetime]
       ["PRIMARY KEY (username, chatid, word)"]]))

  (sql/db-do-commands test-db
    (sql/create-table-ddl :message_history
      [[:username :text]
       [:chatid :integer]
       [:word_count :integer]
       [:char_count :integer]
       [:sentence_count :integer]
       [:avg_word_length :real]
       [:avg_sentence_length :real]
       [:swear_word_count :integer]
       [:timestamp :datetime]]))

  (sql/db-do-commands test-db
    (sql/create-table-ddl :swear_history
      [[:username :text]
       [:chatid :integer]
       [:swear_word :text]
       [:message_number :integer]
       [:timestamp :datetime]])))

(defn teardown-test-db []
  (let [db-file (File. "test/resources/test-wordstats.db")]
    (when (.exists db-file) (.delete db-file))))

(defn db-fixture [f]
  (setup-test-db)
  (msg-utils/clear-replies!)
  (with-redefs [db/db test-db]
    (f))
  (teardown-test-db))

(use-fixtures :each db-fixture)

;; Message Filtering Tests
(deftest test-contains-url?
  (testing "Detects URLs in messages"
    (is (sut/contains-url? "Check out http://example.com"))
    (is (sut/contains-url? "Visit https://github.com"))
    (is (sut/contains-url? "Go to www.google.com"))
    (is (not (sut/contains-url? "No URL here")))))

(deftest test-quoted-text?
  (testing "Detects quoted text"
    (is (sut/quoted-text? "\"This is quoted\""))
    (is (sut/quoted-text? "'Also quoted'"))
    (is (sut/quoted-text? "> This is a quote reply"))
    (is (not (sut/quoted-text? "Not quoted at all")))))

(deftest test-contains-words?
  (testing "Verifies message contains actual words"
    (is (sut/contains-words? "Hello world"))
    (is (sut/contains-words? "Test123"))
    (is (not (sut/contains-words? "123 456")))
    (is (not (sut/contains-words? "🎉🎊")))))

(deftest test-too-long?
  (testing "Detects messages longer than 1000 characters"
    (is (not (sut/too-long? "Short message")))
    (is (not (sut/too-long? (apply str (repeat 1000 "x")))))
    (is (sut/too-long? (apply str (repeat 1001 "x"))))
    (is (sut/too-long? (apply str (repeat 2000 "x"))))))

(deftest test-has-formatting?
  (testing "Detects markdown and HTML formatting"
    ;; Markdown bold
    (is (sut/has-formatting? "This is **bold** text"))
    (is (sut/has-formatting? "This is __bold__ text"))
    ;; Markdown italic
    (is (sut/has-formatting? "This is *italic* text"))
    (is (sut/has-formatting? "This is _italic_ text"))
    ;; HTML tags
    (is (sut/has-formatting? "This is <b>bold</b> text"))
    (is (sut/has-formatting? "This is <i>italic</i> text"))
    (is (sut/has-formatting? "This is <strong>strong</strong> text"))
    (is (sut/has-formatting? "This is <em>emphasized</em> text"))
    (is (sut/has-formatting? "This is <code>code</code> text"))
    ;; Code blocks
    (is (sut/has-formatting? "```code block```"))
    ;; No formatting
    (is (not (sut/has-formatting? "Plain text message")))
    (is (not (sut/has-formatting? "Text with numbers 123 and symbols !@#")))))

(deftest test-should-analyse-message?
  (testing "Correctly filters messages for analysis"
    (is (sut/should-analyse-message? "This is a normal message"))
    (is (not (sut/should-analyse-message? "Check http://example.com")))
    (is (not (sut/should-analyse-message? "\"Quoted text\"")))
    (is (not (sut/should-analyse-message? "> Quote reply")))
    (is (not (sut/should-analyse-message? "Hi")))  ; too short
    (is (not (sut/should-analyse-message? "123")))  ; no words
    (is (not (sut/should-analyse-message? nil)))
    (is (not (sut/should-analyse-message? (apply str (repeat 1001 "x")))))  ; too long
    (is (not (sut/should-analyse-message? "This is **bold** text")))  ; has formatting
    (is (not (sut/should-analyse-message? "This is *italic* text")))  ; has formatting
    (is (not (sut/should-analyse-message? "This is <b>HTML</b> text")))))

(deftest test-is-swear-word?
  (testing "Detects swear words"
    ;; Mock swear-words set
    (with-redefs [sut/swear-words (delay #{"damn" "hell" "shit"})]
      (is (sut/is-swear-word? "damn"))
      (is (sut/is-swear-word? "DAMN"))
      (is (sut/is-swear-word? "Damn"))
      (is (not (sut/is-swear-word? "darn")))
      (is (not (sut/is-swear-word? "hello"))))))

(deftest test-extract-swear-words
  (testing "Extracts swear words from word list"
    (with-redefs [sut/swear-words (delay #{"damn" "hell" "shit"})]
      (is (= ["damn" "hell"] (sut/extract-swear-words ["damn" "this" "hell"])))
      (is (= [] (sut/extract-swear-words ["clean" "words" "only"])))
      (is (= ["shit"] (sut/extract-swear-words ["oh" "shit"]))))))

(deftest test-calculate-avg-word-length
  (testing "Calculates average word length"
    (is (= 5.0 (sut/calculate-avg-word-length ["hello" "world"])))
    (is (= 3.0 (sut/calculate-avg-word-length ["one" "two" "six"])))
    (is (= 0.0 (sut/calculate-avg-word-length [])))))

(deftest test-calculate-avg-sentence-length
  (testing "Calculates average sentence length"
    (is (= 5.0 (sut/calculate-avg-sentence-length "One. Two." 10)))
    (is (= 3.0 (sut/calculate-avg-sentence-length "A. B. C." 9)))
    (is (= 0.0 (sut/calculate-avg-sentence-length "No sentences" 0)))))

(deftest test-analyse-and-store-clean-message
  (testing "Analyzes and stores clean message correctly"
    (with-redefs [sut/swear-words (delay #{"damn" "hell"})]
      (let [result (sut/analyse-and-store-message "Hello world this is a test" "alice" 123)]
        (is (false? (:had-swear result)))

        ;; Check message stats were updated
        (let [stats (db/get-user-stats "alice" 123)]
          (is (= 1 (:total_messages stats)))
          (is (= 6 (:total_words stats)))
          (is (= 1 (:messages_since_last_swear stats)))
          (is (= 0 (:swear_words_count stats))))

        ;; Check vocabulary was stored
        (is (= 6 (db/get-vocabulary-size "alice" 123)))))))

(deftest test-analyse-and-store-message-with-swear
+  (testing "Analyzes and stores message with swear word"
    (with-redefs [sut/swear-words (delay #{"damn" "hell"})]
      (let [result (sut/analyse-and-store-message "Oh damn that sucks" "bob" 456)]
        (is (true? (:had-swear result)))

        ;; Check message stats
        (let [stats (db/get-user-stats "bob" 456)]
          (is (= 1 (:total_messages stats)))
          (is (= 0 (:messages_since_last_swear stats)))
          (is (= 1 (:swear_words_count stats)))
          (is (some? (:last_swear_time stats))))

        ;; Check swear was recorded
        (let [swears (db/get-last-two-swears "bob" 456)]
          (is (= 1 (count swears)))
          (is (= "damn" (:swear_word (first swears)))))))))

(deftest test-analyse-multiple-messages-building-streak
  (testing "Multiple clean messages build up streak"
    (with-redefs [sut/swear-words (delay #{"damn"})]
      ;; Send 3 clean messages
      (sut/analyse-and-store-message "Clean message one" "alice" 123)
      (sut/analyse-and-store-message "Clean message two" "alice" 123)
      (sut/analyse-and-store-message "Clean message three" "alice" 123)

      (let [stats (db/get-user-stats "alice" 123)]
        (is (= 3 (:total_messages stats)))
        (is (= 3 (:messages_since_last_swear stats)))
        (is (= 0 (:swear_words_count stats)))))))

(deftest test-analyse-streak-then-swear
  (testing "Clean streak followed by swear resets counter"
    (with-redefs [sut/swear-words (delay #{"damn"})]
      ;; Build streak
      (sut/analyse-and-store-message "Clean one" "alice" 123)
      (sut/analyse-and-store-message "Clean two" "alice" 123)
      (sut/analyse-and-store-message "Clean three" "alice" 123)

      (let [stats-before (db/get-user-stats "alice" 123)]
        (is (= 3 (:messages_since_last_swear stats-before))))

      ;; Swear - should reset
      (let [result (sut/analyse-and-store-message "Oh damn it" "alice" 123)]
        (is (= 3 (:previous-streak result)))

        (let [stats-after (db/get-user-stats "alice" 123)]
          (is (= 0 (:messages_since_last_swear stats-after)))
          (is (= 1 (:swear_words_count stats-after))))))))


(deftest test-count-sentences
  (testing "Counts sentences correctly"
    (is (= 2 (sut/count-sentences "First sentence. Second sentence.")))
    (is (= 3 (sut/count-sentences "One! Two? Three.")))
    (is (= 1 (sut/count-sentences "Just one")))))

;; Note: Purity fall scolding is tested through integration test below
;; where we process actual messages through the module

(deftest test-show-user-stats-with-data
  (testing "Displays user stats correctly"
    (msg-utils/with-clean-replies
      ;; Create some stats
      (sql/insert! test-db :message_stats
        {:username "alice" :chatid 123
         :total_messages 100
         :total_words 500
         :total_chars 2500
         :unique_words_count 200
         :swear_words_count 5
         :messages_since_last_swear 10
         :first_message_time (System/currentTimeMillis)
         :last_updated (System/currentTimeMillis)})

      (let [msg (msg-utils/mock-command-message "wordstats" nil {:sender "alice" :chat-id 123})]
        (sut/process-message msg)

        ;; Verify response contains key stats
        (is (msg-utils/replied-with? "Word Stats"))
        (is (msg-utils/replied-with? "100"))  ; total messages
        (is (msg-utils/replied-with? "500"))  ; total words
        (is (msg-utils/replied-with? "200"))  ; unique words
        (is (msg-utils/replied-with? "10 messages")))))) ; clean streak

(deftest test-show-user-stats-no-data
  (testing "Handles user with no stats gracefully"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "wordstats" nil {:sender "nobody" :chat-id 999})]
        (sut/process-message msg)

        (is (msg-utils/replied-with? "No statistics available"))))))

(deftest test-show-purity-stats-pure-user
  (testing "Shows purity stats for clean user"
    (msg-utils/with-clean-replies
      (sql/insert! test-db :message_stats
        {:username "alice" :chatid 123
         :total_messages 50
         :total_words 250
         :total_chars 1250
         :unique_words_count 100
         :swear_words_count 0
         :messages_since_last_swear 50
         :first_message_time (System/currentTimeMillis)
         :last_updated (System/currentTimeMillis)})

      (let [msg (msg-utils/mock-command-message "purity" nil {:sender "alice" :chat-id 123})]
        (sut/process-message msg)

        (is (msg-utils/replied-with? "PURE"))
        (is (msg-utils/replied-with? "50 messages"))
        (is (msg-utils/replied-with? "Keep up the good work"))))))

(deftest test-show-purity-stats-impure-user
  (testing "Shows purity stats for impure user"
    (msg-utils/with-clean-replies
      (sql/insert! test-db :message_stats
        {:username "bob" :chatid 456
         :total_messages 50
         :total_words 250
         :total_chars 1250
         :unique_words_count 100
         :swear_words_count 5
         :messages_since_last_swear 0
         :last_swear_time (System/currentTimeMillis)
         :first_message_time (System/currentTimeMillis)
         :last_updated (System/currentTimeMillis)})

      (let [msg (msg-utils/mock-command-message "purity" nil {:sender "bob" :chat-id 456})]
        (sut/process-message msg)

        (is (msg-utils/replied-with? "IMPURE"))
        (is (msg-utils/replied-with? "clean up your act"))))))

;; Full module Integration Tests with Commands
(deftest test-stats-command
  (testing "Stats command triggers stats display"
    (msg-utils/with-clean-replies
      ;; Create some data
      (sql/insert! test-db :message_stats
        {:username "alice" :chatid 123
         :total_messages 10
         :total_words 50
         :total_chars 250
         :unique_words_count 30
         :swear_words_count 1
         :messages_since_last_swear 5
         :first_message_time (System/currentTimeMillis)
         :last_updated (System/currentTimeMillis)})

      (let [msg (msg-utils/mock-command-message "wordstats" nil {:sender "alice" :chat-id 123})]
        ;; Simulate module processing
        (sut/process-message msg)

        (is (msg-utils/replied-with? "Word Stats"))))))

(deftest test-purity-command
  (testing "Purity command triggers purity display"
    (msg-utils/with-clean-replies
      (sql/insert! test-db :message_stats
        {:username "bob" :chatid 456
         :total_messages 20
         :total_words 100
         :total_chars 500
         :unique_words_count 50
         :swear_words_count 0
         :messages_since_last_swear 20
         :first_message_time (System/currentTimeMillis)
         :last_updated (System/currentTimeMillis)})

      (let [msg (msg-utils/mock-command-message "purity" nil {:sender "bob" :chat-id 456})]
        ;; Simulate module processing
        (sut/process-message msg)

        (is (msg-utils/replied-with? "Purity Report"))))))

;; Integration tests for unclaimed message processing
(deftest test-message-command-parsing
  (testing "Message parser creates command from first word"
    (let [msg (msg-utils/mock-message {:text "Hello world"
                                       :sender "alice"
                                       :chat-id 123
                                       :private? false})]
      (is (= "Hello world" (msg/get-text msg)))
      (is (= :hello (msg/command msg)) "First word becomes command keyword"))))

(deftest test-module-handles-unregistered-commands
  (testing "Module treats unregistered commands as unclaimed messages"
    ;; In production, the dispatcher only sends unclaimed messages to this module.
    ;; But if we call process-message directly with an unregistered command,
    ;; it should treat it as an unclaimed message and analyze it.
    (msg-utils/with-clean-replies
      (with-redefs [sut/swear-words (delay #{"damn"})]
        ;; Simulate a message with a command registered by another module
        ;; (in reality, dispatcher wouldn't send this here, but we're testing
        ;; that the module correctly handles anything not :wordstats/:purity)
        (let [msg (msg-utils/mock-message {:text "wordle 5 hard mode"
                                           :sender "bob"
                                           :chat-id 456
                                           :private? false})]
          (sut/process-message msg)

          ;; Should analyze it since it's not :wordstats or :purity
          (let [stats (db/get-user-stats "bob" 456)]
            (is (some? stats) "Module analyzes any non-registered command")
            (is (= 1 (:total_messages stats)))
            (is (= 4 (:total_words stats)))))))))

(deftest test-unclaimed-message-is-analyzed
  (testing "Unclaimed non-private message gets analyzed and stored"
    (msg-utils/with-clean-replies
      (with-redefs [sut/swear-words (delay #{"damn"})]
        ;; Create a regular message (not a command, not private)
        (let [msg (msg-utils/mock-message {:text "Hello world this is a test message"
                                           :sender "alice"
                                           :chat-id 123
                                           :private? false})]
          ;; Process through the module
          (sut/process-message msg)

          ;; Verify stats were stored
          (let [stats (db/get-user-stats "alice" 123)]
            (is (some? stats) "Stats should be created")
            (is (= 1 (:total_messages stats)) "Should have 1 message")
            (is (= 7 (:total_words stats)) "Should have 7 words")
            (is (= 1 (:messages_since_last_swear stats)) "Should have clean streak of 1"))

          ;; Verify vocabulary was stored
          (is (= 7 (db/get-vocabulary-size "alice" 123)) "Should have 7 unique words"))))))

(deftest test-private-message-not-analyzed
  (testing "Private messages are not analyzed"
    (msg-utils/with-clean-replies
      ;; Create a private message
      (let [msg (msg-utils/mock-message {:text "Hello world this is a test message"
                                         :sender "bob"
                                         :chat-id 456
                                         :private? true})]
        ;; Process through the module
        (sut/process-message msg)

        ;; Verify NO stats were stored
        (let [stats (db/get-user-stats "bob" 456)]
          (is (nil? stats) "Private messages should not be analyzed"))))))

(deftest test-unclaimed-message-with-swear
  (testing "Unclaimed message with swear word is tracked"
    (msg-utils/with-clean-replies
      (with-redefs [sut/swear-words (delay #{"damn"})]
        ;; Send a message with a swear word
        (let [msg (msg-utils/mock-message {:text "Oh damn that is tough"
                                           :sender "charlie"
                                           :chat-id 789
                                           :private? false})]
          (sut/process-message msg)

          ;; Verify stats
          (let [stats (db/get-user-stats "charlie" 789)]
            (is (= 1 (:total_messages stats)))
            (is (= 0 (:messages_since_last_swear stats)) "Should have broken streak")
            (is (= 1 (:swear_words_count stats)) "Should have 1 swear"))

          ;; Verify swear was logged
          (let [swears (db/get-last-two-swears "charlie" 789)]
            (is (= 1 (count swears)))
            (is (= "damn" (:swear_word (first swears))))))))))

(deftest test-unclaimed-message-fall-from-grace
  (testing "Fall from grace triggers scolding message"
    (msg-utils/with-clean-replies
      (with-redefs [sut/swear-words (delay #{"damn"})]
        ;; Build up a streak of 10+ messages
        (dotimes [_ 10]
          (let [msg (msg-utils/mock-message {:text "Clean message here"
                                             :sender "david"
                                             :chat-id 999
                                             :private? false})]
            (sut/process-message msg)))

        ;; Verify streak built up
        (let [stats-before (db/get-user-stats "david" 999)]
          (is (= 10 (:messages_since_last_swear stats-before))))

        ;; Now send a swear - should trigger fall from grace
        (let [msg (msg-utils/mock-message {:text "Oh damn it"
                                           :sender "david"
                                           :chat-id 999
                                           :private? false})]
          (sut/process-message msg)

          ;; Should get scolded for falling from grace
          (is (msg-utils/replied-with? "fallen from grace"))
          (is (msg-utils/replied-with? "david"))
          (is (msg-utils/replied-with? "10 messages")))))))

(deftest test-formatted-messages-not-analyzed
  (testing "Messages with formatting are excluded from analysis"
    (msg-utils/with-clean-replies
      (with-redefs [sut/swear-words (delay #{"damn"})]
        ;; Send messages with various formatting - none should be analyzed
        (let [formatted-msgs ["This is **bold** text"
                              "This is *italic* text"
                              "This has <b>HTML</b> tags"
                              "```code block```"]]
          (doseq [text formatted-msgs]
            (let [msg (msg-utils/mock-message {:text text
                                               :sender "eve"
                                               :chat-id 111
                                               :private? false})]
              (sut/process-message msg))))

        ;; No stats should be created
        (let [stats (db/get-user-stats "eve" 111)]
          (is (nil? stats) "Formatted messages should not be analyzed"))))))

(deftest test-long-messages-not-analyzed
  (testing "Messages longer than 1000 characters are excluded"
    (msg-utils/with-clean-replies
      ;; Create a long pasted message
      (let [long-text (apply str (repeat 1001 "x"))
            msg (msg-utils/mock-message {:text long-text
                                         :sender "frank"
                                         :chat-id 222
                                         :private? false})]
        (sut/process-message msg)

        ;; No stats should be created
        (let [stats (db/get-user-stats "frank" 222)]
          (is (nil? stats) "Long messages should not be analyzed"))))))

;; Integration tests for :allstats command

(deftest test-allstats-command-with-multiple-users
  (testing "Shows table of all users with statistics"
    (msg-utils/with-clean-replies
      ;; Create stats for multiple users
      (doseq [[user msgs words chars unique swears clean]
              [["alice" 100 500 2500 250 5 10]
               ["bob" 50 300 1500 150 2 5]
               ["charlie" 25 150 750 75 0 25]]]
        (sql/insert! test-db :message_stats
          {:username user :chatid 123
           :total_messages msgs
           :total_words words
           :total_chars chars
           :unique_words_count unique
           :swear_words_count swears
           :messages_since_last_swear clean
           :first_message_time (System/currentTimeMillis)
           :last_updated (System/currentTimeMillis)}))

      (let [msg (msg-utils/mock-command-message "allstats" nil {:sender "alice" :chat-id 123})]
        (sut/process-message msg)

        ;; Verify table output
        (is (msg-utils/replied-with? "Word Stats for All Users"))
        (is (msg-utils/replied-with? "<pre>"))  ; Monospace table
        (is (msg-utils/replied-with? "alice"))
        (is (msg-utils/replied-with? "bob"))
        (is (msg-utils/replied-with? "charlie"))
        (is (msg-utils/replied-with? "100"))    ; alice's message count
        (is (msg-utils/replied-with? "50"))     ; bob's message count
        (is (msg-utils/replied-with? "25"))))))  ; charlie's message count

(deftest test-allstats-command-no-users
  (testing "Handles no users gracefully"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "allstats" nil {:sender "nobody" :chat-id 999})]
        (sut/process-message msg)

        (is (msg-utils/replied-with? "No statistics available"))))))

(deftest test-allstats-filters-low-message-users
  (testing "Filters users with less than 5 messages"
    (msg-utils/with-clean-replies
      ;; Create users with varying message counts
      (doseq [[user msgs words] [["alice" 10 50]
                                  ["bob" 3 15]     ; Should be filtered (< 5)
                                  ["charlie" 20 100]]]
        (sql/insert! test-db :message_stats
          {:username user :chatid 456
           :total_messages msgs
           :total_words words
           :total_chars (* words 5)
           :unique_words_count (int (/ words 2))
           :swear_words_count 0
           :messages_since_last_swear msgs
           :first_message_time (System/currentTimeMillis)
           :last_updated (System/currentTimeMillis)}))

      (let [msg (msg-utils/mock-command-message "allstats" nil {:sender "alice" :chat-id 456})]
        (sut/process-message msg)

        ;; Verify alice and charlie are shown, but not bob
        (is (msg-utils/replied-with? "alice"))
        (is (msg-utils/replied-with? "charlie"))
        (is (not (msg-utils/replied-with? "bob")))))))  ; bob filtered out
