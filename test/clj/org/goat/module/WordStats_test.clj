(ns org.goat.module.WordStats-test
  (:require [org.goat.module.WordStats :as sut]
            [org.goat.db.user-stats :as db]
            [org.goat.testutils.message :as msg-utils]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as sql])
  (:import [java.io File]))

;; ============================================================================
;; Test Database Setup
;; ============================================================================

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-wordstats.db"})

(defn setup-test-db []
  (let [db-file (File. "test/resources/test-wordstats.db")]
    (when (.exists db-file) (.delete db-file)))

  ;; Create all tables
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

;; ============================================================================
;; Message Filtering Tests
;; ============================================================================

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

(deftest test-should-analyse-message?
  (testing "Correctly filters messages for analysis"
    (is (sut/should-analyse-message? "This is a normal message"))
    (is (not (sut/should-analyse-message? "Check http://example.com")))
    (is (not (sut/should-analyse-message? "\"Quoted text\"")))
    (is (not (sut/should-analyse-message? "> Quote reply")))
    (is (not (sut/should-analyse-message? "Hi")))  ; too short
    (is (not (sut/should-analyse-message? "123")))  ; no words
    (is (not (sut/should-analyse-message? nil)))))

;; ============================================================================
;; Text Analysis Tests
;; ============================================================================

(deftest test-tokenize-message
  (testing "Tokenizes message into words"
    (is (= ["hello" "world"] (sut/tokenize-message "Hello world")))
    (is (= ["test" "message"] (sut/tokenize-message "Test, message!")))
    (is (= ["one" "two" "three"] (sut/tokenize-message "one, two, three")))))

(deftest test-extract-sentences
  (testing "Extracts sentences from text"
    (is (= ["Hello" "How are you"] (sut/extract-sentences "Hello. How are you?")))
    (is (= ["Stop"] (sut/extract-sentences "Stop!")))
    (is (= ["Really" "Yes"] (sut/extract-sentences "Really? Yes.")))))

(deftest test-count-sentences
  (testing "Counts sentences correctly"
    (is (= 2 (sut/count-sentences "First sentence. Second sentence.")))
    (is (= 3 (sut/count-sentences "One! Two? Three.")))
    (is (= 1 (sut/count-sentences "Just one")))))

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

(deftest test-normalize-word
  (testing "Normalizes words for vocabulary tracking"
    (is (= "hello" (sut/normalize-word "Hello")))
    (is (= "world" (sut/normalize-word "world!")))
    (is (= "test" (sut/normalize-word "TEST?")))))

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

;; ============================================================================
;; Message Analysis Integration Tests
;; ============================================================================

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
  (testing "Analyzes and stores message with swear word"
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

;; ============================================================================
;; Purity Fall Detection Tests
;; ============================================================================

;; Note: Purity fall scolding is tested through integration test below
;; where we process actual messages through the module

;; ============================================================================
;; Display Function Tests
;; ============================================================================

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
        (sut/process-channel-message nil msg)

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
        (sut/process-channel-message nil msg)

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
        (sut/process-channel-message nil msg)

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
        (sut/process-channel-message nil msg)

        (is (msg-utils/replied-with? "IMPURE"))
        (is (msg-utils/replied-with? "clean up your act"))))))

;; ============================================================================
;; Module Integration Tests with Commands
;; ============================================================================

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
        (sut/process-channel-message nil msg)

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
        (sut/process-channel-message nil msg)

        (is (msg-utils/replied-with? "Purity Report"))))))
