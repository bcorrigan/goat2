(ns org.goat.db.user-stats-test
  (:require [org.goat.db.user-stats :as sut]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as sql])
  (:import [java.io File]))

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-user-stats.db"})

(defn setup-test-db []
  (let [db-file (File. "test/resources/test-user-stats.db")]
    (when (.exists db-file) (.delete db-file)))

  ;; Create message_stats table
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

  ;; Create user_vocabulary table
  (sql/db-do-commands test-db
    (sql/create-table-ddl :user_vocabulary
      [[:username :text]
       [:chatid :integer]
       [:word :text]
       [:frequency :integer "DEFAULT 1"]
       [:first_seen :datetime]
       [:last_seen :datetime]
       ["PRIMARY KEY (username, chatid, word)"]]))

  ;; Create message_history table
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

  ;; Create swear_history table
  (sql/db-do-commands test-db
    (sql/create-table-ddl :swear_history
      [[:username :text]
       [:chatid :integer]
       [:swear_word :text]
       [:message_number :integer]
       [:timestamp :datetime]])))

(defn teardown-test-db []
  (let [db-file (File. "test/resources/test-user-stats.db")]
    (when (.exists db-file) (.delete db-file))))

(defn db-fixture [f]
  (setup-test-db)
  (with-redefs [sut/db test-db]
    (f))
  (teardown-test-db))

(use-fixtures :each db-fixture)

;; ============================================================================
;; Message Stats Tests
;; ============================================================================

(deftest test-update-message-stats-creates-new-record
  (testing "update-message-stats creates new record when none exists"
    (sut/update-message-stats "alice" 123 10 50)
    (let [stats (sut/get-user-stats "alice" 123)]
      (is (= 1 (:total_messages stats)))
      (is (= 10 (:total_words stats)))
      (is (= 50 (:total_chars stats)))
      (is (some? (:first_message_time stats)))
      (is (some? (:last_updated stats))))))

(deftest test-update-message-stats-increments-existing
  (testing "update-message-stats increments existing record"
    (sut/update-message-stats "bob" 456 5 25)
    (sut/update-message-stats "bob" 456 8 40)
    (let [stats (sut/get-user-stats "bob" 456)]
      (is (= 2 (:total_messages stats)))
      (is (= 13 (:total_words stats)))
      (is (= 65 (:total_chars stats))))))

(deftest test-get-user-stats-returns-nil-when-no-data
  (testing "get-user-stats returns nil for non-existent user"
    (is (nil? (sut/get-user-stats "nobody" 999)))))

;; ============================================================================
;; Vocabulary Tests
;; ============================================================================

(deftest test-add-word-creates-new-entry
  (testing "add-or-update-word creates new vocabulary entry"
    (sut/add-or-update-word "alice" 123 "hello")
    (let [result (sql/query test-db
                   ["SELECT * FROM user_vocabulary WHERE username=? AND chatid=? AND word=?"
                    "alice" 123 "hello"])]
      (is (= 1 (count result)))
      (is (= 1 (:frequency (first result)))))))

(deftest test-add-word-increments-frequency
  (testing "add-or-update-word increments frequency for existing word"
    (sut/add-or-update-word "bob" 456 "test")
    (sut/add-or-update-word "bob" 456 "test")
    (sut/add-or-update-word "bob" 456 "test")
    (let [result (sql/query test-db
                   ["SELECT * FROM user_vocabulary WHERE username=? AND chatid=? AND word=?"
                    "bob" 456 "test"])]
      (is (= 3 (:frequency (first result)))))))

(deftest test-get-vocabulary-size
  (testing "get-vocabulary-size returns correct count"
    (sut/add-or-update-word "alice" 123 "hello")
    (sut/add-or-update-word "alice" 123 "world")
    (sut/add-or-update-word "alice" 123 "hello") ; duplicate - shouldn't increase count
    (is (= 2 (sut/get-vocabulary-size "alice" 123)))))

(deftest test-vocabulary-isolated-by-user-and-chat
  (testing "Vocabulary is isolated per user/chat combination"
    (sut/add-or-update-word "alice" 123 "test")
    (sut/add-or-update-word "alice" 456 "test")
    (sut/add-or-update-word "bob" 123 "test")

    (is (= 1 (sut/get-vocabulary-size "alice" 123)))
    (is (= 1 (sut/get-vocabulary-size "alice" 456)))
    (is (= 1 (sut/get-vocabulary-size "bob" 123)))))

;; ============================================================================
;; Message History Tests
;; ============================================================================

(deftest test-record-message-history
  (testing "record-message-history stores message metrics"
    (sut/record-message-history "alice" 123 10 50 2 5.0 5.0 0)
    (let [history (sql/query test-db
                    ["SELECT * FROM message_history WHERE username=? AND chatid=?"
                     "alice" 123])]
      (is (= 1 (count history)))
      (is (= 10 (:word_count (first history))))
      (is (= 50 (:char_count (first history))))
      (is (= 2 (:sentence_count (first history)))))))

(deftest test-get-messages-in-timeframe
  (testing "get-messages-in-timeframe returns messages in window"
    (let [now (System/currentTimeMillis)
          day-ms (* 24 60 60 1000)
          yesterday (- now day-ms)
          two-days-ago (- now (* 2 day-ms))]

      ;; Insert messages at different times
      (sql/insert! test-db :message_history
        {:username "alice" :chatid 123 :word_count 10 :char_count 50
         :sentence_count 2 :avg_word_length 5.0 :avg_sentence_length 5.0
         :swear_word_count 0 :timestamp two-days-ago})

      (sql/insert! test-db :message_history
        {:username "alice" :chatid 123 :word_count 15 :char_count 75
         :sentence_count 3 :avg_word_length 5.0 :avg_sentence_length 5.0
         :swear_word_count 1 :timestamp yesterday})

      (let [recent (sut/get-messages-in-timeframe "alice" 123 yesterday now)]
        (is (= 1 (count recent)))
        (is (= 15 (:word_count (first recent))))))))

;; ============================================================================
;; Purity Tracking Tests
;; ============================================================================

(deftest test-record-swear
  (testing "record-swear stores swear instance"
    (let [timestamp (System/currentTimeMillis)]
      (sut/record-swear "alice" 123 "damn" 5 timestamp)
      (let [swears (sql/query test-db
                     ["SELECT * FROM swear_history WHERE username=? AND chatid=?"
                      "alice" 123])]
        (is (= 1 (count swears)))
        (is (= "damn" (:swear_word (first swears))))
        (is (= 5 (:message_number (first swears))))))))

(deftest test-get-last-two-swears
  (testing "get-last-two-swears returns most recent swears in order"
    (let [now (System/currentTimeMillis)]
      (sut/record-swear "alice" 123 "first" 1 (- now 3000))
      (sut/record-swear "alice" 123 "second" 2 (- now 2000))
      (sut/record-swear "alice" 123 "third" 3 (- now 1000))

      (let [swears (sut/get-last-two-swears "alice" 123)]
        (is (= 2 (count swears)))
        (is (= "third" (:swear_word (first swears))))
        (is (= "second" (:swear_word (second swears))))))))

(deftest test-update-purity-stats-on-swear
  (testing "update-purity-stats updates swear count and resets streak"
    ;; Create initial stats
    (sut/update-message-stats "alice" 123 10 50)

    ;; User had a clean streak, now swears
    (let [timestamp (System/currentTimeMillis)]
      (sut/update-purity-stats "alice" 123 0 timestamp)

      (let [stats (sut/get-user-stats "alice" 123)]
        (is (= 0 (:messages_since_last_swear stats)))
        (is (= 1 (:swear_words_count stats)))
        (is (= timestamp (:last_swear_time stats)))))))

(deftest test-update-purity-stats-clean-message
  (testing "update-purity-stats increments streak on clean message"
    ;; Create initial stats
    (sut/update-message-stats "alice" 123 10 50)

    ;; Clean message - increment streak (no timestamp)
    (sut/update-purity-stats "alice" 123 1 nil)
    (sut/update-purity-stats "alice" 123 2 nil)

    (let [stats (sut/get-user-stats "alice" 123)]
      (is (= 2 (:messages_since_last_swear stats)))
      (is (= 0 (:swear_words_count stats))))))

(deftest test-purity-fall-scenario
  (testing "Complete purity fall scenario"
    ;; Setup: user sends clean messages building up a streak
    (sut/update-message-stats "bob" 456 5 25)
    (sut/update-purity-stats "bob" 456 1 nil)
    (sut/update-message-stats "bob" 456 8 40)
    (sut/update-purity-stats "bob" 456 2 nil)
    (sut/update-message-stats "bob" 456 6 30)
    (sut/update-purity-stats "bob" 456 3 nil)

    ;; Verify clean streak
    (let [stats-before (sut/get-user-stats "bob" 456)]
      (is (= 3 (:messages_since_last_swear stats-before)))
      (is (= 0 (:swear_words_count stats-before))))

    ;; User swears - fall from grace
    (let [swear-time (System/currentTimeMillis)]
      (sut/record-swear "bob" 456 "shit" 4 swear-time)
      (sut/update-purity-stats "bob" 456 0 swear-time)

      ;; Verify fall
      (let [stats-after (sut/get-user-stats "bob" 456)
            last-swears (sut/get-last-two-swears "bob" 456)]
        (is (= 0 (:messages_since_last_swear stats-after)))
        (is (= 1 (:swear_words_count stats-after)))
        (is (= 1 (count last-swears)))
        (is (= "shit" (:swear_word (first last-swears))))))))

;; ============================================================================
;; Time-Based Calculation Tests
;; ============================================================================

(deftest test-calculate-messages-per-week
  (testing "calculate-messages-per-week computes correct average"
    (let [now (System/currentTimeMillis)
          week-ms (* 7 24 60 60 1000)
          two-weeks-ago (- now (* 2 week-ms))]

      ;; Create user with 14 messages over 2 weeks
      (sql/insert! test-db :message_stats
        {:username "alice" :chatid 123
         :total_messages 14
         :total_words 140
         :total_chars 700
         :unique_words_count 0
         :swear_words_count 0
         :messages_since_last_swear 14
         :first_message_time two-weeks-ago
         :last_updated now})

      (let [msgs-per-week (sut/calculate-messages-per-week "alice" 123)]
        (is (number? msgs-per-week))
        (is (>= msgs-per-week 6.0))  ; Should be ~7 msgs/week
        (is (<= msgs-per-week 8.0))))))

(deftest test-calculate-messages-per-day
  (testing "calculate-messages-per-day computes correct average"
    (let [now (System/currentTimeMillis)
          day-ms (* 24 60 60 1000)
          ten-days-ago (- now (* 10 day-ms))]

      ;; Create user with 20 messages over 10 days
      (sql/insert! test-db :message_stats
        {:username "bob" :chatid 456
         :total_messages 20
         :total_words 200
         :total_chars 1000
         :unique_words_count 0
         :swear_words_count 0
         :messages_since_last_swear 20
         :first_message_time ten-days-ago
         :last_updated now})

      (let [msgs-per-day (sut/calculate-messages-per-day "bob" 456)]
        (is (number? msgs-per-day))
        (is (>= msgs-per-day 1.5))  ; Should be ~2 msgs/day
        (is (<= msgs-per-day 2.5))))))

(deftest test-get-recent-message-count
  (testing "get-recent-message-count returns count in window"
    (let [now (System/currentTimeMillis)
          day-ms (* 24 60 60 1000)]

      ;; Insert messages at various times
      (sql/insert! test-db :message_history
        {:username "alice" :chatid 123 :word_count 10 :char_count 50
         :sentence_count 2 :avg_word_length 5.0 :avg_sentence_length 5.0
         :swear_word_count 0 :timestamp (- now (* 2 day-ms))})

      (sql/insert! test-db :message_history
        {:username "alice" :chatid 123 :word_count 10 :char_count 50
         :sentence_count 2 :avg_word_length 5.0 :avg_sentence_length 5.0
         :swear_word_count 0 :timestamp (- now (* 5 day-ms))})

      (sql/insert! test-db :message_history
        {:username "alice" :chatid 123 :word_count 10 :char_count 50
         :sentence_count 2 :avg_word_length 5.0 :avg_sentence_length 5.0
         :swear_word_count 0 :timestamp (- now (* 10 day-ms))})

      (is (= 2 (sut/get-recent-message-count "alice" 123 7)))
      (is (= 3 (sut/get-recent-message-count "alice" 123 30))))))

;; ============================================================================
;; Average Calculation Tests
;; ============================================================================

(deftest test-calculate-averages
  (testing "calculate-averages computes correct statistics"
    ;; Create stats: 3 messages, 30 total words, 150 total chars
    (sql/insert! test-db :message_stats
      {:username "alice" :chatid 123
       :total_messages 3
       :total_words 30
       :total_chars 150
       :unique_words_count 20
       :swear_words_count 0
       :messages_since_last_swear 3
       :first_message_time (System/currentTimeMillis)
       :last_updated (System/currentTimeMillis)})

    ;; Add message history for sentence length calculation
    (sql/insert! test-db :message_history
      {:username "alice" :chatid 123 :word_count 10 :char_count 50
       :sentence_count 2 :avg_word_length 5.0 :avg_sentence_length 5.0
       :swear_word_count 0 :timestamp (System/currentTimeMillis)})

    (let [avgs (sut/calculate-averages "alice" 123)]
      (is (= 5.0 (:avg-word-length avgs)))      ; 150 chars / 30 words = 5
      (is (= 10.0 (:avg-words-per-message avgs))) ; 30 words / 3 messages = 10
      (is (number? (:avg-sentence-length avgs))))))
