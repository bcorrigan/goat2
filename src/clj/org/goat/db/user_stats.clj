(ns org.goat.db.user-stats
  (:require [clojure.java.jdbc :refer :all :as sql]
            [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/user_stats.db"})

(defn create-db
  "If no DB file found, create the user stats db and tables"
  []
  ;; Create message_stats table
  (when-not (util/tbl-exists? db :message_stats)
    (try
      (sql/db-do-commands db
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
      (sql/execute! db "CREATE INDEX stats_user_idx ON message_stats(username)")
      (sql/execute! db "CREATE INDEX stats_chat_idx ON message_stats(chatid)")
      (catch Exception e
        (println "Cannot create message_stats table" (.getMessage e)))))

  ;; Create user_vocabulary table
  (when-not (util/tbl-exists? db :user_vocabulary)
    (try
      (sql/db-do-commands db
        (sql/create-table-ddl :user_vocabulary
          [[:username :text]
           [:chatid :integer]
           [:word :text]
           [:frequency :integer "DEFAULT 1"]
           [:first_seen :datetime]
           [:last_seen :datetime]
           ["PRIMARY KEY (username, chatid, word)"]]))
      (sql/execute! db "CREATE INDEX vocab_user_idx ON user_vocabulary(username, chatid)")
      (catch Exception e
        (println "Cannot create user_vocabulary table" (.getMessage e)))))

  ;; Create message_history table
  (when-not (util/tbl-exists? db :message_history)
    (try
      (sql/db-do-commands db
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
      (sql/execute! db "CREATE INDEX msg_hist_user_idx ON message_history(username, chatid)")
      (sql/execute! db "CREATE INDEX msg_hist_time_idx ON message_history(timestamp)")
      (sql/execute! db "CREATE INDEX msg_hist_user_time_idx ON message_history(username, chatid, timestamp)")
      (catch Exception e
        (println "Cannot create message_history table" (.getMessage e)))))

  ;; Create swear_history table
  (when-not (util/tbl-exists? db :swear_history)
    (try
      (sql/db-do-commands db
        (sql/create-table-ddl :swear_history
          [[:username :text]
           [:chatid :integer]
           [:swear_word :text]
           [:message_number :integer]
           [:timestamp :datetime]]))
      (sql/execute! db "CREATE INDEX swear_hist_user_idx ON swear_history(username, chatid)")
      (sql/execute! db "CREATE INDEX swear_hist_time_idx ON swear_history(timestamp)")
      (sql/execute! db "CREATE INDEX swear_hist_user_time_idx ON swear_history(username, chatid, timestamp)")
      (catch Exception e
        (println "Cannot create swear_history table" (.getMessage e))))))

;; ============================================================================
;; Message Stats Functions
;; ============================================================================

(defn get-user-stats
  "Get the current stats record for a user, or nil if none exists"
  [username chatid]
  (-> (sql/query db ["SELECT * FROM message_stats WHERE username=? AND chatid=?" username chatid])
      first))

(defn update-message-stats
  "Update message statistics for a user. Creates record if it doesn't exist."
  [username chatid word-count char-count]
  (let [existing (get-user-stats username chatid)
        timestamp (System/currentTimeMillis)]
    (if existing
      ;; Update existing record
      (sql/execute! db
        ["UPDATE message_stats
          SET total_messages = total_messages + 1,
              total_words = total_words + ?,
              total_chars = total_chars + ?,
              last_updated = ?
          WHERE username=? AND chatid=?"
         word-count char-count timestamp username chatid])
      ;; Create new record
      (sql/insert! db :message_stats
        {:username username
         :chatid chatid
         :total_messages 1
         :total_words word-count
         :total_chars char-count
         :messages_since_last_swear 1
         :first_message_time timestamp
         :last_updated timestamp}))))

;; ============================================================================
;; Vocabulary Functions
;; ============================================================================

(defn add-or-update-word
  "Add a word to user's vocabulary or increment its frequency"
  [username chatid word]
  (let [timestamp (System/currentTimeMillis)
        existing (-> (sql/query db
                       ["SELECT * FROM user_vocabulary WHERE username=? AND chatid=? AND word=?"
                        username chatid word])
                     first)]
    (if existing
      ;; Update frequency and last_seen
      (sql/execute! db
        ["UPDATE user_vocabulary
          SET frequency = frequency + 1,
              last_seen = ?
          WHERE username=? AND chatid=? AND word=?"
         timestamp username chatid word])
      ;; Insert new word
      (sql/insert! db :user_vocabulary
        {:username username
         :chatid chatid
         :word word
         :frequency 1
         :first_seen timestamp
         :last_seen timestamp}))))

(defn get-vocabulary-size
  "Get the count of unique words for a user"
  [username chatid]
  (-> (sql/query db
        ["SELECT COUNT(*) as count FROM user_vocabulary WHERE username=? AND chatid=?"
         username chatid])
      first
      :count))

(defn update-unique-word-count
  "Update the cached unique word count in message_stats"
  [username chatid]
  (let [vocab-size (get-vocabulary-size username chatid)]
    (sql/execute! db
      ["UPDATE message_stats SET unique_words_count = ? WHERE username=? AND chatid=?"
       vocab-size username chatid])))

;; ============================================================================
;; Message History Functions
;; ============================================================================

(defn record-message-history
  "Store individual message metrics"
  [username chatid word-count char-count sentence-count avg-word-length avg-sentence-length swear-count]
  (sql/insert! db :message_history
    {:username username
     :chatid chatid
     :word_count word-count
     :char_count char-count
     :sentence_count sentence-count
     :avg_word_length avg-word-length
     :avg_sentence_length avg-sentence-length
     :swear_word_count swear-count
     :timestamp (System/currentTimeMillis)}))

(defn get-messages-in-timeframe
  "Get all messages for a user within a time window (in milliseconds)"
  [username chatid start-time end-time]
  (sql/query db
    ["SELECT * FROM message_history
      WHERE username=? AND chatid=?
      AND timestamp >= ? AND timestamp <= ?
      ORDER BY timestamp DESC"
     username chatid start-time end-time]))

;; ============================================================================
;; Purity Tracking Functions
;; ============================================================================

(defn record-swear
  "Log a swear word instance to swear_history"
  [username chatid swear-word message-number timestamp]
  (sql/insert! db :swear_history
    {:username username
     :chatid chatid
     :swear_word swear-word
     :message_number message-number
     :timestamp timestamp}))

(defn update-purity-stats
  "Update purity-related stats. Pass timestamp only when swear detected."
  [username chatid messages-since-last-swear last-swear-time]
  (let [stats (get-user-stats username chatid)]
    (if last-swear-time
      ;; User swore - update swear count and reset streak
      (sql/execute! db
        ["UPDATE message_stats
          SET messages_since_last_swear = ?,
              last_swear_time = ?,
              swear_words_count = swear_words_count + 1
          WHERE username=? AND chatid=?"
         messages-since-last-swear last-swear-time username chatid])
      ;; User didn't swear - just increment streak
      (sql/execute! db
        ["UPDATE message_stats
          SET messages_since_last_swear = ?
          WHERE username=? AND chatid=?"
         messages-since-last-swear username chatid]))))

(defn get-last-two-swears
  "Get the two most recent swear instances for a user"
  [username chatid]
  (sql/query db
    ["SELECT * FROM swear_history
      WHERE username=? AND chatid=?
      ORDER BY timestamp DESC
      LIMIT 2"
     username chatid]))

(defn get-purity-stats
  "Get purity-related statistics for a user"
  [username chatid]
  (let [stats (get-user-stats username chatid)]
    (select-keys stats [:messages_since_last_swear :last_swear_time :swear_words_count])))

;; ============================================================================
;; Time-Based Calculation Functions
;; ============================================================================

(defn calculate-messages-per-week
  "Calculate average messages per week for a user"
  [username chatid]
  (let [stats (get-user-stats username chatid)
        first-msg-time (:first_message_time stats)
        last-msg-time (:last_updated stats)
        total-messages (:total_messages stats)]
    (when (and first-msg-time last-msg-time (> total-messages 0))
      (let [time-diff-ms (- last-msg-time first-msg-time)
            weeks (/ time-diff-ms 1000 60 60 24 7.0)]
        (if (> weeks 0)
          (double (/ total-messages weeks))
          0.0)))))

(defn calculate-messages-per-day
  "Calculate average messages per day for a user"
  [username chatid]
  (let [stats (get-user-stats username chatid)
        first-msg-time (:first_message_time stats)
        last-msg-time (:last_updated stats)
        total-messages (:total_messages stats)]
    (when (and first-msg-time last-msg-time (> total-messages 0))
      (let [time-diff-ms (- last-msg-time first-msg-time)
            days (/ time-diff-ms 1000 60 60 24.0)]
        (if (> days 0)
          (double (/ total-messages days))
          0.0)))))

(defn get-recent-message-count
  "Get count of messages in the last N days"
  [username chatid days]
  (let [now (System/currentTimeMillis)
        cutoff (- now (* days 24 60 60 1000))]
    (-> (sql/query db
          ["SELECT COUNT(*) as count FROM message_history
            WHERE username=? AND chatid=? AND timestamp >= ?"
           username chatid cutoff])
        first
        :count)))

(defn calculate-averages
  "Calculate average word length, sentence length, and words per message"
  [username chatid]
  (let [stats (get-user-stats username chatid)
        total-messages (:total_messages stats)
        total-words (:total_words stats)
        total-chars (:total_chars stats)]
    (when (and stats (> total-messages 0))
      {:avg-word-length (if (> total-words 0)
                          (double (/ total-chars total-words))
                          0.0)
       :avg-words-per-message (double (/ total-words total-messages))
       ;; For sentence length, we need to query message_history
       :avg-sentence-length
       (let [result (sql/query db
                      ["SELECT AVG(avg_sentence_length) as avg_sent_len
                        FROM message_history
                        WHERE username=? AND chatid=? AND avg_sentence_length > 0"
                       username chatid])]
         (or (:avg_sent_len (first result)) 0.0))})))

;; Initialize database on load
(create-db)
