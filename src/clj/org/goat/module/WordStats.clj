(ns org.goat.module.WordStats
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.user-stats :as db]
            [clojure.string :as str])
  (:import [org.goat.core Constants]))

;; ============================================================================
;; Swear Word List
;; ============================================================================

(def swear-words
  "Set of swear words loaded from resources/swear_words.txt"
  (delay
    (try
      (-> (slurp "resources/swear_words.txt")
          (str/split #"\n")
          (->> (map str/trim)
               (map str/lower-case)
               (remove empty?)
               set))
      (catch Exception e
        (println "Warning: Could not load swear_words.txt:" (.getMessage e))
        #{}))))

;; ============================================================================
;; Message Filtering
;; ============================================================================

(defn contains-url?
  "Check if text contains a URL"
  [text]
  (or (str/includes? text "http://")
      (str/includes? text "https://")
      (str/includes? text "www.")))

(defn quoted-text?
  "Check if text is enclosed in quotes or is a quote reply"
  [text]
  (let [trimmed (str/trim text)]
    (or (re-matches #"^[\"'].*[\"']$" trimmed)
        (str/starts-with? trimmed ">"))))

(defn contains-words?
  "Verify message contains actual words (not just emojis/numbers)"
  [text]
  (re-find #"[a-zA-Z]{2,}" text))

(defn should-analyze-message?
  "Determines if a message should be analyzed"
  [text]
  (and text
       (not (contains-url? text))
       (not (quoted-text? text))
       (>= (count text) 3)
       (contains-words? text)))

;; ============================================================================
;; Text Analysis
;; ============================================================================

(defn tokenize-message
  "Split message into words, removing punctuation"
  [text]
  (->> (str/split text #"\s+")
       (map #(str/replace % #"[^\w]" ""))
       (remove empty?)
       (map str/lower-case)))

(defn extract-sentences
  "Split text into sentences"
  [text]
  (-> text
      (str/split #"[.!?]+")
      (->> (map str/trim)
           (remove empty?))))

(defn is-swear-word?
  "Check if a word is a swear word"
  [word]
  (contains? @swear-words (str/lower-case word)))

(defn extract-swear-words
  "Find all swear words in a list of words"
  [words]
  (filter is-swear-word? words))

(defn normalize-word
  "Normalize word for vocabulary tracking"
  [word]
  (-> word
      str/lower-case
      (str/replace #"[^\w]" "")))

(defn count-sentences
  "Count number of sentences in text"
  [text]
  (count (extract-sentences text)))

(defn calculate-avg-word-length
  "Calculate average word length from a list of words"
  [words]
  (if (empty? words)
    0.0
    (double (/ (reduce + (map count words)) (count words)))))

(defn calculate-avg-sentence-length
  "Calculate average sentence length (words per sentence)"
  [text word-count]
  (let [sentence-count (count-sentences text)]
    (if (> sentence-count 0)
      (double (/ word-count sentence-count))
      0.0)))

;; ============================================================================
;; Purity Tracking
;; ============================================================================

(defn check-purity-fall
  "Check if user just fell from purity and scold them if so"
  [m username chatid previous-streak]
  (when (>= previous-streak 10)
    (let [last-two-swears (db/get-last-two-swears username chatid)
          current-swear (first last-two-swears)
          swear-word (:swear_word current-swear)]
      (msg/reply m (format "💔 %s, you were pure for %d messages! You have fallen from grace with \"%s\". Shame! 🔔"
                           username
                           previous-streak
                           swear-word)))))

;; ============================================================================
;; Message Analysis and Storage
;; ============================================================================

(defn analyze-and-store-message
  "Analyze message and store stats. Returns true if message contained swears."
  [text username chatid]
  (let [words (tokenize-message text)
        word-count (count words)
        char-count (count text)
        swear-words (extract-swear-words words)
        had-swear (seq swear-words)
        stats (db/get-user-stats username chatid)
        current-message-num (or (:total_messages stats) 0)
        previous-streak (or (:messages_since_last_swear stats) 0)
        timestamp (System/currentTimeMillis)
        sentence-count (count-sentences text)
        avg-word-length (calculate-avg-word-length words)
        avg-sentence-length (calculate-avg-sentence-length text word-count)]

    ;; Update message stats
    (db/update-message-stats username chatid word-count char-count)

    ;; Update vocabulary
    (doseq [word words]
      (let [normalized (normalize-word word)]
        (when-not (empty? normalized)
          (db/add-or-update-word username chatid normalized))))

    ;; Update cached unique word count
    (db/update-unique-word-count username chatid)

    ;; Record message history
    (db/record-message-history username chatid
                               word-count
                               char-count
                               sentence-count
                               avg-word-length
                               avg-sentence-length
                               (count swear-words))

    ;; Handle swear words and purity tracking
    (if had-swear
      (do
        ;; Record each swear word
        (doseq [swear swear-words]
          (db/record-swear username chatid swear (inc current-message-num) timestamp))
        ;; Update purity stats (reset streak)
        (db/update-purity-stats username chatid 0 timestamp)
        ;; Return previous streak for fall detection
        {:had-swear true :previous-streak previous-streak})
      ;; No swear - increment clean streak
      (do
        (db/update-purity-stats username chatid (inc previous-streak) nil)
        {:had-swear false :previous-streak previous-streak}))))

;; ============================================================================
;; Display Functions
;; ============================================================================

(defn format-duration
  "Format milliseconds into a human-readable duration"
  [ms]
  (let [days (int (/ ms 1000 60 60 24))]
    (cond
      (= days 0) "today"
      (= days 1) "1 day"
      :else (str days " days"))))

(defn format-date
  "Format timestamp to a readable date"
  [timestamp]
  (when timestamp
    (let [date (java.util.Date. timestamp)
          formatter (java.text.SimpleDateFormat. "MMM dd, yyyy")]
      (.format formatter date))))

(defn show-user-stats
  "Display comprehensive word statistics for a user"
  [m username chatid]
  (let [stats (db/get-user-stats username chatid)]
    (if-not stats
      (msg/reply m (format "📊 No statistics available for %s yet." username))
      (let [total-messages (:total_messages stats)
            total-words (:total_words stats)
            total-chars (:total_chars stats)
            unique-words (:unique_words_count stats)
            swear-count (:swear_words_count stats)
            first-msg-time (:first_message_time stats)
            last-updated (:last_updated stats)
            msgs-since-swear (:messages_since_last_swear stats)

            days-active (if (and first-msg-time last-updated)
                          (int (/ (- last-updated first-msg-time) 1000 60 60 24))
                          0)
            msgs-per-week (or (db/calculate-messages-per-week username chatid) 0.0)
            msgs-per-day (or (db/calculate-messages-per-day username chatid) 0.0)
            recent-7days (db/get-recent-message-count username chatid 7)

            averages (db/calculate-averages username chatid)
            avg-word-length (:avg-word-length averages)
            avg-sent-length (:avg-sentence-length averages)
            avg-words-msg (:avg-words-per-message averages)

            vocab-percent (if (> total-words 0)
                            (* 100.0 (/ unique-words total-words))
                            0.0)
            swear-percent (if (> total-words 0)
                            (* 100.0 (/ swear-count total-words))
                            0.0)]

        (msg/reply m
          (format (str "📊 <b>Word Stats for %s</b>\n\n"
                      "<b>Activity:</b>\n"
                      "• Total Messages: %,d\n"
                      "• Active Since: %s (%d days)\n"
                      "• Messages/Week: %.1f\n"
                      "• Messages/Day: %.1f\n"
                      "• Recent: %d messages (last 7 days)\n\n"
                      "<b>Vocabulary:</b>\n"
                      "• Total Words: %,d\n"
                      "• Unique Words: %,d\n"
                      "• Vocabulary Size: %.1f%% uniqueness\n\n"
                      "<b>Averages:</b>\n"
                      "• Word Length: %.1f characters\n"
                      "• Sentence Length: %.1f words\n"
                      "• Words per Message: %.1f\n\n"
                      "<b>Language:</b>\n"
                      "• Swear Words: %,d (%.2f%%)\n"
                      "• Clean Streak: %d messages")
                  username
                  total-messages
                  (format-date first-msg-time) days-active
                  msgs-per-week
                  msgs-per-day
                  recent-7days
                  total-words
                  unique-words
                  vocab-percent
                  avg-word-length
                  avg-sent-length
                  avg-words-msg
                  swear-count
                  swear-percent
                  msgs-since-swear))))))

(defn show-purity-stats
  "Display purity report for a user"
  [m username chatid]
  (let [stats (db/get-user-stats username chatid)]
    (if-not stats
      (msg/reply m (format "✨ No purity data available for %s yet." username))
      (let [msgs-since-swear (:messages_since_last_swear stats)
            last-swear-time (:last_swear_time stats)
            total-swears (:swear_words_count stats)
            last-two-swears (db/get-last-two-swears username chatid)

            is-pure (> msgs-since-swear 0)
            last-swear-str (if last-swear-time
                             (format-duration (- (System/currentTimeMillis) last-swear-time))
                             "Never")
            status-emoji (if is-pure "😇" "😈")]

        (if is-pure
          (msg/reply m
            (format (str "✨ <b>Purity Report for %s</b>\n\n"
                        "Status: PURE %s\n"
                        "Clean Streak: %d messages\n"
                        "Last Swear: %s\n"
                        "Total Swears: %,d (lifetime)\n\n"
                        "Keep up the good work!")
                    username
                    status-emoji
                    msgs-since-swear
                    last-swear-str
                    total-swears))
          (msg/reply m
            (format (str "💔 <b>Purity Report for %s</b>\n\n"
                        "Status: IMPURE %s\n"
                        "Clean Streak: %d messages\n"
                        "Last Swear: %s\n"
                        "Total Swears: %,d (lifetime)\n\n"
                        "Time to clean up your act!")
                    username
                    status-emoji
                    msgs-since-swear
                    last-swear-str
                    total-swears)))))))

;; ============================================================================
;; Module Definition
;; ============================================================================

(defmodule WordStats
  :commands [:stats :wordstats :mystats :purity]
  :receive-messages :unclaimed

  (defn process-channel-message [m]
    (let [text (msg/get-text m)
          username (msg/sender m)
          chatid (msg/chat-id m)
          command (msg/command m)]

      (if command
        ;; Handle stats commands
        (case command
          :stats (show-user-stats m username chatid)
          :wordstats (show-user-stats m username chatid)
          :mystats (show-user-stats m username chatid)
          :purity (show-purity-stats m username chatid)
          nil)
        ;; Analyze unclaimed messages
        (when (should-analyze-message? text)
          (let [result (analyze-and-store-message text username chatid)]
            ;; Check for "fall from grace"
            (when (:had-swear result)
              (check-purity-fall m username chatid (:previous-streak result)))))))))
