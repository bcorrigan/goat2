(ns org.goat.module.Remind
  "Reminder module - allows users to set time-based reminders with natural language"
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.message-parse :as msg-parse]
            [org.goat.db.reminders :as db]
            [clojure.core.async :refer [go-loop timeout <!]]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [com.zoho.hawking HawkingTimeParser]
           [com.zoho.hawking.datetimeparser.configuration HawkingConfiguration]
           [java.time Instant ZonedDateTime ZoneId DayOfWeek]
           [java.time.format DateTimeFormatter]
           [java.time.temporal TemporalAdjusters]))

;; ============================================================================
;; Natural Language Date Parser (Zoho Hawking)
;; ============================================================================

(def ^:private hawking-config
  "Hawking configuration for UK timezone and date format"
  (doto (com.zoho.hawking.datetimeparser.configuration.HawkingConfiguration.)
    (.setFiscalYearStart 4)
    (.setFiscalYearEnd 3)
    (.setTimeZone "Europe/London")
    (.setDateFormat "DD/MM/YYYY")))

(def ^:private parser
  "Hawking time parser instance"
  (HawkingTimeParser.))

(defn preprocess-date-text
  "Preprocess date text to convert 2-digit years to 4-digit years.
   Converts DD/MM/YY to DD/MM/20YY to avoid year 27 AD being interpreted as year 0027."
  [text]
  (str/replace text #"\b(\d{1,2}/\d{1,2}/)(\d{2})\b" "$120$2"))

(defn parse-date
  "Parse natural language date/time from text.
   Returns timestamp in milliseconds (even if in the past), or nil if parsing fails.
   The caller should check if the timestamp is in the past.
   Examples: 'tomorrow at 3pm', 'next Tuesday', 'in 2 hours'"
  [text]
  (try
    (let [preprocessed (preprocess-date-text text)
          dates-found (.parse parser preprocessed (java.util.Date.) hawking-config "eng")
          parsed-dates (.getParserOutputs dates-found)]
      (when (seq parsed-dates)
        ;; Get the nearest date (first in sorted order)
        (let [sorted-dates (sort-by #(.getMillis (.getStart (.getDateRange %))) parsed-dates)
              first-date (first sorted-dates)
              date-range (.getDateRange first-date)
              timestamp (.getMillis (.getStart date-range))]
          timestamp)))
    (catch Exception e
      nil)))

;; ============================================================================
;; Command Parsing
;; ============================================================================

;; Helper functions for parsing recurring commands

(defn weekday-keyword
  "Convert weekday string to keyword"
  [weekday-str]
  (when weekday-str
    (keyword (.toLowerCase weekday-str))))

(defn ordinal-number
  "Convert ordinal string to number"
  [ordinal-str]
  (when ordinal-str
    (case (.toLowerCase ordinal-str)
      "second" 2
      "third" 3
      "fourth" 4
      "fifth" 5
      "sixth" 6
      "seventh" 7
      "eighth" 8
      "ninth" 9
      "tenth" 10
      nil)))

(defn position-keyword
  "Convert position string to keyword"
  [position-str]
  (when position-str
    (keyword (.toLowerCase position-str))))

(defn parse-interval-command
  "Parse 'remind <person> every <N> seconds to <task>' for testing.
   Returns {:target username :pattern map :task text} or nil."
  [text]
  (when text
    (let [trimmed (str/trim text)]
      (when-let [[_ target-text seconds task] (re-find #"^(\S+)\s+every\s+(\d+)\s+seconds?\s+to\s+(.+)$" trimmed)]
        (let [target (if (= "me" (.toLowerCase target-text))
                       nil
                       target-text)]
          {:target target
           :recurrence {:type :interval
                        :seconds (Integer/parseInt seconds)}
           :task (str/trim task)})))))

(defn parse-weekly-command
  "Parse 'remind <person> every [ordinal] <weekday> [at/in] <time> starting <date> to <task>'.
   - 'every friday' → weekly (every week)
   - 'every third friday' → interval-weekly (every 3 weeks on Friday)
   Returns {:target username :pattern map :task text} or nil."
  [text]
  (when text
    (let [trimmed (str/trim text)]
      (when-let [[_ target-text ordinal weekday time-text start-date task]
                 (re-find #"^(\S+)\s+every\s+(?:(second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\s+)?(\w+)\s+(?:(?:at|in)\s+)?(.+?)\s+starting\s+(.+?)\s+to\s+(.+)$" trimmed)]
        (let [target (if (= "me" (.toLowerCase target-text))
                       nil
                       target-text)]
          {:target target
           :recurrence (if ordinal
                         {:type :interval-weekly
                          :weeks (ordinal-number ordinal)
                          :weekday (weekday-keyword weekday)
                          :time-text (str/trim time-text)
                          :start-date (str/trim start-date)}
                         {:type :weekly
                          :weekday (weekday-keyword weekday)
                          :time-text (str/trim time-text)
                          :start-date (str/trim start-date)})
           :task (str/trim task)})))))

(defn parse-monthly-command
  "Parse 'remind <person> each <position> <weekday|day> of the month [at/in] <time> to <task>'.
   Returns {:target username :pattern map :task text} or nil."
  [text]
  (when text
    (let [trimmed (str/trim text)]
      (when-let [[_ target-text position day-or-weekday time-text task]
                 (re-find #"^(\S+)\s+each\s+(first|second|third|fourth|last)\s+(monday|tuesday|wednesday|thursday|friday|saturday|sunday|day)\s+of\s+the\s+month\s+(?:(?:at|in)\s+)?(.+?)\s+to\s+(.+)$" trimmed)]
        (let [target (if (= "me" (.toLowerCase target-text))
                       nil
                       target-text)]
          {:target target
           :recurrence (if (= "day" day-or-weekday)
                         {:type :monthly-lastday
                          :time-text (str/trim time-text)}
                         {:type :monthly-weekday
                          :position (position-keyword position)
                          :weekday (weekday-keyword day-or-weekday)
                          :time-text (str/trim time-text)})
           :task (str/trim task)})))))

(defn parse-monthly-day-command
  "Parse 'remind <person> each <N>(st|nd|rd|th) of the month [at/in] <time> to <task>'.
   Supports days 1-31. Returns {:target username :pattern map :task text} or nil."
  [text]
  (when text
    (let [trimmed (str/trim text)]
      (when-let [[_ target-text day-num time-text task]
                 (re-find #"^(\S+)\s+each\s+(\d{1,2})(?:st|nd|rd|th)\s+of\s+the\s+month\s+(?:(?:at|in)\s+)?(.+?)\s+to\s+(.+)$" trimmed)]
        (let [day (Integer/parseInt day-num)]
          (when (and (>= day 1) (<= day 31))
            (let [target (if (= "me" (.toLowerCase target-text))
                           nil
                           target-text)]
              {:target target
               :recurrence {:type :monthly-day
                            :day-of-month day
                            :time-text (str/trim time-text)}
               :task (str/trim task)})))))))

(defn parse-recurring-command
  "Parse any recurring reminder command. Returns nil if not a recurring command."
  [text]
  (or (parse-interval-command text)
      (parse-weekly-command text)
      (parse-monthly-day-command text)
      (parse-monthly-command text)))

(defn parse-remind-command
  "Parse 'remind <person> at/in <time> to <task>' command.
   Returns {:target username :time-text text :task text} or nil if invalid format."
  [text]
  (when text
    (let [trimmed (str/trim text)]
      ;; Match: <person> [at/in] <time> to <task> (at/in is optional)
      (when-let [[_ target-text time-text task] (re-find #"^(\S+)\s+(?:(?:at|in)\s+)?(.+?)\s+to\s+(.+)$" trimmed)]
        (let [target (if (= "me" (.toLowerCase target-text))
                       nil  ; Will use message sender
                       target-text)]
          {:target target
           :time-text (str/trim time-text)
           :task (str/trim task)})))))

;; ============================================================================
;; Date/Time Formatting
;; ============================================================================

(defn format-datetime
  "Format a timestamp (milliseconds) into a human-readable date/time string"
  [timestamp]
  (try
    (let [instant (Instant/ofEpochMilli timestamp)
          zoned (.atZone instant (ZoneId/of "Europe/London"))
          formatter (DateTimeFormatter/ofPattern "EEE d MMM yyyy 'at' HH:mm")]
      (.format zoned formatter))
    (catch Exception e
      (str "at " timestamp))))

;; ============================================================================
;; Date/Time Calculation for Recurring Reminders
;; ============================================================================

(defn timestamp-to-zoned
  "Convert timestamp (milliseconds) to ZonedDateTime in UK timezone"
  [timestamp]
  (-> (Instant/ofEpochMilli timestamp)
      (.atZone (ZoneId/of "Europe/London"))))

(defn zoned-to-timestamp
  "Convert ZonedDateTime to timestamp (milliseconds)"
  [zdt]
  (.toEpochMilli (.toInstant zdt)))

(defn weekday-to-dayofweek
  "Convert weekday keyword to DayOfWeek"
  [weekday]
  (case weekday
    :monday DayOfWeek/MONDAY
    :tuesday DayOfWeek/TUESDAY
    :wednesday DayOfWeek/WEDNESDAY
    :thursday DayOfWeek/THURSDAY
    :friday DayOfWeek/FRIDAY
    :saturday DayOfWeek/SATURDAY
    :sunday DayOfWeek/SUNDAY
    nil))

(defn with-time-of-day
  "Set time of day on a ZonedDateTime from parsed time string"
  [zdt time-text]
  (let [time-timestamp (parse-date time-text)]
    (if time-timestamp
      (let [time-zdt (timestamp-to-zoned time-timestamp)]
        (-> zdt
            (.withHour (.getHour time-zdt))
            (.withMinute (.getMinute time-zdt))
            (.withSecond 0)
            (.withNano 0)))
      zdt)))

(defn nth-weekday-of-month
  "Get Nth occurrence of weekday in month (1-based ordinal).
   Returns nil if month doesn't have that many occurrences."
  [zdt n weekday]
  (let [day-of-week (weekday-to-dayofweek weekday)
        first-of-month (.with zdt (TemporalAdjusters/firstDayOfMonth))
        first-weekday (.with first-of-month (TemporalAdjusters/nextOrSame day-of-week))
        target-date (.plusWeeks first-weekday (dec n))]
    ;; Check if we're still in the same month
    (if (= (.getMonthValue first-of-month) (.getMonthValue target-date))
      target-date
      nil)))

(defn last-weekday-of-month
  "Get last occurrence of weekday in month"
  [zdt weekday]
  (let [day-of-week (weekday-to-dayofweek weekday)]
    (.with zdt (TemporalAdjusters/lastInMonth day-of-week))))

(defn last-day-of-month
  "Get last day of month"
  [zdt]
  (.with zdt (TemporalAdjusters/lastDayOfMonth)))

(defn day-of-month-or-last
  "Get the Nth day of the month for the month containing zdt, clamped to the
   last day if the month has fewer days (e.g. Feb 30 → Feb 28/29)."
  [zdt day-of-month]
  (let [last-day (.getDayOfMonth (.with zdt (TemporalAdjusters/lastDayOfMonth)))
        clamped (min day-of-month last-day)]
    (.withDayOfMonth zdt clamped)))

(defn calculate-next-occurrence
  "Calculate next occurrence timestamp from pattern and last occurrence.
   Returns timestamp in milliseconds, or nil if can't calculate."
  [pattern last-occurrence-timestamp]
  (try
    (let [last-zdt (timestamp-to-zoned last-occurrence-timestamp)
          pattern-type (:type pattern)]
      (case pattern-type
        :interval
        (let [seconds (:seconds pattern)]
          (+ last-occurrence-timestamp (* seconds 1000)))

        :weekly
        (let [next-week (.plusWeeks last-zdt 1)]
          (zoned-to-timestamp next-week))

        :interval-weekly
        (let [weeks (:weeks pattern)]
          (zoned-to-timestamp (.plusWeeks last-zdt weeks)))

        :ordinal-weekly
        (let [ordinal (:ordinal pattern)
              weekday (:weekday pattern)
              time-text (:time-text pattern)
              next-month (.plusMonths last-zdt 1)
              first-of-next-month (.with next-month (TemporalAdjusters/firstDayOfMonth))
              target-date (nth-weekday-of-month first-of-next-month ordinal weekday)]
          (when target-date
            (let [with-time (with-time-of-day target-date time-text)]
              (zoned-to-timestamp with-time))))

        :monthly-weekday
        (let [position (:position pattern)
              weekday (:weekday pattern)
              time-text (:time-text pattern)
              next-month (.plusMonths last-zdt 1)
              first-of-next-month (.with next-month (TemporalAdjusters/firstDayOfMonth))
              target-date (if (= position :last)
                            (last-weekday-of-month first-of-next-month weekday)
                            (nth-weekday-of-month first-of-next-month
                                                  (case position
                                                    :first 1
                                                    :second 2
                                                    :third 3
                                                    :fourth 4
                                                    1)
                                                  weekday))]
          (when target-date
            (let [with-time (with-time-of-day target-date time-text)]
              (zoned-to-timestamp with-time))))

        :monthly-lastday
        (let [time-text (:time-text pattern)
              next-month (.plusMonths last-zdt 1)
              first-of-next-month (.with next-month (TemporalAdjusters/firstDayOfMonth))
              target-date (last-day-of-month first-of-next-month)
              with-time (with-time-of-day target-date time-text)]
          (zoned-to-timestamp with-time))

        :monthly-day
        (let [day-of-month (:day-of-month pattern)
              time-text (:time-text pattern)
              next-month (.plusMonths last-zdt 1)
              first-of-next-month (.with next-month (TemporalAdjusters/firstDayOfMonth))
              target-date (day-of-month-or-last first-of-next-month day-of-month)
              with-time (with-time-of-day target-date time-text)]
          (zoned-to-timestamp with-time))

        nil))
    (catch Exception e
      (println "Error calculating next occurrence:" (.getMessage e))
      nil)))

(defn calculate-first-occurrence
  "Calculate first occurrence timestamp from pattern.
   Returns timestamp in milliseconds, or nil if can't calculate."
  [pattern]
  (try
    (let [now (System/currentTimeMillis)
          now-zdt (timestamp-to-zoned now)
          pattern-type (:type pattern)]
      (case pattern-type
        :interval
        (let [seconds (:seconds pattern)]
          (+ now (* seconds 1000)))

        :weekly
        (let [start-date (:start-date pattern)
              time-text (:time-text pattern)
              weekday (:weekday pattern)
              ;; Parse the start date
              start-timestamp (parse-date (str "at " start-date))
              _ (when-not start-timestamp
                  (throw (Exception. "Could not parse start date")))
              start-zdt (timestamp-to-zoned start-timestamp)
              day-of-week (weekday-to-dayofweek weekday)
              ;; Find next occurrence of that weekday from start date
              target-date (if (= (.getDayOfWeek start-zdt) day-of-week)
                            start-zdt
                            (.with start-zdt (TemporalAdjusters/next day-of-week)))
              with-time (with-time-of-day target-date time-text)]
          (zoned-to-timestamp with-time))

        :interval-weekly
        (let [start-date (:start-date pattern)
              time-text (:time-text pattern)
              weekday (:weekday pattern)
              ;; Parse the start date
              start-timestamp (parse-date (str "at " start-date))
              _ (when-not start-timestamp
                  (throw (Exception. "Could not parse start date")))
              start-zdt (timestamp-to-zoned start-timestamp)
              day-of-week (weekday-to-dayofweek weekday)
              ;; Find next occurrence of that weekday from start date
              target-date (if (= (.getDayOfWeek start-zdt) day-of-week)
                            start-zdt
                            (.with start-zdt (TemporalAdjusters/next day-of-week)))
              with-time (with-time-of-day target-date time-text)]
          (zoned-to-timestamp with-time))

        :ordinal-weekly
        (let [start-date (:start-date pattern)
              time-text (:time-text pattern)
              ordinal (:ordinal pattern)
              weekday (:weekday pattern)
              ;; Parse the start date
              start-timestamp (parse-date (str "at " start-date))
              _ (when-not start-timestamp
                  (throw (Exception. "Could not parse start date")))
              start-zdt (timestamp-to-zoned start-timestamp)
              day-of-week (weekday-to-dayofweek weekday)]
          ;; If start date is the right weekday, use it as first occurrence
          ;; This matches the behavior of :weekly pattern
          (if (= (.getDayOfWeek start-zdt) day-of-week)
            (let [with-time (with-time-of-day start-zdt time-text)]
              (zoned-to-timestamp with-time))
            ;; Otherwise, find the next Nth weekday
            (let [first-of-month (.with start-zdt (TemporalAdjusters/firstDayOfMonth))
                  target-date (nth-weekday-of-month first-of-month ordinal weekday)]
              (if (and target-date (>= (zoned-to-timestamp target-date) start-timestamp))
                (let [with-time (with-time-of-day target-date time-text)]
                  (zoned-to-timestamp with-time))
                ;; Try next month
                (let [next-month (.plusMonths first-of-month 1)
                      target-date (nth-weekday-of-month next-month ordinal weekday)]
                  (when target-date
                    (let [with-time (with-time-of-day target-date time-text)]
                      (zoned-to-timestamp with-time))))))))

        :monthly-weekday
        (let [time-text (:time-text pattern)
              position (:position pattern)
              weekday (:weekday pattern)
              now-zdt (timestamp-to-zoned now)
              first-of-month (.with now-zdt (TemporalAdjusters/firstDayOfMonth))
              target-date (if (= position :last)
                            (last-weekday-of-month first-of-month weekday)
                            (nth-weekday-of-month first-of-month
                                                  (case position
                                                    :first 1
                                                    :second 2
                                                    :third 3
                                                    :fourth 4
                                                    1)
                                                  weekday))]
          (if (and target-date (> (zoned-to-timestamp target-date) now))
            (let [with-time (with-time-of-day target-date time-text)]
              (zoned-to-timestamp with-time))
            ;; Try next month
            (let [next-month (.plusMonths first-of-month 1)
                  target-date (if (= position :last)
                                (last-weekday-of-month next-month weekday)
                                (nth-weekday-of-month next-month
                                                      (case position
                                                        :first 1
                                                        :second 2
                                                        :third 3
                                                        :fourth 4
                                                        1)
                                                      weekday))]
              (when target-date
                (let [with-time (with-time-of-day target-date time-text)]
                  (zoned-to-timestamp with-time))))))

        :monthly-lastday
        (let [time-text (:time-text pattern)
              now-zdt (timestamp-to-zoned now)
              first-of-month (.with now-zdt (TemporalAdjusters/firstDayOfMonth))
              target-date (last-day-of-month first-of-month)
              candidate-timestamp (zoned-to-timestamp (with-time-of-day target-date time-text))]
          (if (> candidate-timestamp now)
            candidate-timestamp
            ;; Try next month
            (let [next-month (.plusMonths first-of-month 1)
                  target-date (last-day-of-month next-month)
                  with-time (with-time-of-day target-date time-text)]
              (zoned-to-timestamp with-time))))

        :monthly-day
        (let [day-of-month (:day-of-month pattern)
              time-text (:time-text pattern)
              now-zdt (timestamp-to-zoned now)
              first-of-month (.with now-zdt (TemporalAdjusters/firstDayOfMonth))
              target-date (day-of-month-or-last first-of-month day-of-month)
              candidate-timestamp (zoned-to-timestamp (with-time-of-day target-date time-text))]
          (if (> candidate-timestamp now)
            candidate-timestamp
            (let [next-month (.plusMonths first-of-month 1)
                  target-date (day-of-month-or-last next-month day-of-month)
                  with-time (with-time-of-day target-date time-text)]
              (zoned-to-timestamp with-time))))

        nil))
    (catch Exception e
      (println "Error calculating first occurrence:" (.getMessage e))
      nil)))

;; ============================================================================
;; Reminder Scheduler
;; ============================================================================

(def ^:private scheduler-running (atom false))

(defn fire-reminder!
  "Fire a reminder - send the message and mark as fired in database.
   For recurring reminders, generates the next instance.
   IMPORTANT: Always marks as fired, even if sending fails, to prevent infinite retry loops."
  [reminder]
  (let [{:keys [chat_id username target_user message reminder_id
                recurrence_type recurrence_pattern recurrence_end_time
                parent_reminder_id due_time]} reminder]
    (try
      ;; Mark as fired FIRST to prevent infinite retry loops
      (db/mark-reminder-fired! reminder_id)

      ;; Format and send the reminder message
      (let [setter-name (if (= target_user username)
                          "you"
                          username)
            reminder-text (str target_user ", " setter-name " asked me to remind you to " message)
            ;; Ensure chat_id is a Long object
            chat-id-long (Long/valueOf (long chat_id))
            msg (msg-parse/create-message
                 :chat-id chat-id-long
                 :text reminder-text
                 :sender "goat"
                 :private? false)]
        (msg/send-msg msg)
        (println (str "✓ Fired reminder #" reminder_id " for " target_user ": " message)))

      ;; If recurring, generate next instance
      (when recurrence_type
        (try
          (let [pattern (edn/read-string recurrence_pattern)
                next-time (calculate-next-occurrence pattern due_time)]
            (when (and next-time
                       (or (nil? recurrence_end_time)
                           (< next-time recurrence_end_time)))
              (let [new-reminder-id (db/add-reminder!
                                      {:chat-id chat_id
                                       :username username
                                       :target-user target_user
                                       :message message
                                       :due-time next-time
                                       :recurrence-type recurrence_type
                                       :recurrence-pattern recurrence_pattern
                                       :recurrence-end-time recurrence_end_time
                                       :parent-reminder-id (or parent_reminder_id reminder_id)})]
                (println (str "✓ Generated next instance #" new-reminder-id " for " (format-datetime next-time))))))
          (catch Exception e
            (println (str "⚠ Error generating next recurring instance: " (.getMessage e))))))

      (catch Exception e
        (println (str "⚠ Error sending reminder #" reminder_id ": " (.getMessage e)))
        (println "  (Reminder marked as fired to prevent retry loop)")))))

(defn start-reminder-scheduler!
  "Start the background scheduler that monitors and fires reminders.
   Uses core.async go-loop to periodically check for due reminders."
  []
  (when (compare-and-set! scheduler-running false true)
    (println "Starting reminder scheduler...")
    (go-loop []
      (let [continue?
            (try
              (let [next-reminder (db/get-next-pending-reminder)]
                (if next-reminder
                  (let [due-time (:due_time next-reminder)
                        now (System/currentTimeMillis)
                        delay-ms (- due-time now)]
                    (if (<= delay-ms 0)
                      ;; Fire immediately and continue
                      (do
                        (fire-reminder! next-reminder)
                        true)
                      ;; Wait until due (but cap at 5 seconds to catch new reminders quickly)
                      (let [wait-time (min delay-ms 5000)]
                        (<! (timeout wait-time))
                        true)))
                  ;; No reminders, check again in 5 seconds to catch new reminders quickly
                  (do
                    (<! (timeout 5000))
                    true)))
              (catch Exception e
                (println "Error in reminder scheduler:" (.getMessage e))
                ;; Keep running despite errors
                (<! (timeout 5000))
                true))]
        (when continue?
          (recur))))))

;; Start the scheduler when the namespace loads
(start-reminder-scheduler!)

;; ============================================================================
;; Reminder Listing and Management
;; ============================================================================

(defn day-of-month-suffix
  "Return the English ordinal suffix for a day-of-month (1-31)."
  [n]
  (cond
    (and (>= n 11) (<= n 13)) "th"
    (= 1 (mod n 10)) "st"
    (= 2 (mod n 10)) "nd"
    (= 3 (mod n 10)) "rd"
    :else "th"))

(defn format-recurrence-description
  "Format a recurrence pattern into a human-readable description"
  [recurrence-type recurrence-pattern]
  (when (and recurrence-type recurrence-pattern)
    (try
      (let [pattern (edn/read-string recurrence-pattern)
            type (:type pattern)]
        (case type
          :interval
          (str "every " (:seconds pattern) " seconds")

          :weekly
          (str "every " (name (:weekday pattern)))

          :interval-weekly
          (let [weeks (:weeks pattern)
                week-str (cond
                           (= weeks 2) "fortnight"
                           :else (str weeks " weeks"))]
            (if (= weeks 2)
              (str "every " week-str " on " (name (:weekday pattern)))
              (str "every " week-str " on " (name (:weekday pattern)))))

          :ordinal-weekly
          (let [ordinals {2 "second" 3 "third" 4 "fourth" 5 "fifth"
                          6 "sixth" 7 "seventh" 8 "eighth" 9 "ninth" 10 "tenth"}
                ordinal-str (get ordinals (:ordinal pattern) (str (:ordinal pattern)))]
            (str "every " ordinal-str " " (name (:weekday pattern))))

          :monthly-weekday
          (let [position (:position pattern)]
            (str "each " (name position) " " (name (:weekday pattern)) " of the month"))

          :monthly-lastday
          "each last day of the month"

          :monthly-day
          (let [d (:day-of-month pattern)]
            (str "each " d (day-of-month-suffix d) " of the month"))

          "recurring"))
      (catch Exception e
        "recurring"))))

(defn format-reminder-for-list
  "Format a single reminder for display in a list.
   Returns a map with :number, :description"
  [index reminder]
  (let [{:keys [due_time message recurrence_type recurrence_pattern]} reminder
        time-str (format-datetime due_time)
        recurrence-str (when recurrence_type
                         (str " (" (format-recurrence-description recurrence_type recurrence_pattern) ")"))]
    {:number (inc index)
     :id (:reminder_id reminder)
     :description (str time-str ": " message (or recurrence-str ""))}))

(defn list-reminders
  "Get formatted list of all reminders"
  []
  (let [reminders (db/get-all-listable-reminders)]
    (if (empty? reminders)
      "📋 There are no pending reminders.\n\nUse 'remind' to create one!"
      (str "📋 <b>All Reminders:</b>\n\n"
           (str/join "\n"
                     (map-indexed
                       (fn [idx r]
                         (let [formatted (format-reminder-for-list idx r)]
                           (str (:number formatted) ". " (:description formatted))))
                       reminders))
           "\n\n"
           "<b>reminders remove &lt;number&gt;</b> to delete; "
           "<b>reminders edit &lt;number&gt; &lt;new text&gt;</b> to change the text."))))

(defn remove-reminder
  "Remove a reminder by its list number (1-based). For recurring reminders,
   cancels the entire lineage even when the listed entry is an instance."
  [number-str]
  (try
    (let [number (Integer/parseInt number-str)
          reminders (db/get-all-listable-reminders)]
      (if (or (< number 1) (> number (count reminders)))
        (str "❌ Invalid reminder number. There are " (count reminders) " reminder(s).\n\n"
             "Use <b>reminders</b> to see the list.")
        (let [reminder (nth reminders (dec number))
              reminder-id (:reminder_id reminder)
              is-recurring (:recurrence_type reminder)]
          (if is-recurring
            (do
              (db/cancel-recurring-and-instances! reminder-id)
              (str "✅ Cancelled recurring reminder: " (:message reminder)))
            (do
              (db/cancel-reminder! reminder-id)
              (str "✅ Cancelled reminder: " (:message reminder)))))))
    (catch NumberFormatException e
      "❌ Invalid number. Use <b>reminders remove &lt;number&gt;</b>")
    (catch Exception e
      (str "❌ Error removing reminder: " (.getMessage e)))))

(defn edit-reminder
  "Edit the message text of a reminder by its list number (1-based).
   For recurring reminders, updates both the root parent and the next
   pending instance so future fires use the new text."
  [number-str new-message]
  (try
    (let [number (Integer/parseInt number-str)
          reminders (db/get-all-listable-reminders)]
      (cond
        (or (< number 1) (> number (count reminders)))
        (str "❌ Invalid reminder number. There are " (count reminders) " reminder(s).\n\n"
             "Use <b>reminders</b> to see the list.")

        (str/blank? new-message)
        "❌ New text is empty. Use <b>reminders edit &lt;number&gt; &lt;new text&gt;</b>"

        :else
        (let [reminder (nth reminders (dec number))
              reminder-id (:reminder_id reminder)]
          (db/update-reminder-message! reminder-id (str/trim new-message))
          (str "✏️ Updated reminder: " (str/trim new-message)))))
    (catch NumberFormatException e
      "❌ Invalid number. Use <b>reminders edit &lt;number&gt; &lt;new text&gt;</b>")
    (catch Exception e
      (str "❌ Error editing reminder: " (.getMessage e)))))

;; ============================================================================
;; Module Definition
;; ============================================================================

(defn show-help
  "Display help message for the remind command"
  []
  (str "⏰ <b>Remind Command</b>\n\n"
       "<b>One-time Reminders:</b>\n"
       "remind [person] at/in [time] to [task]\n\n"
       "<b>Examples:</b>\n"
       "• remind me in 1 minute to check the oven\n"
       "• remind me at 3pm to buy milk\n"
       "• remind me tomorrow at 9am to take out the bins\n"
       "• remind Janet in 2 hours to check email\n"
       "• remind Barry next Tuesday to phone mum\n"
       "• remind me at 5/3/2026 3pm to renew passport\n\n"
       "<b>Recurring Reminders:</b>\n\n"
       "<b>Weekly (same weekday every week):</b>\n"
       "• remind me every tuesday at 9am starting 9/12/25 to put out bins\n"
       "• remind janet every sunday at 6pm starting 14/12/25 to put out the milk bottles\n\n"
       "<b>Fortnightly / every N weeks (uses ordinal):</b>\n"
       "• remind me every second friday at 5pm starting 9/1/26 to fortnightly review (every 2 weeks)\n"
       "• remind me every third thursday at 6pm starting 15/1/26 to put out the green bin (every 3 weeks)\n"
       "• remind me every fourth monday at 9am starting 5/1/26 to do team retro (every 4 weeks)\n\n"
       "<b>Monthly — Nth weekday of the month:</b>\n"
       "• remind me each first monday of the month at 7pm to review budget\n"
       "• remind me each second tuesday of the month at 12pm to pay credit card\n"
       "• remind me each last thursday of the month at 9am to submit expenses\n\n"
       "<b>Monthly — specific day of the month (1st-31st):</b>\n"
       "• remind me each 1st of the month at 9am to pay rent\n"
       "• remind me each 15th of the month at 10am to check savings\n"
       "• remind me each 28th of the month at 6pm to run backups\n"
       "  (days 29-31 fall back to the last day in shorter months)\n\n"
       "<b>Monthly — last day of the month:</b>\n"
       "• remind me each last day of the month at 3am to run backup\n\n"
       "<b>Managing reminders:</b>\n"
       "• <b>reminders</b> — list all pending reminders (one-shot and recurring)\n"
       "• <b>reminders remove &lt;number&gt;</b> — cancel a reminder; for recurring, the whole series stops\n"
       "• <b>reminders edit &lt;number&gt; &lt;new text&gt;</b> — change the reminder text\n\n"
       "<b>Time Formats:</b>\n"
       "• in 2 hours, in 30 minutes, in 1 minute\n"
       "• at 3pm, at 9:30am, at 15:00\n"
       "• tomorrow, today, next Tuesday, next week\n"
       "• tomorrow at 3pm\n"
       "• at DD/MM/YYYY or DD/MM/YY [time] (e.g., at 10/4/27 1pm)"))

(defmodule Remind
  :commands [:remind :reminders]
  :receive-messages :commands
  :wants-private true

  (defn process-message [m]
    (let [command (msg/command m)
          text (msg/mod-text m)]
      (cond
        ;; Handle "reminders" command
        (= command :reminders)
        (let [trimmed (str/trim (or text ""))]
          (cond
            (str/blank? trimmed)
            (msg/reply m (list-reminders))

            (re-find #"^remove\s+\d+$" trimmed)
            (let [[_ number] (re-find #"^remove\s+(\d+)$" trimmed)]
              (msg/reply m (remove-reminder number)))

            (re-find #"^edit\s+\d+\s+\S" trimmed)
            (let [[_ number new-text] (re-find #"^edit\s+(\d+)\s+(.+)$" trimmed)]
              (msg/reply m (edit-reminder number new-text)))

            :else
            (msg/reply m (str "❌ Invalid command. Use:\n"
                              "• <b>reminders</b> — list reminders\n"
                              "• <b>reminders remove &lt;number&gt;</b> — delete a reminder\n"
                              "• <b>reminders edit &lt;number&gt; &lt;new text&gt;</b> — change the reminder text"))))

        ;; Handle "remind" command
        (= command :remind)
        (let [recurring-parsed (parse-recurring-command text)
              one-shot-parsed (parse-remind-command text)]
          (cond
            ;; No text or invalid format - show usage
            (or (not text) (and (not recurring-parsed) (not one-shot-parsed)))
            (msg/reply m (show-help))

            ;; Recurring command
            recurring-parsed
            (let [{:keys [target recurrence task]} recurring-parsed
                  pattern recurrence
                  timestamp (calculate-first-occurrence pattern)]
              (cond
                ;; Couldn't calculate first occurrence
                (nil? timestamp)
                (msg/reply m (str "❌ Sorry, I couldn't calculate when that reminder should fire.\n\n"
                                 "Please check your date/time format."))

                ;; Date is in the past
                (< timestamp (System/currentTimeMillis))
                (msg/reply m "⚠️ Great Scott! That time is in the past! Please specify a future time.")

                ;; Success - create recurring reminder
                :else
                (let [target-user (or target (msg/sender m))
                      pattern-str (pr-str pattern)
                      recurrence-type (name (:type pattern))
                      reminder-id (db/add-reminder!
                                    {:chat-id (msg/chat-id m)
                                     :username (msg/sender m)
                                     :target-user target-user
                                     :message task
                                     :due-time timestamp
                                     :recurrence-type recurrence-type
                                     :recurrence-pattern pattern-str})]
                  (msg/reply m (format "✅ I'll remind %s to %s on %s (recurring)"
                                     target-user
                                     task
                                     (format-datetime timestamp))))))

            ;; One-shot command
            one-shot-parsed
            (let [timestamp (parse-date (:time-text one-shot-parsed))]
              (cond
                ;; Couldn't parse date
                (nil? timestamp)
                (msg/reply m (str "❌ Sorry, I couldn't understand that date/time.\n\n"
                                 "Try formats like:\n"
                                 "• in 2 hours\n"
                                 "• at 3pm\n"
                                 "• tomorrow at 9am"))

                ;; Date is in the past
                (< timestamp (System/currentTimeMillis))
                (msg/reply m "⚠️ Great Scott! That time is in the past! Please specify a future time.")

                ;; Success - create reminder
                :else
                (let [task (:task one-shot-parsed)
                      target (or (:target one-shot-parsed) (msg/sender m))
                      reminder-id (db/add-reminder!
                                    {:chat-id (msg/chat-id m)
                                     :username (msg/sender m)
                                     :target-user target
                                     :message task
                                     :due-time timestamp})]
                  (msg/reply m (format "✅ I'll remind %s to %s on %s"
                                     target
                                     task
                                     (format-datetime timestamp))))))))))))
