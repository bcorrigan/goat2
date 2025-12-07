(ns org.goat.module.Remind
  "Reminder module - allows users to set time-based reminders with natural language"
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.reminders :as db]
            [clojure.core.async :refer [go-loop timeout <!]]
            [clojure.string :as str])
  (:import [com.zoho.hawking HawkingTimeParser]
           [com.zoho.hawking.datetimeparser.configuration HawkingConfiguration]
           [org.goat.core Message]
           [java.time Instant ZonedDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

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
;; Reminder Scheduler
;; ============================================================================

(def ^:private scheduler-running (atom false))

(defn fire-reminder!
  "Fire a reminder - send the message and mark as fired in database.
   IMPORTANT: Always marks as fired, even if sending fails, to prevent infinite retry loops."
  [reminder]
  (let [{:keys [chat_id username target_user message reminder_id]} reminder]
    (try
      ;; Mark as fired FIRST to prevent infinite retry loops
      (db/mark-reminder-fired! reminder_id)

      ;; Format and send the reminder message
      (let [display-name (if (= target_user username)
                           "you"
                           target_user)
            reminder-text (str username ", " display-name " asked me to remind you to " message)
            ;; Ensure chat_id is a Long object
            chat-id-long (Long/valueOf (long chat_id))
            msg (Message. chat-id-long reminder-text false target_user)]
        (.send msg)
        (println (str "✓ Fired reminder #" reminder_id " for " target_user ": " message)))
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
                      ;; Wait until due (but check every minute minimum to catch new earlier reminders)
                      (let [wait-time (min delay-ms 60000)]
                        (<! (timeout wait-time))
                        true)))
                  ;; No reminders, check again in 60 seconds
                  (do
                    (<! (timeout 60000))
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
;; Module Definition
;; ============================================================================

(defn show-help
  "Display help message for the remind command"
  []
  (str "⏰ <b>Remind Command</b>\n\n"
       "<b>Usage:</b>\n"
       "remind [person] at/in [time] to [task]\n\n"
       "<b>Examples:</b>\n"
       "• remind me in 1 minute to check the oven\n"
       "• remind me at 3pm to buy milk\n"
       "• remind me tomorrow at 9am to take out the bins\n"
       "• remind Janet in 2 hours to check email\n"
       "• remind Barry next Tuesday to phone mum\n"
       "• remind me at 5/3/2026 3pm to renew passport\n\n"
       "<b>Time Formats:</b>\n"
       "• in 2 hours, in 30 minutes, in 1 minute\n"
       "• at 3pm, at 9:30am, at 15:00\n"
       "• tomorrow, today, next Tuesday, next week\n"
       "• tomorrow at 3pm\n"
       "• at DD/MM/YYYY or DD/MM/YY [time] (e.g., at 10/4/27 1pm)"))

(defmodule Remind
  :commands [:remind]
  :receive-messages :commands
  :wants-private true

  (defn process-channel-message [m]
    (let [text (msg/mod-text m)
          parsed (parse-remind-command text)]
      (cond
        ;; No text or invalid format - show usage
        (or (not text) (not parsed))
        (msg/reply m (show-help))

        ;; Valid command - try to parse date
        :else
        (let [timestamp (parse-date (:time-text parsed))]
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
            (let [task (:task parsed)
                  target (or (:target parsed) (msg/sender m))
                  reminder-id (db/add-reminder!
                                {:chat-id (msg/chat-id m)
                                 :username (msg/sender m)
                                 :target-user target
                                 :message task
                                 :due-time timestamp})]
              (msg/reply m (format "✅ I'll remind %s to %s on %s"
                                 target
                                 task
                                 (format-datetime timestamp))))))))))
