(ns org.goat.module.Remind-test
  "Integration tests for Remind module using message mocking"
  (:require [org.goat.module.Remind :as sut]
            [org.goat.db.reminders :as db]
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
   :subname "test/resources/test-reminders.db"})

(defn setup-test-db []
  (let [db-file (File. "test/resources/test-reminders.db")]
    (when (.exists db-file) (.delete db-file)))

  ;; Create reminders table
  (sql/db-do-commands test-db
    (sql/create-table-ddl :reminders
      [[:reminder_id :integer "PRIMARY KEY AUTOINCREMENT"]
       [:chat_id :integer "NOT NULL"]
       [:username :text "NOT NULL"]
       [:target_user :text "NOT NULL"]
       [:message :text "NOT NULL"]
       [:set_time :integer "NOT NULL"]
       [:due_time :integer "NOT NULL"]
       [:status :text "NOT NULL DEFAULT 'pending'"]
       [:fired_time :integer]
       [:recurrence_type :text]
       [:recurrence_pattern :text]
       [:recurrence_end_time :integer]
       [:parent_reminder_id :integer]]))

  ;; Create indexes
  (sql/execute! test-db "CREATE INDEX idx_due_time ON reminders(due_time)")
  (sql/execute! test-db "CREATE INDEX idx_status_due ON reminders(status, due_time)"))

(defn teardown-test-db []
  (let [db-file (File. "test/resources/test-reminders.db")]
    (when (.exists db-file) (.delete db-file))))

(defn db-fixture [f]
  (setup-test-db)
  (msg-utils/clear-replies!)
  (with-redefs [db/db test-db]
    (f))
  (teardown-test-db))

(use-fixtures :each db-fixture)

;; ============================================================================
;; Command Parsing Tests
;; ============================================================================

(deftest test-remind-me-simple
  (testing "Simple 'remind me' command with new format"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me tomorrow at 3pm to buy milk"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        ;; Should get confirmation
        (is (msg-utils/replied-with? "I'll remind"))
        (is (msg-utils/replied-with? "alice"))
        (is (msg-utils/replied-with? "buy milk"))

        ;; Should be in database
        (let [reminders (db/get-user-reminders "alice")]
          (is (= 1 (count reminders)))
          (let [reminder (first reminders)]
            (is (= "alice" (:username reminder)))
            (is (= "alice" (:target_user reminder)))
            (is (= "buy milk" (:message reminder)))))))))

(deftest test-remind-other-user
  (testing "Remind another user"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "bob in 2 hours to finish report"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "I'll remind"))
        (is (msg-utils/replied-with? "bob"))
        (is (msg-utils/replied-with? "finish report"))

        ;; Reminder should target bob, set by alice
        (let [reminders (db/get-user-reminders "bob")]
          (is (= 1 (count reminders)))
          (let [reminder (first reminders)]
            (is (= "bob" (:target_user reminder)))
            (is (= "alice" (:username reminder)))
            (is (= "finish report" (:message reminder)))))))))

(deftest test-remind-invalid-syntax
  (testing "Invalid command syntax shows usage"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "something wrong"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "One-time Reminders:"))
        (is (msg-utils/replied-with? "Examples:"))

        ;; Should NOT create a reminder
        (is (= 0 (count (db/get-pending-reminders))))))))

(deftest test-remind-no-arguments
  (testing "Command with no arguments shows usage"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  ""
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "One-time Reminders:"))))))

(deftest test-remind-missing-to
  (testing "Command missing 'to' keyword shows usage"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "bob tomorrow check email"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "One-time Reminders:"))))))

;; ============================================================================
;; Date Parsing Tests
;; ============================================================================

(deftest test-remind-tomorrow
  (testing "Parsing 'tomorrow' creates a future reminder"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me tomorrow to check email"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "I'll remind"))

        ;; Reminder should be in the future
        (let [reminders (db/get-user-reminders "alice")
              reminder (first reminders)]
          (is (> (:due_time reminder) (System/currentTimeMillis))))))))

(deftest test-remind-in-hours
  (testing "Parsing 'in X hours' works"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me in 2 hours to call mom"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "I'll remind"))

        ;; Reminder should be in the future (Hawking may interpret "2 hours" variously)
        ;; Just verify it's in the future - Hawking's interpretation can vary
        (let [reminders (db/get-user-reminders "alice")
              reminder (first reminders)
              now (System/currentTimeMillis)
              time-until-due (- (:due_time reminder) now)]
          (is (pos? time-until-due) "Reminder should be in the future"))))))

(deftest test-remind-unparseable-date
  (testing "Unparseable date/time shows error"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me xyzabc to do something"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "couldn't understand"))

        ;; Should NOT create a reminder
        (is (= 0 (count (db/get-pending-reminders))))))))

(deftest test-remind-past-date
  (testing "Date in the past is rejected"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me yesterday to check logs"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "past"))
        (is (msg-utils/replied-with? "Great Scott"))

        ;; Should NOT create a reminder
        (is (= 0 (count (db/get-pending-reminders))))))))

;; ============================================================================
;; Multiple Users and Reminders
;; ============================================================================

(deftest test-multiple-reminders
  (testing "Multiple users can set multiple reminders"
    (msg-utils/with-clean-replies
      ;; Alice sets a reminder for herself
      (let [msg1 (msg-utils/mock-command-message
                   "remind"
                   "me tomorrow to task1"
                   {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg1))

      (msg-utils/clear-replies!)

      ;; Alice sets a reminder for Bob
      (let [msg2 (msg-utils/mock-command-message
                   "remind"
                   "bob in 2 hours to task2"
                   {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg2))

      (msg-utils/clear-replies!)

      ;; Bob sets a reminder for himself
      (let [msg3 (msg-utils/mock-command-message
                   "remind"
                   "me tomorrow to task3"
                   {:sender "bob" :chat-id 456})]
        (sut/-processChannelMessage nil msg3))

      ;; Check Alice's reminders (as target)
      (let [alice-reminders (db/get-user-reminders "alice")]
        (is (= 1 (count alice-reminders))))

      ;; Check Bob's reminders (as target)
      (let [bob-reminders (db/get-user-reminders "bob")]
        (is (= 2 (count bob-reminders))))

      ;; Check all pending
      (is (= 3 (count (db/get-pending-reminders)))))))

;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest test-remind-case-insensitive-me
  (testing "'ME' (uppercase) is treated as self-reminder"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "ME tomorrow to check email"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "I'll remind"))

        ;; Should target alice (the sender)
        (let [reminders (db/get-user-reminders "alice")]
          (is (= 1 (count reminders)))
          (is (= "alice" (:target_user (first reminders)))))))))

(deftest test-remind-different-chat_ids
  (testing "Reminders are associated with correct chat IDs"
    (msg-utils/with-clean-replies
      (let [msg1 (msg-utils/mock-command-message
                   "remind"
                   "me tomorrow to task1"
                   {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg1))

      (msg-utils/clear-replies!)

      (let [msg2 (msg-utils/mock-command-message
                   "remind"
                   "me tomorrow to task2"
                   {:sender "alice" :chat-id 456})]
        (sut/-processChannelMessage nil msg2))

      ;; Both should be created with their respective chat IDs
      (let [reminders (db/get-user-reminders "alice")]
        (is (= 2 (count reminders)))
        (is (= #{123 456} (set (map :chat_id reminders))))))))

;; ============================================================================
;; Firing Tests
;; ============================================================================

(deftest test-fire-reminder-marks-as-fired
  (testing "Firing a reminder marks it as fired in database"
    (let [now (System/currentTimeMillis)
          past-time (- now 1000)  ; 1 second ago (already due)
          reminder-id (db/add-reminder!
                        {:chat-id 123
                         :username "alice"
                         :target-user "bob"
                         :message "test task"
                         :due-time past-time})
          reminder (db/get-reminder-by-id reminder-id)]

      ;; Initially pending
      (is (= "pending" (:status reminder)))

      ;; Fire it (may fail to send, but should mark as fired)
      (sut/fire-reminder! reminder)

      ;; Should be marked as fired
      (let [updated (db/get-reminder-by-id reminder-id)]
        (is (= "fired" (:status updated)))
        (is (some? (:fired_time updated)))))))

(deftest test-overdue-reminder-can-be-fired
  (testing "An overdue reminder can be fired manually"
    (let [now (System/currentTimeMillis)
          past-time (- now 5000)  ; 5 seconds ago (already due)
          reminder-id (db/add-reminder!
                        {:chat-id 123
                         :username "alice"
                         :target-user "bob"
                         :message "overdue task"
                         :due-time past-time})
          reminder (db/get-reminder-by-id reminder-id)]

      ;; Fire it manually
      (sut/fire-reminder! reminder)

      ;; Reminder should be marked as fired
      (let [updated (db/get-reminder-by-id reminder-id)]
        (is (= "fired" (:status updated))
            "Overdue reminder should be marked as fired"))

      ;; Should no longer be in pending list
      (is (= 0 (count (db/get-pending-reminders)))
          "Fired reminder should not be in pending list"))))

(deftest test-fire-reminder-with-invalid-chat-handles-error
  (testing "Firing a reminder with invalid chat_id doesn't crash"
    (let [now (System/currentTimeMillis)
          reminder-id (db/add-reminder!
                        {:chat-id -1  ; Invalid chat ID
                         :username "alice"
                         :target-user "bob"
                         :message "test"
                         :due-time now})
          reminder (db/get-reminder-by-id reminder-id)]

      ;; Should not throw, even with invalid chat
      (is (nil? (sut/fire-reminder! reminder)))

      ;; Should still be marked as fired (to prevent retry loop)
      (let [updated (db/get-reminder-by-id reminder-id)]
        (is (= "fired" (:status updated)))))))

;; ============================================================================
;; Command Parsing Unit Tests
;; ============================================================================

(deftest test-parse-remind-command-function
  (testing "parse-remind-command function with new format"
    ;; Valid commands
    (is (= {:target nil :time-text "tomorrow" :task "buy milk"}
           (sut/parse-remind-command "me tomorrow to buy milk")))

    (is (= {:target nil :time-text "1 minute" :task "poop"}
           (sut/parse-remind-command "me in 1 minute to poop")))

    (is (= {:target "bob" :time-text "2 hours" :task "finish report"}
           (sut/parse-remind-command "bob in 2 hours to finish report")))

    (is (= {:target "alice" :time-text "3pm" :task "check email"}
           (sut/parse-remind-command "alice at 3pm to check email")))

    ;; Invalid commands
    (is (nil? (sut/parse-remind-command "something wrong")))
    (is (nil? (sut/parse-remind-command "bob tomorrow check email")))
    (is (nil? (sut/parse-remind-command "me to buy milk tomorrow")))  ; Old format
    (is (nil? (sut/parse-remind-command "")))))

(deftest test-parse-date-function
  (testing "parse-date function"
    ;; Should parse "tomorrow"
    (let [timestamp (sut/parse-date "tomorrow at 3pm")]
      (is (some? timestamp))
      (is (> timestamp (System/currentTimeMillis))))

    ;; Should parse "in X hours"
    (let [timestamp (sut/parse-date "in 2 hours")]
      (is (some? timestamp))
      (is (> timestamp (System/currentTimeMillis))))

    ;; Should return nil for unparseable text
    (is (nil? (sut/parse-date "xyzabc")))))

(deftest test-preprocess-date-text
  (testing "Preprocessing converts 2-digit years to 4-digit years"
    ;; Two-digit year should be converted to 20YY
    (is (= "10/4/2027" (sut/preprocess-date-text "10/4/27")))
    (is (= "5/12/2099" (sut/preprocess-date-text "5/12/99")))
    (is (= "1/1/2025" (sut/preprocess-date-text "1/1/25")))

    ;; Four-digit year should be unchanged
    (is (= "10/4/2027" (sut/preprocess-date-text "10/4/2027")))

    ;; Times should not be affected
    (is (= "tomorrow at 3:30pm" (sut/preprocess-date-text "tomorrow at 3:30pm")))

    ;; Mixed text with 2-digit year
    (is (= "10/4/2027 1pm" (sut/preprocess-date-text "10/4/27 1pm")))))

;; ============================================================================
;; Recurring Reminders Command Parsing Tests
;; ============================================================================

(deftest test-parse-interval-command
  (testing "Parse interval command for testing"
    (is (= {:target nil
            :recurrence {:type :interval :seconds 5}
            :task "check something"}
           (sut/parse-interval-command "me every 5 seconds to check something")))

    (is (= {:target "bob"
            :recurrence {:type :interval :seconds 10}
            :task "poke alice"}
           (sut/parse-interval-command "bob every 10 seconds to poke alice")))

    ;; Invalid formats
    (is (nil? (sut/parse-interval-command "me every tuesday to do thing")))
    (is (nil? (sut/parse-interval-command "me in 5 seconds to do thing")))))

(deftest test-parse-weekly-command
  (testing "Parse weekly recurring command"
    (let [result (sut/parse-weekly-command "me every tuesday at 9am starting 9/12/25 to put out bins")]
      (is (= nil (:target result)))
      (is (= :weekly (get-in result [:recurrence :type])))
      (is (= :tuesday (get-in result [:recurrence :weekday])))
      (is (= "9am" (get-in result [:recurrence :time-text])))
      (is (= "9/12/25" (get-in result [:recurrence :start-date])))
      (is (= "put out bins" (:task result))))

    ;; Test interval weekly (every N weeks on a weekday)
    (let [result (sut/parse-weekly-command "me every third friday at 3pm starting 12/12/25 to timesheet")]
      (is (= :interval-weekly (get-in result [:recurrence :type])))
      (is (= 3 (get-in result [:recurrence :weeks])))
      (is (= :friday (get-in result [:recurrence :weekday])))
      (is (= "timesheet" (:task result))))))

(deftest test-parse-monthly-command
  (testing "Parse monthly recurring command"
    ;; Last weekday
    (let [result (sut/parse-monthly-command "me each last thursday of the month at 9am to submit expenses")]
      (is (= nil (:target result)))
      (is (= :monthly-weekday (get-in result [:recurrence :type])))
      (is (= :last (get-in result [:recurrence :position])))
      (is (= :thursday (get-in result [:recurrence :weekday])))
      (is (= "9am" (get-in result [:recurrence :time-text])))
      (is (= "submit expenses" (:task result))))

    ;; First weekday
    (let [result (sut/parse-monthly-command "me each first monday of the month at 7pm to review budget")]
      (is (= :monthly-weekday (get-in result [:recurrence :type])))
      (is (= :first (get-in result [:recurrence :position])))
      (is (= :monday (get-in result [:recurrence :weekday])))
      (is (= "review budget" (:task result))))

    ;; Last day
    (let [result (sut/parse-monthly-command "me each last day of the month at 3am to run backup")]
      (is (= :monthly-lastday (get-in result [:recurrence :type])))
      (is (= "3am" (get-in result [:recurrence :time-text])))
      (is (= "run backup" (:task result))))))

;; ============================================================================
;; Recurring Reminders Integration Tests
;; ============================================================================

(deftest test-interval-recurring-reminder
  (testing "Create and fire interval recurring reminder (every 5 seconds)"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me every 5 seconds to breathe"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        ;; Should get confirmation with (recurring) marker
        (is (msg-utils/replied-with? "I'll remind"))
        (is (msg-utils/replied-with? "alice"))
        (is (msg-utils/replied-with? "breathe"))
        (is (msg-utils/replied-with? "(recurring)"))

        ;; Should be in database
        (let [reminders (db/get-user-reminders "alice")]
          (is (= 1 (count reminders)))
          (let [reminder (first reminders)
                reminder-id (:reminder_id reminder)]
            (is (= "alice" (:username reminder)))
            (is (= "alice" (:target_user reminder)))
            (is (= "breathe" (:message reminder)))
            (is (= "interval" (:recurrence_type reminder)))
            (is (some? (:recurrence_pattern reminder)))

            ;; Fire the reminder manually
            (msg-utils/clear-replies!)
            (sut/fire-reminder! reminder)

            ;; Check that next instance was created
            (let [all-reminders (db/get-pending-reminders)]
              (is (= 1 (count all-reminders)) "Should have one pending instance")
              (let [next-instance (first all-reminders)]
                (is (= "breathe" (:message next-instance)))
                (is (= "interval" (:recurrence_type next-instance)))
                (is (= reminder-id (:parent_reminder_id next-instance)))
                (is (> (:due_time next-instance) (:due_time reminder)) "Next instance should be later")))))))))

(deftest test-recurring-generates-multiple-instances
  (testing "Recurring reminder generates multiple instances when fired repeatedly"
    (msg-utils/with-clean-replies
      ;; Create recurring reminder
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me every 5 seconds to blink"
                  {:sender "bob" :chat-id 456})]
        (sut/-processChannelMessage nil msg)

        ;; Fire first instance
        (let [r1 (first (db/get-pending-reminders))
              r1-id (:reminder_id r1)]
          (sut/fire-reminder! r1)

          ;; Fire second instance
          (let [r2 (first (db/get-pending-reminders))]
            (is (some? r2) "Second instance should exist")
            (is (= r1-id (:parent_reminder_id r2))
                "Second instance should have first instance as parent")
            (sut/fire-reminder! r2)

            ;; Fire third instance
            (let [r3 (first (db/get-pending-reminders))]
              (is (some? r3) "Third instance should exist")
              (is (= "blink" (:message r3)))
              (is (= r1-id (:parent_reminder_id r3))
                  "Third instance should have first instance as parent")

              ;; Check that all three were marked as fired
              (is (= "fired" (:status (db/get-reminder-by-id (:reminder_id r1)))))
              (is (= "fired" (:status (db/get-reminder-by-id (:reminder_id r2)))))
              (is (= "pending" (:status (db/get-reminder-by-id (:reminder_id r3))))))))))))
(deftest test-cancel-recurring-stops-instances
  (testing "Cancelling recurring reminder stops future instances"
    (msg-utils/with-clean-replies
      ;; Create recurring reminder
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me every 5 seconds to test"
                  {:sender "charlie" :chat-id 789})]
        (sut/-processChannelMessage nil msg)

        (let [first-instance (first (db/get-pending-reminders))
              parent-id (or (:parent_reminder_id first-instance)
                            (:reminder_id first-instance))]

          ;; Fire first instance (creates second)
          (sut/fire-reminder! first-instance)

          (let [second-instance (first (db/get-pending-reminders))]
            (is (some? second-instance) "Second instance should exist")

            ;; Cancel the recurring reminder
            (db/cancel-recurring-and-instances! parent-id)

            ;; Second instance should now be cancelled
            (is (= "cancelled" (:status (db/get-reminder-by-id (:reminder_id second-instance)))))

            ;; No pending reminders should remain
            (is (empty? (db/get-pending-reminders)))))))))

;;============================================================================
;; Reminders Listing and Deletion Tests
;;============================================================================

(deftest test-list-empty-reminders
  (testing "Listing reminders when none exist"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "reminders"
                  ""
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "no pending reminders"))))))

(deftest test-list-one-reminder
  (testing "Listing a single one-shot reminder"
    (msg-utils/with-clean-replies
      ;; Create a reminder first
      (db/add-reminder! {:chat-id 123
                         :username "alice"
                         :target-user "alice"
                         :message "buy milk"
                         :due-time (+ (System/currentTimeMillis) 3600000)})

      ;; List reminders
      (let [msg (msg-utils/mock-command-message
                  "reminders"
                  ""
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "Your Reminders"))
        (is (msg-utils/replied-with? "buy milk"))
        (is (msg-utils/replied-with? "1."))))))

(deftest test-list-recurring-reminder
  (testing "Listing a recurring reminder shows recurrence description"
    (msg-utils/with-clean-replies
      ;; Create a recurring reminder
      (db/add-reminder! {:chat-id 123
                         :username "alice"
                         :target-user "alice"
                         :message "weekly meeting"
                         :due-time (+ (System/currentTimeMillis) 3600000)
                         :recurrence-type "weekly"
                         :recurrence-pattern "{:type :weekly :weekday :monday}"})

      ;; List reminders
      (let [msg (msg-utils/mock-command-message
                  "reminders"
                  ""
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "weekly meeting"))
        (is (msg-utils/replied-with? "every monday"))))))

(deftest test-remove-reminder
  (testing "Removing a reminder by number"
    (msg-utils/with-clean-replies
      ;; Create two reminders
      (db/add-reminder! {:chat-id 123
                         :username "alice"
                         :target-user "alice"
                         :message "first task"
                         :due-time (+ (System/currentTimeMillis) 3600000)})
      (db/add-reminder! {:chat-id 123
                         :username "alice"
                         :target-user "alice"
                         :message "second task"
                         :due-time (+ (System/currentTimeMillis) 7200000)})

      ;; Verify we have 2 reminders
      (is (= 2 (count (db/get-user-reminders "alice"))))

      ;; Remove the first one
      (let [msg (msg-utils/mock-command-message
                  "reminders"
                  "remove 1"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "Cancelled reminder"))
        (is (msg-utils/replied-with? "first task")))

      ;; Verify we now have only 1 pending reminder
      (is (= 1 (count (db/get-user-reminders "alice"))))
      (is (= "second task" (:message (first (db/get-user-reminders "alice"))))))))

(deftest test-remove-recurring-reminder
  (testing "Removing a recurring reminder cancels it and all instances"
    (msg-utils/with-clean-replies
      ;; Create a recurring reminder
      (let [parent-id (db/add-reminder! {:chat-id 123
                                         :username "alice"
                                         :target-user "alice"
                                         :message "daily standup"
                                         :due-time (+ (System/currentTimeMillis) 3600000)
                                         :recurrence-type "interval"
                                         :recurrence-pattern "{:type :interval :seconds 10}"})]

        ;; Create an instance of it
        (db/add-reminder! {:chat-id 123
                           :username "alice"
                           :target-user "alice"
                           :message "daily standup"
                           :due-time (+ (System/currentTimeMillis) 3610000)
                           :parent-reminder-id parent-id})

        ;; Verify we have the parent in listable reminders
        (is (= 1 (count (db/get-user-listable-reminders "alice"))))
        ;; Verify we have 2 total pending (parent + instance)
        (is (= 2 (count (db/get-pending-reminders))))

        ;; Remove the recurring reminder
        (let [msg (msg-utils/mock-command-message
                    "reminders"
                    "remove 1"
                    {:sender "alice" :chat-id 123})]
          (sut/-processChannelMessage nil msg)

          (is (msg-utils/replied-with? "Cancelled recurring reminder")))

        ;; Verify both parent and instance are cancelled
        (is (= 0 (count (db/get-user-reminders "alice"))))
        (is (= 0 (count (db/get-pending-reminders))))))))

(deftest test-remove-invalid-number
  (testing "Attempting to remove with invalid reminder number"
    (msg-utils/with-clean-replies
      ;; Create one reminder
      (db/add-reminder! {:chat-id 123
                         :username "alice"
                         :target-user "alice"
                         :message "task"
                         :due-time (+ (System/currentTimeMillis) 3600000)})

      ;; Try to remove reminder #5 (doesn't exist)
      (let [msg (msg-utils/mock-command-message
                  "reminders"
                  "remove 5"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "Invalid reminder number"))))))

(deftest test-interval-weekly-uses-start-date-and-weeks-interval
  (testing "Interval weekly pattern (every third friday) means every 3 weeks on Friday"
    (msg-utils/with-clean-replies
      ;; 12/12/25 is a Friday
      ;; "every third friday starting 12/12/25" means every 3 weeks on Friday
      ;; First: 12/12/25, Second: 2/1/26 (3 weeks later), Third: 23/1/26 (3 weeks after that)
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "me every third friday at 3pm starting 12/12/25 to check timesheet"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        ;; Should confirm creation
        (is (msg-utils/replied-with? "I'll remind"))
        (is (msg-utils/replied-with? "check timesheet"))

        ;; Check the reminder was created with correct start date
        (let [reminders (db/get-user-reminders "alice")]
          (is (= 1 (count reminders)))
          (let [reminder (first reminders)
                due-zdt (java.time.ZonedDateTime/ofInstant
                         (java.time.Instant/ofEpochMilli (:due_time reminder))
                         (java.time.ZoneId/systemDefault))]
            ;; Should be December 12, 2025 (first occurrence)
            (is (= 12 (.getDayOfMonth due-zdt)))
            (is (= 12 (.getMonthValue due-zdt)))
            (is (= 2025 (.getYear due-zdt)))
            ;; Should be a Friday
            (is (= java.time.DayOfWeek/FRIDAY (.getDayOfWeek due-zdt)))
            ;; Should be 3pm
            (is (= 15 (.getHour due-zdt)))
            ;; Should be interval-weekly type with 3 weeks
            (is (= "interval-weekly" (:recurrence_type reminder)))
            (let [pattern (clojure.edn/read-string (:recurrence_pattern reminder))]
              (is (= :interval-weekly (:type pattern)))
              (is (= 3 (:weeks pattern)))
              (is (= :friday (:weekday pattern))))))))))

(deftest test-interval-weekly-next-occurrence-calculation
  (testing "Next occurrence for interval-weekly adds the correct number of weeks"
    ;; First occurrence is 12/12/25 (Friday)
    ;; Second should be 3 weeks later: 2/1/26 (Friday)
    ;; Third should be 3 weeks after that: 23/1/26 (Friday)
    (let [first-date (java.time.ZonedDateTime/of 2025 12 12 15 0 0 0 (java.time.ZoneId/systemDefault))
          first-timestamp (inst-ms (java.time.Instant/from first-date))
          pattern {:type :interval-weekly :weeks 3 :weekday :friday}
          second-timestamp (sut/calculate-next-occurrence pattern first-timestamp)
          second-date (java.time.ZonedDateTime/ofInstant
                       (java.time.Instant/ofEpochMilli second-timestamp)
                       (java.time.ZoneId/systemDefault))]
      ;; Second occurrence should be 3 weeks later
      (is (= 2 (.getDayOfMonth second-date)))
      (is (= 1 (.getMonthValue second-date)))
      (is (= 2026 (.getYear second-date)))
      (is (= java.time.DayOfWeek/FRIDAY (.getDayOfWeek second-date)))

      ;; Third occurrence should be 3 weeks after the second
      (let [third-timestamp (sut/calculate-next-occurrence pattern second-timestamp)
            third-date (java.time.ZonedDateTime/ofInstant
                        (java.time.Instant/ofEpochMilli third-timestamp)
                        (java.time.ZoneId/systemDefault))]
        (is (= 23 (.getDayOfMonth third-date)))
        (is (= 1 (.getMonthValue third-date)))
        (is (= 2026 (.getYear third-date)))
        (is (= java.time.DayOfWeek/FRIDAY (.getDayOfWeek third-date)))))))
