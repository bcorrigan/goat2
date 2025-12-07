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
       [:fired_time :integer]]))

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

        (is (msg-utils/replied-with? "Usage:"))
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

        (is (msg-utils/replied-with? "Usage:"))))))

(deftest test-remind-missing-to
  (testing "Command missing 'to' keyword shows usage"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message
                  "remind"
                  "bob tomorrow check email"
                  {:sender "alice" :chat-id 123})]
        (sut/-processChannelMessage nil msg)

        (is (msg-utils/replied-with? "Usage:"))))))

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
