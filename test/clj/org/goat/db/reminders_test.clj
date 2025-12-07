(ns org.goat.db.reminders-test
  (:require [clojure.test :refer :all]
            [org.goat.db.reminders :as sut]
            [clojure.java.jdbc :as sql]
            [clojure.java.io :as io]))

;; Test database configuration
(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/reminders_test.db"})

(defn clean-test-db
  "Remove test database file"
  []
  (when (.exists (io/file "resources/reminders_test.db"))
    (io/delete-file "resources/reminders_test.db")))

(defn setup-test-db
  "Override the db var in reminders namespace with test db"
  []
  (clean-test-db)
  (alter-var-root #'sut/db (constantly test-db))
  (sut/create-db))

(defn teardown-test-db
  "Clean up after tests"
  []
  (clean-test-db))

(use-fixtures :each
  (fn [f]
    (setup-test-db)
    (f)
    (teardown-test-db)))

;; ============================================================================
;; Basic CRUD Tests
;; ============================================================================

(deftest test-add-reminder
  (testing "Adding a reminder returns an ID"
    (let [due-time (+ (System/currentTimeMillis) 3600000)  ; 1 hour from now
          reminder-id (sut/add-reminder!
                        {:chat-id 123
                         :username "alice"
                         :target-user "bob"
                         :message "buy milk"
                         :due-time due-time})]
      (is (number? reminder-id))
      (is (pos? reminder-id))

      (let [reminder (sut/get-reminder-by-id reminder-id)]
        (is (= 123 (:chat_id reminder)))
        (is (= "alice" (:username reminder)))
        (is (= "bob" (:target_user reminder)))
        (is (= "buy milk" (:message reminder)))
        (is (= due-time (:due_time reminder)))
        (is (= "pending" (:status reminder)))
        (is (some? (:set_time reminder)))
        (is (nil? (:fired_time reminder)))))))

(deftest test-add-reminder-to-self
  (testing "Adding a reminder to yourself"
    (let [due-time (+ (System/currentTimeMillis) 3600000)
          reminder-id (sut/add-reminder!
                        {:chat-id 456
                         :username "alice"
                         :target-user "alice"
                         :message "check email"
                         :due-time due-time})]
      (is (some? reminder-id))

      (let [reminder (sut/get-reminder-by-id reminder-id)]
        (is (= "alice" (:username reminder)))
        (is (= "alice" (:target_user reminder)))))))

(deftest test-get-reminder-by-id
  (testing "Getting a reminder by ID"
    (let [due-time (+ (System/currentTimeMillis) 3600000)
          reminder-id (sut/add-reminder!
                        {:chat-id 123
                         :username "alice"
                         :target-user "bob"
                         :message "task"
                         :due-time due-time})
          reminder (sut/get-reminder-by-id reminder-id)]
      (is (some? reminder))
      (is (= reminder-id (:reminder_id reminder)))
      (is (= "task" (:message reminder))))))

(deftest test-get-nonexistent-reminder
  (testing "Getting a nonexistent reminder returns nil"
    (is (nil? (sut/get-reminder-by-id 99999)))))

;; ============================================================================
;; Query Tests
;; ============================================================================

(deftest test-get-next-pending-reminder
  (testing "Get next pending reminder returns earliest"
    (let [now (System/currentTimeMillis)
          ;; Add reminders out of order
          id1 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task1" :due-time (+ now 7200000)})  ; 2 hours
          id2 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task2" :due-time (+ now 3600000)})  ; 1 hour
          id3 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task3" :due-time (+ now 10800000)})]  ; 3 hours
      (let [next-reminder (sut/get-next-pending-reminder)]
        (is (= id2 (:reminder_id next-reminder)))
        (is (= "task2" (:message next-reminder)))))))

(deftest test-get-next-pending-reminder-empty
  (testing "Get next pending reminder with no reminders returns nil"
    (is (nil? (sut/get-next-pending-reminder)))))

(deftest test-get-pending-reminders
  (testing "Get all pending reminders in order"
    (let [now (System/currentTimeMillis)
          id1 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task1" :due-time (+ now 7200000)})
          id2 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task2" :due-time (+ now 3600000)})
          id3 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task3" :due-time (+ now 10800000)})]
      (let [reminders (sut/get-pending-reminders)]
        (is (= 3 (count reminders)))
        ;; Should be ordered by due_time ascending
        (is (= [id2 id1 id3] (map :reminder_id reminders)))
        (is (= ["task2" "task1" "task3"] (map :message reminders)))))))

(deftest test-get-user-reminders
  (testing "Get reminders for a specific user"
    (let [now (System/currentTimeMillis)]
      (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "bob"
                          :message "task for bob" :due-time (+ now 3600000)})
      (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "charlie"
                          :message "task for charlie" :due-time (+ now 3600000)})
      (sut/add-reminder! {:chat-id 123 :username "bob" :target-user "bob"
                          :message "another task for bob" :due-time (+ now 7200000)})

      (let [bob-reminders (sut/get-user-reminders "bob")]
        (is (= 2 (count bob-reminders)))
        (is (every? #(= "bob" (:target_user %)) bob-reminders)))

      (let [charlie-reminders (sut/get-user-reminders "charlie")]
        (is (= 1 (count charlie-reminders)))
        (is (= "task for charlie" (:message (first charlie-reminders))))))))

(deftest test-get-user-reminders-case-insensitive
  (testing "Get user reminders is case-insensitive"
    (let [now (System/currentTimeMillis)]
      (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "Bob"
                          :message "task" :due-time (+ now 3600000)})

      (is (= 1 (count (sut/get-user-reminders "bob"))))
      (is (= 1 (count (sut/get-user-reminders "BOB"))))
      (is (= 1 (count (sut/get-user-reminders "Bob")))))))

;; ============================================================================
;; Status Update Tests
;; ============================================================================

(deftest test-mark-reminder-fired
  (testing "Marking reminder as fired updates status and timestamp"
    (let [due-time (+ (System/currentTimeMillis) 3600000)
          reminder-id (sut/add-reminder!
                        {:chat-id 123
                         :username "alice"
                         :target-user "bob"
                         :message "task"
                         :due-time due-time})]

      ;; Initially pending
      (is (= "pending" (:status (sut/get-reminder-by-id reminder-id))))
      (is (nil? (:fired_time (sut/get-reminder-by-id reminder-id))))

      ;; Mark as fired
      (sut/mark-reminder-fired! reminder-id)

      (let [reminder (sut/get-reminder-by-id reminder-id)]
        (is (= "fired" (:status reminder)))
        (is (some? (:fired_time reminder)))
        (is (number? (:fired_time reminder)))))))

(deftest test-fired-reminder-not-in-pending
  (testing "Fired reminder does not appear in pending queries"
    (let [now (System/currentTimeMillis)
          id1 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task1" :due-time (+ now 3600000)})
          id2 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                   :message "task2" :due-time (+ now 7200000)})]

      ;; Both should be pending
      (is (= 2 (count (sut/get-pending-reminders))))

      ;; Fire the first one
      (sut/mark-reminder-fired! id1)

      ;; Only one should be pending now
      (let [pending (sut/get-pending-reminders)]
        (is (= 1 (count pending)))
        (is (= id2 (:reminder_id (first pending)))))

      ;; But the fired one still exists in DB
      (is (some? (sut/get-reminder-by-id id1))))))

(deftest test-cancel-reminder
  (testing "Cancelling a reminder updates status"
    (let [due-time (+ (System/currentTimeMillis) 3600000)
          reminder-id (sut/add-reminder!
                        {:chat-id 123
                         :username "alice"
                         :target-user "bob"
                         :message "task"
                         :due-time due-time})]

      ;; Cancel it
      (sut/cancel-reminder! reminder-id)

      (let [reminder (sut/get-reminder-by-id reminder-id)]
        (is (= "cancelled" (:status reminder)))
        ;; Should not appear in pending
        (is (empty? (sut/get-pending-reminders)))))))

;; ============================================================================
;; Cleanup Tests
;; ============================================================================

(deftest test-delete-old-reminders
  (testing "Deleting old fired/cancelled reminders"
    (let [now (System/currentTimeMillis)
          old-time (- now (* 31 24 60 60 1000))  ; 31 days ago
          recent-time (- now (* 29 24 60 60 1000))]  ; 29 days ago

      ;; Add some reminders and mark them as fired/cancelled
      (let [id1 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                     :message "old fired" :due-time old-time})
            id2 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                     :message "recent fired" :due-time recent-time})
            id3 (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                                     :message "pending" :due-time (+ now 3600000)})]

        ;; Fire the first two (need to update fired_time manually for this test)
        (sql/execute! test-db
          ["UPDATE reminders SET status='fired', fired_time=? WHERE reminder_id=?" old-time id1])
        (sql/execute! test-db
          ["UPDATE reminders SET status='fired', fired_time=? WHERE reminder_id=?" recent-time id2])

        ;; Delete old reminders (default 30 days)
        (sut/delete-old-reminders!)

        ;; Old fired reminder should be gone
        (is (nil? (sut/get-reminder-by-id id1)))

        ;; Recent fired reminder should still exist
        (is (some? (sut/get-reminder-by-id id2)))

        ;; Pending reminder should still exist
        (is (some? (sut/get-reminder-by-id id3)))))))

;; ============================================================================
;; Integration Tests
;; ============================================================================

(deftest test-full-workflow
  (testing "Complete workflow: create reminder, query it, mark as fired"
    (let [now (System/currentTimeMillis)
          due-time (+ now 3600000)]

      ;; Alice sets a reminder for Bob
      (let [reminder-id (sut/add-reminder!
                          {:chat-id 123
                           :username "alice"
                           :target-user "bob"
                           :message "buy milk"
                           :due-time due-time})]

        ;; Should be the next pending reminder
        (is (= reminder-id (:reminder_id (sut/get-next-pending-reminder))))

        ;; Should be in Bob's reminders
        (let [bob-reminders (sut/get-user-reminders "bob")]
          (is (= 1 (count bob-reminders)))
          (is (= "buy milk" (:message (first bob-reminders)))))

        ;; Fire the reminder
        (sut/mark-reminder-fired! reminder-id)

        ;; Should no longer be pending
        (is (nil? (sut/get-next-pending-reminder)))
        (is (empty? (sut/get-user-reminders "bob")))

        ;; But should still exist in DB with fired status
        (let [reminder (sut/get-reminder-by-id reminder-id)]
          (is (= "fired" (:status reminder)))
          (is (some? (:fired_time reminder))))))))

(deftest test-multiple-users-workflow
  (testing "Multiple users with multiple reminders"
    (let [now (System/currentTimeMillis)]

      ;; Alice sets reminders
      (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "alice"
                          :message "task 1" :due-time (+ now 3600000)})
      (sut/add-reminder! {:chat-id 123 :username "alice" :target-user "bob"
                          :message "task 2" :due-time (+ now 7200000)})

      ;; Bob sets reminders
      (sut/add-reminder! {:chat-id 456 :username "bob" :target-user "bob"
                          :message "task 3" :due-time (+ now 10800000)})
      (sut/add-reminder! {:chat-id 456 :username "bob" :target-user "alice"
                          :message "task 4" :due-time (+ now 14400000)})

      ;; Check Alice's reminders (as target)
      (let [alice-reminders (sut/get-user-reminders "alice")]
        (is (= 2 (count alice-reminders)))
        (is (= #{"task 1" "task 4"} (set (map :message alice-reminders)))))

      ;; Check Bob's reminders (as target)
      (let [bob-reminders (sut/get-user-reminders "bob")]
        (is (= 2 (count bob-reminders)))
        (is (= #{"task 2" "task 3"} (set (map :message bob-reminders)))))

      ;; Check all pending
      (is (= 4 (count (sut/get-pending-reminders))))

      ;; Next should be the earliest (task 1)
      (is (= "task 1" (:message (sut/get-next-pending-reminder)))))))
