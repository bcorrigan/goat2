;; Suppress the watchdog autostart before the Zappi namespace loads.
;; This must run before the (ns ...) form's :require triggers the load.
(do (System/setProperty "goat.zappi.no-autostart" "true"))

(ns org.goat.module.Zappi-test
  "Tests for the Zappi watchdog: state-machine transitions, retry-on-disagreement
   semantics, history pairing, and the watchzappi command surface (via mock
   messages)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as sql]
            [org.goat.module.Zappi :as sut]
            [org.goat.db.zappi :as db]
            [org.goat.testutils.message :as msg-utils])
  (:import [java.io File]))

;; ============================================================================
;; Test DB
;; ============================================================================

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-zappi.db"})

(defn- setup-test-db []
  (let [f (File. "test/resources/test-zappi.db")]
    (when (.exists f) (.delete f)))
  (sql/db-do-commands test-db
    (sql/create-table-ddl :zappi_config
      [[:key :text "PRIMARY KEY"]
       [:value :text]]))
  (sql/db-do-commands test-db
    (sql/create-table-ddl :zappi_events
      [[:id :integer "PRIMARY KEY AUTOINCREMENT"]
       [:event :text "NOT NULL"]
       [:timestamp :integer "NOT NULL"]
       [:details :text]])))

(defn- teardown-test-db []
  (let [f (File. "test/resources/test-zappi.db")]
    (when (.exists f) (.delete f))))

(defn db-fixture [f]
  (setup-test-db)
  (msg-utils/clear-replies!)
  (with-redefs [db/db test-db]
    (reset! sut/state sut/initial-state)
    (f))
  (teardown-test-db))

(use-fixtures :each db-fixture)

;; ============================================================================
;; Helpers
;; ============================================================================

(defn- snap [zappi? router? & [ts]]
  {:zappi?    zappi?
   :router?   router?
   :internet? true
   :myenergi? true
   :timestamp (or ts 1000)})

(defn- scripted-checks
  "Build a check-zappi stand-in that returns successive values from a vector,
   then throws if exhausted (so tests catch unexpected extra calls).
   Returns [redef-fn call-count-atom]."
  [results]
  (let [calls (atom 0)
        results (vec results)]
    [(fn [] (let [i @calls]
              (when (>= i (count results))
                (throw (ex-info "check-zappi called more times than scripted"
                                {:scripted (count results) :got (inc i)})))
              (swap! calls inc)
              (nth results i)))
     calls]))

;; ============================================================================
;; Step (pure) - simplified state machine
;; ============================================================================

(deftest test-step-up-stays-up-on-success
  (let [{:keys [state event]} (sut/step (assoc sut/initial-state :status :up)
                                        (snap true true 1000))]
    (is (= :up (:status state)))
    (is (nil? event))
    (is (= 1000 (:last-check state)))))

(deftest test-step-up-to-down-on-confirmed-failure
  (testing "snapshot's :zappi? is already retry-confirmed, so a single false transitions"
    (let [{:keys [state event event-time]} (sut/step (assoc sut/initial-state :status :up)
                                                     (snap false true 1000))]
      (is (= :down (:status state)))
      (is (= :down event))
      (is (= 1000 event-time))
      (is (= 1000 (:down-since state))))))

(deftest test-step-down-to-up-on-confirmed-success
  (let [start (assoc sut/initial-state :status :down :down-since 500)
        {:keys [state event event-time details]} (sut/step start (snap true true 1500))]
    (is (= :up (:status state)))
    (is (= :up event))
    (is (= 1500 event-time))
    (is (nil? (:down-since state)))
    (is (= 1000 (:offline-ms details)))))

(deftest test-step-router-down-suppresses-transition
  (testing "If router is also unreachable, do not transition to :down"
    (let [{:keys [state event]} (sut/step (assoc sut/initial-state :status :up)
                                          (snap false false 1000))]
      (is (= :up (:status state)))
      (is (nil? event)))))

(deftest test-step-unknown-treated-as-up
  (testing "From :unknown, a confirmed failure transitions to :down"
    (let [{:keys [state event]} (sut/step sut/initial-state
                                          (snap false true 1000))]
      (is (= :down (:status state)))
      (is (= :down event)))))

(deftest test-step-unknown-promoted-to-up-silently
  (testing "A success while :unknown promotes to :up but emits no event - we never declared :down"
    (let [{:keys [state event]} (sut/step sut/initial-state
                                          (snap true true 1000))]
      (is (= :up (:status state)))
      (is (nil? event))
      (is (= 1000 (:last-check state))))))

;; ============================================================================
;; check-zappi-confirmed - retry-on-disagreement
;; ============================================================================

(deftest test-confirm-up-no-retry-needed
  (testing "When :up and ping succeeds, retry is skipped"
    (let [[f calls] (scripted-checks [true])]
      (with-redefs [sut/check-zappi f]
        (is (true? (sut/check-zappi-confirmed :up)))
        (is (= 1 @calls))))))

(deftest test-confirm-down-no-retry-needed
  (testing "When :down and ping fails, retry is skipped"
    (let [[f calls] (scripted-checks [false])]
      (with-redefs [sut/check-zappi f]
        (is (false? (sut/check-zappi-confirmed :down)))
        (is (= 1 @calls))))))

(deftest test-confirm-up-failure-retried-and-confirmed
  (testing "When :up and ping fails, retry is run; if retry also fails, return false"
    (let [[f calls] (scripted-checks [false false])]
      (with-redefs [sut/check-zappi f]
        (is (false? (sut/check-zappi-confirmed :up)))
        (is (= 2 @calls))))))

(deftest test-confirm-up-failure-retried-and-recovered
  (testing "When :up and ping fails, retry succeeds: treat as transient, return true"
    (let [[f calls] (scripted-checks [false true])]
      (with-redefs [sut/check-zappi f]
        (is (true? (sut/check-zappi-confirmed :up)))
        (is (= 2 @calls))))))

(deftest test-confirm-down-success-retried-and-confirmed
  (testing "When :down and ping succeeds, retry is run; if retry also succeeds, return true"
    (let [[f calls] (scripted-checks [true true])]
      (with-redefs [sut/check-zappi f]
        (is (true? (sut/check-zappi-confirmed :down)))
        (is (= 2 @calls))))))

(deftest test-confirm-down-success-retried-and-refuted
  (testing "When :down and ping succeeds, retry fails: treat as transient, return false"
    (let [[f calls] (scripted-checks [true false])]
      (with-redefs [sut/check-zappi f]
        (is (false? (sut/check-zappi-confirmed :down)))
        (is (= 2 @calls))))))

(deftest test-confirm-unknown-treated-as-up
  (testing ":unknown behaves like :up - failure triggers retry"
    (let [[f calls] (scripted-checks [false true])]
      (with-redefs [sut/check-zappi f]
        (is (true? (sut/check-zappi-confirmed :unknown)))
        (is (= 2 @calls))))))

;; ============================================================================
;; format-duration
;; ============================================================================

(deftest test-format-duration
  (is (= "0s" (sut/format-duration 500)))
  (is (= "5s" (sut/format-duration 5000)))
  (is (= "1m 0s" (sut/format-duration 60000)))
  (is (= "3m 12s" (sut/format-duration 192000)))
  (is (= "1h 4m" (sut/format-duration (+ 3600000 (* 4 60000)))))
  (is (nil? (sut/format-duration nil))))

;; ============================================================================
;; format-snapshot - check it includes addresses
;; ============================================================================

(deftest test-format-snapshot-shows-addresses
  (let [s (sut/format-snapshot (snap true true 0))]
    (is (re-find #"Zappi" s))
    (is (re-find #"192\.168\.1\.8" s))
    (is (re-find #"Router" s))
    (is (re-find #"192\.168\.1\.1" s))
    (is (re-find #"Internet" s))
    (is (re-find #"1\.1\.1\.1" s))
    (is (re-find #"myenergi" s))
    (is (re-find #"director\.myenergi\.net" s))))

;; ============================================================================
;; Event pairing
;; ============================================================================

(deftest test-pair-events-empty
  (is (= [] (sut/pair-events []))))

(deftest test-pair-events-single-period
  (let [pairs (sut/pair-events [{:event :down :timestamp 100}
                                {:event :up   :timestamp 250}])]
    (is (= 1 (count pairs)))
    (is (= {:down 100 :up 250 :duration-ms 150} (first pairs)))))

(deftest test-pair-events-multiple-periods
  (let [evts [{:event :down :timestamp 100}
              {:event :up   :timestamp 200}
              {:event :down :timestamp 500}
              {:event :up   :timestamp 800}]
        pairs (sut/pair-events evts)]
    (is (= 2 (count pairs)))
    (is (= 100 (-> pairs first :down)))
    (is (= 200 (-> pairs first :up)))
    (is (= 500 (-> pairs second :down)))
    (is (= 800 (-> pairs second :up)))
    (is (= 300 (-> pairs second :duration-ms)))))

(deftest test-pair-events-still-down
  (testing "A trailing :down with no :up represents an ongoing outage"
    (let [pairs (sut/pair-events [{:event :down :timestamp 100}
                                  {:event :up   :timestamp 200}
                                  {:event :down :timestamp 500}])]
      (is (= 2 (count pairs)))
      (is (nil? (:up (second pairs))))
      (is (nil? (:duration-ms (second pairs))))
      (is (= 500 (:down (second pairs)))))))

(deftest test-pair-events-orphan-up-skipped
  (testing "An :up that isn't preceded by a :down is dropped"
    (let [pairs (sut/pair-events [{:event :up   :timestamp 100}
                                  {:event :down :timestamp 200}
                                  {:event :up   :timestamp 300}])]
      (is (= 1 (count pairs)))
      (is (= 200 (:down (first pairs))))
      (is (= 300 (:up (first pairs)))))))

;; ============================================================================
;; Watchzappi command surface
;; ============================================================================

(defn- mk-msg [arg-text & [opts]]
  (msg-utils/mock-command-message
    "watchzappi" arg-text
    (merge {:sender "alice" :chat-id 7777} opts)))

(deftest test-watchzappi-empty-claims-chat
  (testing "An unconfigured chat that runs 'watchzappi' becomes the report destination"
    (with-redefs [sut/check-zappi          (constantly true)
                  sut/check-router         (constantly true)
                  sut/check-internet       (constantly true)
                  sut/check-myenergi-cloud (constantly true)]
      (msg-utils/with-clean-replies
        (sut/process-message (mk-msg ""))
        (is (= 7777 (db/get-report-chat-id)))
        (is (msg-utils/replied-with? "Zappi Watchdog"))
        (is (msg-utils/replied-with? "Reporting to this chat"))
        (is (msg-utils/replied-with? "192.168.1.8"))))))

(deftest test-watchzappi-empty-does-not-overwrite-existing
  (testing "When a report chat is already set, 'watchzappi' in another chat does not steal it"
    (db/set-report-chat-id! 1111)
    (with-redefs [sut/check-zappi          (constantly true)
                  sut/check-router         (constantly true)
                  sut/check-internet       (constantly true)
                  sut/check-myenergi-cloud (constantly true)]
      (msg-utils/with-clean-replies
        (sut/process-message (mk-msg "" {:chat-id 2222}))
        (is (= 1111 (db/get-report-chat-id)))
        (is (msg-utils/replied-with? "Reporting to chat 1111"))))))

(deftest test-watchzappi-here-claims-chat
  (testing "'watchzappi here' overrides any existing report chat"
    (db/set-report-chat-id! 1111)
    (msg-utils/with-clean-replies
      (sut/process-message (mk-msg "here" {:chat-id 2222}))
      (is (= 2222 (db/get-report-chat-id)))
      (is (msg-utils/replied-with? "I'll send")))))

(deftest test-watchzappi-off-clears-chat
  (testing "'watchzappi off' removes the report chat"
    (db/set-report-chat-id! 1111)
    (msg-utils/with-clean-replies
      (sut/process-message (mk-msg "off"))
      (is (nil? (db/get-report-chat-id)))
      (is (msg-utils/replied-with? "alerts disabled")))))

(deftest test-watchzappi-history-empty
  (testing "History reply is helpful when no events recorded"
    (msg-utils/with-clean-replies
      (sut/process-message (mk-msg "history"))
      (is (msg-utils/replied-with? "No events recorded")))))

(deftest test-watchzappi-history-with-events
  (testing "History reply shows pairing for recorded down/up events"
    (db/add-event! :down 1000000 "router=true")
    (db/add-event! :up   1300000 "router=true")
    (msg-utils/with-clean-replies
      (sut/process-message (mk-msg "history"))
      (let [reply (msg-utils/get-first-reply-text)]
        (is (some? reply))
        (is (re-find #"Last \d+ events" reply))
        (is (re-find #"Recent downtime periods" reply))
        ;; 300000 ms = 5 minutes
        (is (re-find #"5m" reply))))))

(deftest test-watchzappi-history-ongoing-outage
  (testing "An unpaired trailing :down is rendered as still-down"
    (db/add-event! :down (- (System/currentTimeMillis) 600000) "router=true")
    (msg-utils/with-clean-replies
      (sut/process-message (mk-msg "history"))
      (is (msg-utils/replied-with? "still down")))))

(deftest test-watchzappi-unknown-subcommand
  (msg-utils/with-clean-replies
    (sut/process-message (mk-msg "wibble"))
    (is (msg-utils/replied-with? "Unknown subcommand"))))

;; ============================================================================
;; tick! integration
;; ============================================================================

(deftest test-tick-fires-down-after-two-failures-in-one-tick
  (testing "Two immediate consecutive failures within a single tick produce :down"
    (db/set-report-chat-id! 9999)
    (reset! sut/state (assoc sut/initial-state :status :up))
    (let [[f _] (scripted-checks [false false])]
      (with-redefs [sut/check-zappi          f
                    sut/check-router         (constantly true)
                    sut/check-internet       (constantly true)
                    sut/check-myenergi-cloud (constantly true)]
        (msg-utils/with-clean-replies
          (sut/tick!)
          (is (= :down (:status @sut/state)))
          (is (msg-utils/replied-with? "Zappi Down"))
          (is (msg-utils/replied-to-chat? 9999))
          (is (= 1 (count (db/last-n-events 5)))))))))

(deftest test-tick-no-down-when-retry-recovers
  (testing "First check fails but retry succeeds: stays :up, no event"
    (db/set-report-chat-id! 9999)
    (reset! sut/state (assoc sut/initial-state :status :up))
    (let [[f _] (scripted-checks [false true])]
      (with-redefs [sut/check-zappi          f
                    sut/check-router         (constantly true)
                    sut/check-internet       (constantly true)
                    sut/check-myenergi-cloud (constantly true)]
        (msg-utils/with-clean-replies
          (sut/tick!)
          (is (= :up (:status @sut/state)))
          (is (zero? (msg-utils/reply-count)))
          (is (zero? (count (db/last-n-events 5)))))))))

(deftest test-tick-recovery-emits-up-after-two-successes
  (testing "Once :down, two immediate successes in one tick emit :up"
    (db/set-report-chat-id! 9999)
    (reset! sut/state (assoc sut/initial-state
                             :status :down
                             :down-since (- (System/currentTimeMillis) 600000)))
    (let [[f _] (scripted-checks [true true])]
      (with-redefs [sut/check-zappi          f
                    sut/check-router         (constantly true)
                    sut/check-internet       (constantly true)
                    sut/check-myenergi-cloud (constantly true)]
        (msg-utils/with-clean-replies
          (sut/tick!)
          (is (= :up (:status @sut/state)))
          (is (msg-utils/replied-with? "Zappi Up"))
          (is (msg-utils/replied-to-chat? 9999)))))))

(deftest test-tick-no-up-when-retry-disagrees
  (testing "While :down, a single success but failed retry stays :down"
    (db/set-report-chat-id! 9999)
    (reset! sut/state (assoc sut/initial-state
                             :status :down
                             :down-since (- (System/currentTimeMillis) 600000)))
    (let [[f _] (scripted-checks [true false])]
      (with-redefs [sut/check-zappi          f
                    sut/check-router         (constantly true)
                    sut/check-internet       (constantly true)
                    sut/check-myenergi-cloud (constantly true)]
        (msg-utils/with-clean-replies
          (sut/tick!)
          (is (= :down (:status @sut/state)))
          (is (zero? (msg-utils/reply-count))))))))

(deftest test-tick-router-down-suppresses-event
  (testing "Local network down: no transition, no notification"
    (db/set-report-chat-id! 9999)
    (reset! sut/state (assoc sut/initial-state :status :up))
    (let [[f _] (scripted-checks [false false])]
      (with-redefs [sut/check-zappi          f
                    sut/check-router         (constantly false)
                    sut/check-internet       (constantly false)
                    sut/check-myenergi-cloud (constantly false)]
        (msg-utils/with-clean-replies
          (sut/tick!)
          (is (= :up (:status @sut/state)))
          (is (zero? (msg-utils/reply-count)))
          (is (zero? (count (db/last-n-events 5)))))))))

(deftest test-tick-no-notify-when-no-report-chat
  (testing "Events are still logged, just no notification"
    (db/clear-report-chat-id!)
    (reset! sut/state (assoc sut/initial-state :status :up))
    (let [[f _] (scripted-checks [false false])]
      (with-redefs [sut/check-zappi          f
                    sut/check-router         (constantly true)
                    sut/check-internet       (constantly true)
                    sut/check-myenergi-cloud (constantly true)]
        (msg-utils/with-clean-replies
          (sut/tick!)
          (is (= :down (:status @sut/state)))
          (is (zero? (msg-utils/reply-count)))
          (is (= 1 (count (db/last-n-events 5)))))))))
