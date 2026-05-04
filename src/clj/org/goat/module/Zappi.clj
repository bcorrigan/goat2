(ns org.goat.module.Zappi
  "Watchdog module for a Zappi 2 EV charger on the local network.

   Periodically pings the charger and the router. If the charger is unreachable
   while the router is fine, it reports 'Zappi Down!!!' to a configured chat,
   then 'Zappi Up!!!' once it returns. Hysteresis is applied so a single missed
   ping doesn't trigger a notification.

   Commands:
     watchzappi          - show current status; claim this chat as the report
                           destination if none is set
     watchzappi here     - claim this chat as the report destination
     watchzappi off      - clear the report destination (checks keep running)
     watchzappi history  - show recent transitions and downtime periods"
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.message-parse :as msg-parse]
            [org.goat.db.zappi :as db]
            [clojure.core.async :refer [go-loop timeout <!]]
            [clojure.string :as str])
  (:import [java.net Socket InetSocketAddress]
           [java.time Instant ZoneId Duration]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; Configuration
;; ============================================================================

(def zappi-ip   "192.168.1.8")
(def router-ip  "192.168.1.1")
(def internet-ip "1.1.1.1")
(def myenergi-host "director.myenergi.net")

(def poll-interval-ms 60000)
(def tcp-timeout-ms 2000)
(def ping-timeout-s 2)

;; ============================================================================
;; Reachability primitives
;; ============================================================================

(defn tcp-reachable?
  "Try to open a TCP connection to host:port within timeout-ms. Returns true on
   successful connect, false on any failure (refused, timeout, unreachable)."
  [host port timeout-ms]
  (try
    (with-open [sock (Socket.)]
      (.connect sock (InetSocketAddress. ^String host (int port)) (int timeout-ms))
      (.isConnected sock))
    (catch Exception _ false)))

(defn icmp-reachable?
  "Shell out to the system ping(8). One packet, short timeout. Returns true if
   ping exits 0, false otherwise. Java's InetAddress/isReachable can't do real
   ICMP without root on Linux, so we use the OS tool."
  [host]
  (try
    (let [pb (ProcessBuilder.
              ^java.util.List
              ["ping" "-c" "1" "-W" (str ping-timeout-s) host])
          _ (.redirectErrorStream pb true)
          proc (.start pb)
          finished? (.waitFor proc (long (+ (* 1000 ping-timeout-s) 1000))
                              java.util.concurrent.TimeUnit/MILLISECONDS)]
      (if finished?
        (zero? (.exitValue proc))
        (do (.destroyForcibly proc) false)))
    (catch Exception _ false)))

;; ============================================================================
;; Composite checks (defn so tests can with-redefs them)
;; ============================================================================

(defn check-zappi
  "True if the Zappi answers a TCP connect on port 80 OR an ICMP ping. The
   Zappi's local HubAPI lives on port 80 so the TCP path is the most reliable
   sign of life; we fall back to ICMP in case the HTTP service is briefly down
   while the unit is otherwise on the network."
  []
  (or (tcp-reachable? zappi-ip 80 tcp-timeout-ms)
      (icmp-reachable? zappi-ip)))

(defn check-router []
  (or (tcp-reachable? router-ip 80 tcp-timeout-ms)
      (icmp-reachable? router-ip)))

(defn check-internet []
  (icmp-reachable? internet-ip))

(defn check-myenergi-cloud []
  (tcp-reachable? myenergi-host 443 tcp-timeout-ms))

(defn check-zappi-confirmed
  "Run the zappi check. If the first result disagrees with current-status,
   immediately retry once and use that retry as the answer. This means a
   transition only ever happens after two consecutive checks agree, while
   readings that just confirm the existing state cost only one ping. Returns
   the final boolean."
  [current-status]
  (let [first? (check-zappi)
        wants-up? (not= current-status :down)]
    (if (= first? wants-up?)
      first?
      (check-zappi))))

(defn snapshot
  "Take a fresh reachability snapshot. The :zappi? value is double-checked
   against current-status (see check-zappi-confirmed); the other components
   are sampled once - they're informational and don't drive transitions."
  [current-status]
  {:zappi?     (check-zappi-confirmed current-status)
   :router?    (check-router)
   :internet?  (check-internet)
   :myenergi?  (check-myenergi-cloud)
   :timestamp  (System/currentTimeMillis)})

;; ============================================================================
;; State machine
;; ============================================================================

(def initial-state
  {:status :unknown    ; :unknown | :up | :down
   :down-since nil     ; epoch ms of the declared :down transition
   :last-check nil})

(def state (atom initial-state))

(defn step
  "Pure state-machine transition. Given the current state map and a snapshot
   in which :zappi? has already been double-checked (see tick!/check-zappi-confirmed),
   returns {:state new-state :event nil|:down|:up :event-time ts :details map}.

   - If the router is unreachable too, the bot's local network is suspect, so
     we record the check time but never transition.
   - :unknown is treated like :up: if we boot when the charger is already down,
     two consecutive failures will still produce a :down alert."
  [s {:keys [zappi? router? timestamp] :as snap}]
  (let [base (assoc s :last-check timestamp)
        status (:status s)]
    (cond
      ;; Local network suspect - hold transitions
      (and (not zappi?) (not router?))
      {:state base :event nil}

      ;; Up confirmed and currently down -> :up (with event)
      (and zappi? (= status :down))
      {:state (assoc base :status :up :down-since nil)
       :event :up
       :event-time timestamp
       :details {:offline-ms (when (:down-since s)
                               (- timestamp (:down-since s)))
                 :snapshot snap}}

      ;; Up confirmed from :unknown -> :up silently (we never declared :down)
      (and zappi? (= status :unknown))
      {:state (assoc base :status :up) :event nil}

      ;; Down confirmed and currently up/unknown -> :down (with event)
      (and (not zappi?) router? (not= status :down))
      {:state (assoc base :status :down :down-since timestamp)
       :event :down
       :event-time timestamp
       :details {:snapshot snap}}

      :else
      {:state base :event nil})))

;; ============================================================================
;; Formatting helpers
;; ============================================================================

(def ^:private uk-zone (ZoneId/of "Europe/London"))
(def ^:private fmt-full   (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
(def ^:private fmt-short  (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))
(def ^:private fmt-time   (DateTimeFormatter/ofPattern "HH:mm"))

(defn- fmt-ts [ts formatter]
  (.format ^DateTimeFormatter formatter
           (.atZone (Instant/ofEpochMilli ts) uk-zone)))

(defn format-duration
  "Render a millisecond duration as a compact human string like '3m 12s' or
   '1h 4m'. Anything under a minute is shown in seconds."
  [ms]
  (when (and ms (>= ms 0))
    (let [total-s (long (/ ms 1000))
          h (long (/ total-s 3600))
          m (long (/ (mod total-s 3600) 60))
          s (long (mod total-s 60))]
      (cond
        (pos? h) (format "%dh %dm" h m)
        (pos? m) (format "%dm %ds" m s)
        :else    (format "%ds" s)))))

(defn- check-glyph [b] (if b "OK" "FAIL"))

(defn format-snapshot
  "Multi-line text describing the snapshot's reachability flags. Each line is
   labelled with the address probed so it's obvious what was tested."
  [{:keys [zappi? router? internet? myenergi?]}]
  (let [components [["Zappi          " zappi-ip       zappi?]
                    ["Router         " router-ip      router?]
                    ["Internet       " internet-ip    internet?]
                    ["myenergi cloud " myenergi-host  myenergi?]]]
    (str/join "\n"
              (map (fn [[label addr ok?]]
                     (str "  " label "(" addr "): " (check-glyph ok?)))
                   components))))

(defn- snapshot-summary
  "One-line summary suitable for embedding in DB :details."
  [snap]
  (str "zappi=" (:zappi? snap)
       " router=" (:router? snap)
       " internet=" (:internet? snap)
       " myenergi=" (:myenergi? snap)))

;; ============================================================================
;; Notifications
;; ============================================================================

(defn- send-text!
  "Send a free-text message to the given chat-id. Used for proactive alerts
   that aren't a reply to any inbound message."
  [chat-id text]
  (try
    (let [msg (msg-parse/create-message
               :chat-id (Long/valueOf (long chat-id))
               :text text
               :sender "goat"
               :private? false)]
      (msg/send-msg msg))
    (catch Exception e
      (println "Zappi: failed to send notification:" (.getMessage e)))))

(defn- down-message [event-time details]
  (let [snap (:snapshot details)]
    (str "Zappi Down!!!\n"
         "Time: " (fmt-ts event-time fmt-full) "\n\n"
         (format-snapshot snap) "\n\n"
         (cond
           (and (:router? snap) (:internet? snap) (:myenergi? snap))
           "Local network and myenergi cloud both healthy - looks like a Zappi-side fault."
           (and (:router? snap) (not (:internet? snap)))
           "Internet connection seems to be down too."
           (:router? snap)
           "Router is up; the Zappi has gone offline locally."
           :else
           "Unable to confirm router status."))))

(defn- up-message [event-time details]
  (let [offline-ms (:offline-ms details)]
    (str "Zappi Up!!!\n"
         "Time: " (fmt-ts event-time fmt-full)
         (when offline-ms
           (str "\nOffline for " (format-duration offline-ms) ".")))))

(defn- handle-event!
  "Persist a transition event and notify the report chat if one is configured."
  [event event-time details]
  (let [text (case event
               :down (down-message event-time details)
               :up   (up-message event-time details))]
    (db/add-event! event event-time (snapshot-summary (:snapshot details)))
    (println (str "Zappi watchdog: " (name event) " at " (fmt-ts event-time fmt-full)))
    (when-let [chat-id (db/get-report-chat-id)]
      (send-text! chat-id text))))

;; ============================================================================
;; Scheduler
;; ============================================================================

(defn tick!
  "Run one watchdog cycle: take a snapshot (with double-check on the zappi
   reading), advance the state machine, and fire side effects on any
   transition."
  []
  (let [snap (snapshot (:status @state))
        result (step @state snap)]
    (reset! state (:state result))
    (when-let [event (:event result)]
      (handle-event! event (:event-time result) (:details result)))
    result))

(def ^:private scheduler-running (atom false))

(defn start-zappi-watcher!
  "Kick off the polling loop. Idempotent - safe to call multiple times."
  []
  (when (compare-and-set! scheduler-running false true)
    (println "Starting Zappi watchdog...")
    (go-loop []
      (try
        (tick!)
        (catch Exception e
          (println "Zappi watchdog tick error:" (.getMessage e))))
      (<! (timeout poll-interval-ms))
      (recur))))

;; Start on namespace load. Tests can suppress this by setting the system
;; property goat.zappi.no-autostart=true before requiring this namespace.
(when-not (= "true" (System/getProperty "goat.zappi.no-autostart"))
  (start-zappi-watcher!))

;; ============================================================================
;; History formatting (pure)
;; ============================================================================

(defn pair-events
  "Walk events in chronological order and pair each :down with the next :up.
   Returns a vector of {:down ts :up ts-or-nil :duration-ms}. A trailing :down
   with no matching :up (i.e. currently down) is included with :up nil."
  [events-asc]
  (loop [evts (seq events-asc)
         pending-down nil
         out []]
    (if-not evts
      (cond-> out
        pending-down (conj {:down (:timestamp pending-down)
                            :up nil
                            :duration-ms nil}))
      (let [e (first evts)]
        (case (:event e)
          :down
          (recur (next evts) e
                 (cond-> out
                   ;; consecutive :down without an :up - close the previous one
                   ;; with no end time (data hiccup, but be defensive)
                   pending-down (conj {:down (:timestamp pending-down)
                                       :up nil
                                       :duration-ms nil})))
          :up
          (if pending-down
            (recur (next evts) nil
                   (conj out {:down (:timestamp pending-down)
                              :up (:timestamp e)
                              :duration-ms (- (:timestamp e) (:timestamp pending-down))}))
            ;; orphan :up - skip
            (recur (next evts) nil out)))))))

(defn- format-history-event
  "Render one event for the 'Last N events' list. For an :up event, include the
   duration of the immediately preceding :down (already paired up by caller)."
  [e prev-down-ts]
  (let [ts (:timestamp e)
        when-str (fmt-ts ts fmt-short)]
    (case (:event e)
      :down (str " " when-str "  Down")
      :up   (let [extra (when prev-down-ts
                          (format-duration (- ts prev-down-ts)))]
              (str " " when-str "  Up"
                   (when extra (str "   (after " extra " offline)")))))))

(defn- format-history-events
  "Render the 'Last N events' section. Events arrive newest-first; we look up
   each :up's preceding :down by timestamp from the full history."
  [events-desc all-events-asc]
  (let [downs-by-ts (->> all-events-asc
                         (filter #(= :down (:event %)))
                         (map :timestamp))
        ;; For each :up event, find the largest :down timestamp that's < its ts
        prev-down-for (fn [up-ts]
                        (last (filter #(< % up-ts) downs-by-ts)))]
    (->> events-desc
         (map (fn [e] (format-history-event e
                                            (when (= :up (:event e))
                                              (prev-down-for (:timestamp e))))))
         (str/join "\n"))))

(defn- format-period
  "Render one downtime period for the 'Recent downtime periods' section."
  [{:keys [down up duration-ms]}]
  (let [date (fmt-ts down fmt-short)]
    (cond
      (nil? up)
      (str "- " date " -> still down ("
           (format-duration (- (System/currentTimeMillis) down)) " and counting)")

      :else
      (str "- " date " -> " (fmt-ts up fmt-time)
           "  (" (format-duration duration-ms) ")"))))

(defn format-history
  "Build the full reply for 'watchzappi history'. period-limit caps the number
   of downtime periods displayed (most recent first)."
  [last-events all-events-asc period-limit]
  (if (empty? all-events-asc)
    "Zappi Watchdog History\n\nNo events recorded yet."
    (let [periods (->> (pair-events all-events-asc)
                       reverse
                       (take period-limit))]
      (str "Zappi Watchdog History\n\n"
           "Last " (count last-events) " events:\n"
           (format-history-events last-events all-events-asc)
           "\n\n"
           "Recent downtime periods:\n"
           (if (empty? periods)
             "(none)"
             (str/join "\n" (map format-period periods)))))))

;; ============================================================================
;; Status snapshot reply
;; ============================================================================

(defn format-status
  "Render the 'watchzappi' status snapshot. Includes current state, last check,
   live diagnostic snapshot, and configured report chat."
  [s snap report-chat-id this-chat-id]
  (let [{:keys [status down-since last-check]} s]
    (str "Zappi Watchdog\n\n"
         "State: " (case status
                     :up "UP"
                     :down "DOWN"
                     :unknown "UNKNOWN (no checks completed yet)")
         (when (and (= status :down) down-since)
           (str "  (since " (fmt-ts down-since fmt-full)
                ", " (format-duration (- (System/currentTimeMillis) down-since))
                " ago)"))
         "\n"
         (when last-check
           (str "Last check: " (fmt-ts last-check fmt-full) "\n"))
         "\n"
         "Live snapshot:\n"
         (format-snapshot snap)
         "\n\n"
         (cond
           (nil? report-chat-id)
           "No report chat configured."

           (= report-chat-id this-chat-id)
           "Reporting to this chat."

           :else
           (str "Reporting to chat " report-chat-id ".")))))

;; ============================================================================
;; Module
;; ============================================================================

(defn show-help
  "Render the help reply. Uses HTML formatting (Telegram parse-mode) for bold
   headers and command names; renders fine as plain text in the CLI too."
  []
  (str "🚗⚡ <b>Zappi Watchdog</b>\n\n"
       "I keep an eye on your Zappi 2 charger at <b>" zappi-ip "</b>. "
       "Once a minute I check it's reachable on the local network. "
       "If it stops answering I retry once immediately to rule out a blip; "
       "two consecutive failures means the unit really is offline and you'll "
       "get a <b>Zappi Down!!!</b> alert here. When it comes back, "
       "two consecutive successes triggers a <b>Zappi Up!!!</b> alert "
       "with the offline duration.\n\n"
       "Each alert also reports the router (<b>" router-ip "</b>), "
       "internet (<b>" internet-ip "</b>) and myenergi cloud "
       "(<b>" myenergi-host "</b>) so you can see at a glance "
       "where the fault is. If the router is unreachable too, no alert "
       "fires - that's a local network problem, not the Zappi.\n\n"
       "<b>Subcommands</b>\n"
       "• <b>watchzappi</b> - show current state and a live snapshot of all "
       "four components. If no chat has been set as the alert destination yet, "
       "running this here claims it.\n"
       "• <b>watchzappi here</b> - explicitly route alerts to this chat "
       "(overrides any previous setting).\n"
       "• <b>watchzappi off</b> - stop sending alerts. Checks keep running and "
       "events keep being recorded; you just won't be pinged.\n"
       "• <b>watchzappi history</b> - last 5 transition events and the most "
       "recent downtime periods with durations. An ongoing outage shows as "
       "&quot;still down&quot; with a live counter.\n"
       "• <b>watchzappi help</b> - show this message."))

(defmodule Zappi
  :commands [:watchzappi]
  :receive-messages :commands
  :wants-private true

  (defn process-message [m]
    (let [arg (str/trim (or (msg/mod-text m) ""))
          chat-id (msg/chat-id m)]
      (case arg
        "" (let [snap (snapshot (:status @state))
                 existing (db/get-report-chat-id)]
             (when-not existing
               (db/set-report-chat-id! chat-id))
             (msg/reply m (format-status @state snap
                                         (or existing chat-id)
                                         chat-id)))

        "here" (do (db/set-report-chat-id! chat-id)
                   (msg/reply m "OK - I'll send Zappi up/down alerts to this chat."))

        "off"  (do (db/clear-report-chat-id!)
                   (msg/reply m "OK - Zappi alerts disabled. Checks keep running."))

        "history" (msg/reply m (format-history (db/last-n-events 5)
                                               (db/all-events-asc)
                                               10))

        "help" (msg/reply m (show-help))

        (msg/reply m (str "Unknown subcommand: " arg "\n\n" (show-help)))))))
