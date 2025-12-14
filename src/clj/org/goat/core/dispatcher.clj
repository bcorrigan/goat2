(ns org.goat.core.dispatcher
  "Core.async based message dispatcher.

   Processes incoming messages from channels/incoming-chan and dispatches
   them to registered modules based on message type and commands."
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
            [clojure.tools.logging :as log]
            [org.goat.core.registry :as registry]
            [org.goat.core.channels :as channels]
            [org.goat.core.message :as msg]))

;; Control channel for shutdown
(defonce ^:private control-chan (chan))

;; Reference to the dispatcher go-loop for monitoring
(defonce ^:private dispatcher-loop-chan (atom nil))

(defn- should-dispatch-to-module?
  "Determine if message should be dispatched to this module.
   Checks private message preference."
  [msg module]
  (let [{:keys [wants-private]} module
        is-private (:message/private? msg)]
    ;; Only check wants-private for private messages
    ;; (all modules receive channel messages)
    (or (not is-private) wants-private)))

(defn- get-message-command
  "Extract command keyword from message, or nil if no command"
  [msg]
  (:message/command msg))

(defn- command-matches?
  "Check if message command matches any of module's commands"
  [msg-cmd module-commands]
  (and msg-cmd
       (seq module-commands)
       (contains? (set module-commands) msg-cmd)))

(defn- dispatch-message
  "Dispatch a single message to appropriate modules.

   Implements the same logic as the original dispatcher:
   1. Send to all :all modules
   2. Check if any :all module claims the command
   3. Send to :commands modules if command matches
   4. If not claimed, send to :unclaimed modules"
  [msg]
  ;; Skip messages without text or documents
  (when (or (:message/text msg)
            (:message.attachment/document-bytes msg))
    (let [modules (registry/get-modules)
          {:keys [all unclaimed commands]} (group-by :message-type modules)
          msg-cmd (get-message-command msg)

          ;; Step 1: Dispatch to :all modules
          _ (doseq [module all]
              (when (should-dispatch-to-module? msg module)
                (try
                  ((:dispatch-fn module) msg)
                  (catch Exception e
                    (log/error e "Error in :all module" (:name module))))))

          ;; Step 2: Check if any :all module claimed this command
          claimed? (boolean
                    (some #(command-matches? msg-cmd (:commands %))
                          all))

          ;; Step 3: Dispatch to :commands modules if command matches
          commands-claimed? (atom false)
          _ (doseq [module commands]
              (when (and (command-matches? msg-cmd (:commands module))
                        (should-dispatch-to-module? msg module))
                (try
                  ((:dispatch-fn module) msg)
                  (reset! commands-claimed? true)
                  (catch Exception e
                    (log/error e "Error in :commands module" (:name module))))))

          ;; Step 4: If not claimed, dispatch to :unclaimed modules
          _ (when-not (or claimed? @commands-claimed?)
              (doseq [module unclaimed]
                (when (should-dispatch-to-module? msg module)
                  (try
                    ((:dispatch-fn module) msg)
                    (catch Exception e
                      (log/error e "Error in :unclaimed module" (:name module)))))))]
      nil)))

(defn- dispatcher-loop
  "Main dispatcher loop - processes messages from incoming channel.
   Runs in a go-loop for lightweight async processing."
  []
  (go-loop []
    (let [[msg ch] (async/alts! [channels/incoming-chan control-chan])]
      (cond
        ;; Control message received - stop
        (= ch control-chan)
        (do
          (log/info "Dispatcher stopping...")
          :stopped)

        ;; nil message means channel closed
        (nil? msg)
        (do
          (log/warn "Incoming channel closed, dispatcher stopping")
          :stopped)

        ;; Normal message - dispatch it
        :else
        (do
          (try
            (dispatch-message msg)
            (catch Exception e
              (log/error e "Error dispatching message")))
          (recur))))))

(defn start!
  "Start the dispatcher. Call once at startup.
   Reads from channels/incoming-chan and dispatches to modules."
  []
  (log/info "Starting Clojure dispatcher...")

  ;; Start dispatcher loop
  (reset! dispatcher-loop-chan (dispatcher-loop))

  (log/info "Clojure dispatcher started"))

(defn stop!
  "Stop the dispatcher gracefully.
   Closes control channel to signal shutdown."
  []
  (log/info "Stopping Clojure dispatcher...")
  (async/put! control-chan :stop)
  (close! control-chan)
  (log/info "Clojure dispatcher stopped"))

(defn dispatcher-status
  "Get the current dispatcher status.

   Returns a map with:
   - :running? - Boolean indicating if dispatcher loop is active
   - :channel-stats - Channel buffer statistics"
  []
  {:running? (some? @dispatcher-loop-chan)
   :channel-stats (channels/channel-stats)})

(comment
  ;; Start dispatcher
  (start!)

  ;; Check status
  (dispatcher-status)
  ;; => {:running? true, :channel-stats {...}}

  ;; Stop dispatcher
  (stop!)

  ;; Test message dispatch
  (require '[org.goat.core.message-parse :as mp])
  (def test-msg (mp/create-message :chat-id 123
                                   :sender "alice"
                                   :private? false
                                   :text "wordle 5"))

  ;; Put message on incoming channel (simulates Telegram)
  (channels/put-incoming! test-msg)

  ;; Check registered modules
  (require '[org.goat.core.registry :as registry])
  (registry/get-modules)
  )
