(ns org.goat.core.connection
  "Telegram connection management using core.async channels.

   Manages InputHandler (Telegram → bot) and OutputHandler (bot → Telegram)."
  (:require [clojure.core.async :as async]
            [clojure.tools.logging :as log]
            [org.goat.core.channels :as channels]
            [org.goat.core.platform :as platform]
            [org.goat.core.message-parse :as msg-parse]
            [org.goat.core.message-spec :as msg-spec]
            [org.goat.core.config :as config])
  (:import [org.telegram.telegrambots.longpolling TelegramBotsLongPollingApplication]
           [org.telegram.telegrambots.longpolling.util LongPollingSingleThreadUpdateConsumer]
           [org.telegram.telegrambots.meta.api.objects Update Document]
           [org.telegram.telegrambots.client.okhttp OkHttpTelegramClient]))

;; Atom holding the current connection state.
(defonce connection-state
  (atom {:running? false
         :platform nil
         :input-handler nil
         :output-handler nil
         :polling-app nil}))

(defn- extract-telegram-message
  "Extract the Telegram message from an Update.
   Handles edited messages, regular messages, and channel posts."
  [^Update update]
  (cond
    (.hasEditedMessage update) (.getEditedMessage update)
    (.hasMessage update) (.getMessage update)
    (.hasChannelPost update) (.getChannelPost update)
    :else nil))

(defn- telegram-message->map
  "Convert a Telegram message to a Clojure message map.

   Downloads documents if present using the platform."
  [telegram-msg platform-inst]
  (let [chat-id (.getChatId telegram-msg)
        sender (if-let [from (.getFrom telegram-msg)]
                (.getFirstName from)
                "unknown")
        text (.getText telegram-msg)
        chat (if-let [c (.getChat telegram-msg)]
              (.getTitle c)
              nil)
        is-private (if-let [c (.getChat telegram-msg)]
                    (= "private" (.getType c))
                    false)

        ;; Create base message
        base-msg (msg-parse/create-message
                  :chat-id chat-id
                  :sender sender
                  :private? is-private
                  :text text
                  :chatname chat
                  :platform-data {:raw-update telegram-msg})]

    ;; Add document if present
    (if (.hasDocument telegram-msg)
      (let [^Document doc (.getDocument telegram-msg)
            file-name (.getFileName doc)
            file-id (.getFileId doc)
            doc-bytes (platform/download-document platform-inst file-id)]
        (if doc-bytes
          (assoc base-msg
                 :message.attachment/document-bytes doc-bytes
                 :message.attachment/document-filename file-name
                 :message.attachment/type (conj (:message.attachment/type base-msg #{}) :document))
          base-msg))
      base-msg)))

(defn- process-single-update
  "Process a single Telegram Update into a message map and put on channel."
  [update platform-inst debug?]
  (when-let [telegram-msg (extract-telegram-message update)]
    (let [msg-map (telegram-message->map telegram-msg platform-inst)]
      ;; Validate message before putting on channel
      (if (msg-spec/valid-incoming? msg-map)
        (do
          (when debug?
            (log/info "Incoming message:" (:message/text msg-map)))
          (channels/put-incoming! msg-map))
        (log/warn "Invalid incoming message, dropping:"
                 (msg-spec/explain-incoming msg-map))))))

(defn- create-input-consumer
  "Create a LongPollingSingleThreadUpdateConsumer for Telegram updates.

   Implements both consume(Update) and consume(List<Update>) methods."
  [platform-inst debug?]
  (proxy [LongPollingSingleThreadUpdateConsumer] []
    (consume [updates]
      (try
        (cond
          ;; Single Update
          (instance? Update updates)
          (process-single-update updates platform-inst debug?)

          ;; List of Updates
          (instance? java.util.List updates)
          (doseq [update updates]
            (process-single-update update platform-inst debug?))

          ;; Unknown type
          :else
          (log/warn "Unknown type received in consume:" (type updates)))
        (catch Exception e
          (log/error e "Exception in consume")
          (.printStackTrace e))))))

(defn start-input-handler
  "Start the InputHandler thread for long-polling Telegram.

   Returns a future that represents the running handler."
  [platform-inst debug?]
  (future
    (try
      (let [token (config/get-token)
            consumer (create-input-consumer platform-inst debug?)
            polling-app (TelegramBotsLongPollingApplication.)]

        ;; Register bot and start long-polling
        (.registerBot polling-app token consumer)
        (log/info "InputHandler started, long-polling Telegram")

        ;; Store polling app for cleanup
        (swap! connection-state assoc :polling-app polling-app)

        ;; Keep thread alive
        (Thread/sleep Long/MAX_VALUE))

      (catch InterruptedException e
        (log/info "InputHandler interrupted, shutting down"))

      (catch Exception e
        (log/error e "Fatal error in InputHandler")
        (.printStackTrace e)
        (System/exit 2)))))

(defn- start-cli-input-handler
  "Start CLI input handler that reads from stdin.
   Runs in a future (separate thread) for blocking I/O.

   Reads lines from System/in and creates message maps,
   putting them on incoming-chan for processing."
  [platform-inst debug?]
  (future
    (try
      (when debug?
        (log/debug "CLI InputHandler starting..."))

      (let [reader (java.io.BufferedReader. (java.io.InputStreamReader. System/in))]
        ;; Read loop
        (loop []
          (when (:running? @connection-state)
            (when-let [line (.readLine reader)]
              (when-not (clojure.string/blank? line)
                (try
                  ;; Create message from stdin
                  (let [msg (msg-parse/create-message
                              :chat-id 123456
                              :sender "CLIUser"
                              :private? true
                              :text line
                              :platform-type :cli)]
                    (when debug?
                      (log/debug "CLI input:" line))
                    (channels/put-incoming! msg))
                  (catch Exception e
                    (log/error e "Error creating message from CLI input"))))
              (recur))))

        (when debug?
          (log/debug "CLI InputHandler stopped")))

      (catch Exception e
        (log/error e "CLI InputHandler error")))))

(defn- send-message-via-platform
  "Send a message using the platform protocol.

   Handles text, image, and document messages appropriately."
  [platform-inst msg]
  (let [chat-id (:message/chat-id msg)]
    (cond
      ;; Image message
      (and (:message.attachment/image msg))
      (platform/send-image platform-inst chat-id
                          (:message.attachment/image msg)
                          {})

      ;; Document message
      (and (:message.attachment/document-bytes msg)
           (:message.attachment/document-filename msg))
      (platform/send-document platform-inst chat-id
                             (:message.attachment/document-bytes msg)
                             (:message.attachment/document-filename msg)
                             {})

      ;; Text message
      (:message/text msg)
      (platform/send-text platform-inst chat-id
                         (:message/text msg)
                         {:parse-mode :html})

      ;; Unknown message type
      :else
      (log/warn "Cannot send message: no text, image, or document" msg))))

(defn start-output-handler
  "Start the OutputHandler go-loop for sending messages to Telegram.

   Returns a go-loop channel that represents the running handler."
  [platform-inst]
  (async/go-loop []
    (if-let [msg (async/<! channels/outgoing-chan)]
      (do
        ;; Send via platform
        (try
          (send-message-via-platform platform-inst msg)
          (catch Exception e
            (log/error e "Error sending message to Telegram, sleeping 5 minutes")
            ;; Sleep on error (like Java version does)
            (async/<! (async/timeout 300000))))
        ;; Continue loop
        (recur))
      ;; Channel closed, exit loop
      (log/info "OutputHandler stopped: outgoing channel closed"))))

(defn start-connection
  "Start the Telegram connection.

   Creates and starts InputHandler and OutputHandler.
   Returns true if successfully started, false otherwise.

   Options:
   - :debug? - Enable debug logging (default false)"
  [& {:keys [debug?] :or {debug? false}}]
  (if (:running? @connection-state)
    (do
      (log/warn "Connection already running")
      false)
    (try
      (log/info "Starting Telegram connection...")

      ;; Create Telegram client and platform
      (let [token (config/get-token)
            telegram-client (OkHttpTelegramClient. token)
            telegram-platform (platform/create-telegram-platform telegram-client)]

        ;; Start handlers
        (let [input-future (start-input-handler telegram-platform debug?)
              output-chan (start-output-handler telegram-platform)]

          ;; Update state
          (swap! connection-state assoc
                 :running? true
                 :platform telegram-platform
                 :input-handler input-future
                 :output-handler output-chan)

          (log/info "Telegram connection started successfully")
          true))

      (catch Exception e
        (log/error e "Failed to start connection")
        (.printStackTrace e)
        false))))

(defn start-cli-connection
  "Start CLI connection for testing mode.

   Reads from stdin, processes through modules, writes to stdout.
   Uses the same channel infrastructure as Telegram connection.

   Options:
   - :debug? - Enable debug logging (default: false)"
  [& {:keys [debug?] :or {debug? false}}]
  (if (:running? @connection-state)
    (do
      (log/warn "Connection already running")
      false)
    (try
      (log/info "Starting CLI test mode connection...")

      ;; Create CLI platform (no capture, direct stdout)
      (let [cli-platform (platform/create-cli-platform :capture? false)]

        ;; Start handlers
        (let [input-future (start-cli-input-handler cli-platform debug?)
              output-chan (start-output-handler cli-platform)]

          ;; Update state
          (swap! connection-state assoc
                 :running? true
                 :platform cli-platform
                 :platform-type :cli
                 :input-handler input-future
                 :output-handler output-chan)

          (log/info "CLI connection started successfully")
          true))

      (catch Exception e
        (log/error e "Failed to start CLI connection")
        (.printStackTrace e)
        false))))

(defn stop-connection
  "Stop the active connection (Telegram or CLI).

   Gracefully shuts down InputHandler and OutputHandler."
  []
  (when (:running? @connection-state)
    (log/info "Stopping connection...")

    ;; Close channels (signals handlers to stop)
    (channels/close-channels!)

    ;; Close polling application
    (when-let [polling-app (:polling-app @connection-state)]
      (try
        (.close polling-app)
        (log/info "Polling application closed")
        (catch Exception e
          (log/warn e "Error closing polling application"))))

    ;; Cancel input handler future
    (when-let [input-future (:input-handler @connection-state)]
      (future-cancel input-future))

    ;; Reset state
    (reset! connection-state {:running? false
                              :platform nil
                              :input-handler nil
                              :output-handler nil
                              :polling-app nil})

    (log/info "Connection stopped")
    true))

(defn reconnect
  "Attempt to reconnect to Telegram with exponential backoff.

   Retries indefinitely until successful."
  [& {:keys [debug? max-retry-delay-ms]
      :or {debug? false
           max-retry-delay-ms 400000}}]
  (loop [retry-count 0
         delay-ms 1000]
    (log/info (format "Connection attempt #%d" (inc retry-count)))

    (if (start-connection :debug? debug?)
      (do
        (log/info "Reconnection successful")
        true)
      (do
        (log/warn (format "Connection failed, retrying in %d seconds..." (/ delay-ms 1000)))
        (Thread/sleep delay-ms)
        (recur (inc retry-count)
               (min max-retry-delay-ms (* delay-ms 2)))))))

(defn connection-status
  "Get the current connection status.

   Returns a map with:
   - :running? - Boolean
   - :platform-type - Keyword (:telegram, :cli, etc.)
   - :channel-stats - Map of channel buffer stats"
  []
  (let [state @connection-state]
    {:running? (:running? state)
     :platform-type (when (:platform state)
                     (platform/platform-name (:platform state)))
     :channel-stats (channels/channel-stats)}))

(comment
  ;; Start connection
  (start-connection :debug? true)

  ;; Check status
  (connection-status)
  ;; => {:running? true, :platform-type :telegram, :channel-stats {...}}

  ;; Stop connection
  (stop-connection)

  ;; Reconnect with retry
  (reconnect :debug? true)

  ;; Test with CLI platform (for development)
  (require '[org.goat.core.platform :as p])
  (def cli-platform (p/create-cli-platform))
  ;; Manually inject messages for testing
  )
