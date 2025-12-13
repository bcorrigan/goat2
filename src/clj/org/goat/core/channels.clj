(ns org.goat.core.channels
  "core.async channels for message flow.

   Replaces the Java LinkedBlockingQueue with idiomatic Clojure channels.
   - incoming-chan: Messages FROM Telegram (or other platforms) TO bot
   - outgoing-chan: Messages FROM bot TO Telegram (or other platforms)"
  (:require [clojure.core.async :as async]
            [clojure.tools.logging :as log]))

;; =============================================================================
;; Channel Definitions
;; =============================================================================

;; Channel for incoming messages from Telegram.
;; ServerConnection's InputHandler puts messages here after parsing.
;; The dispatcher takes from this channel and routes to modules.
;; Buffer size: 100 messages
(defonce incoming-chan (async/chan 100))

;; Channel for outgoing messages to Telegram.
;; Modules put reply messages here via message/send-msg.
;; ServerConnection's OutputHandler takes from this channel and sends via Platform.
;; Buffer size: 100 messages
(defonce outgoing-chan (async/chan 100))

;; =============================================================================
;; Channel Operations
;; =============================================================================

(defn put-incoming!
  "Put an incoming message on the incoming channel.

   Blocks if channel buffer is full.
   Returns true if message was accepted, false if channel is closed.

   Example:
   (put-incoming! message-map)"
  [msg]
  (async/>!! incoming-chan msg))

(defn take-incoming!
  "Take an incoming message from the channel.

   Blocks until a message is available or channel is closed.
   Returns the message or nil if channel is closed.

   Example:
   (let [msg (take-incoming!)]
     (process-message msg))"
  []
  (async/<!! incoming-chan))

(defn put-outgoing!
  "Put an outgoing message on the outgoing channel.

   Blocks if channel buffer is full.
   Returns true if message was accepted, false if channel is closed.

   Example:
   (put-outgoing! reply-message)"
  [msg]
  (async/>!! outgoing-chan msg))

(defn take-outgoing!
  "Take an outgoing message from the channel.

   Blocks until a message is available or channel is closed.
   Returns the message or nil if channel is closed.

   Example:
   (let [msg (take-outgoing!)]
     (send-to-platform msg))"
  []
  (async/<!! outgoing-chan))

;; =============================================================================
;; Channel Lifecycle
;; =============================================================================

(defn close-channels!
  "Close both incoming and outgoing channels.

   This will cause all blocking takes to return nil and puts to return false.
   Use this for graceful shutdown."
  []
  (log/info "Closing message channels")
  (async/close! incoming-chan)
  (async/close! outgoing-chan))

(defn channel-stats
  "Get statistics about channel buffers.

   Returns a map with:
   - :incoming-buffer - Approximate number of messages in incoming buffer
   - :outgoing-buffer - Approximate number of messages in outgoing buffer

   Note: These are estimates and may not be exact due to concurrency."
  []
  {:incoming-buffer (count incoming-chan)
   :outgoing-buffer (count outgoing-chan)})

;; =============================================================================
;; Async Operations (for use in go blocks)
;; =============================================================================

(defn put-incoming-async!
  "Put an incoming message on the channel asynchronously.

   For use in go blocks. Non-blocking within a go block.

   Example:
   (go
     (put-incoming-async! msg))"
  [msg]
  (async/>! incoming-chan msg))

(defn take-incoming-async!
  "Take an incoming message from the channel asynchronously.

   For use in go blocks. Non-blocking within a go block.

   Example:
   (go
     (let [msg (take-incoming-async!)]
       (process msg)))"
  []
  (async/<! incoming-chan))

(defn put-outgoing-async!
  "Put an outgoing message on the channel asynchronously.

   For use in go blocks. Non-blocking within a go block.

   Example:
   (go
     (put-outgoing-async! reply-msg))"
  [msg]
  (async/>! outgoing-chan msg))

(defn take-outgoing-async!
  "Take an outgoing message from the channel asynchronously.

   For use in go blocks. Non-blocking within a go block.

   Example:
   (go
     (let [msg (take-outgoing-async!)]
       (send-to-platform msg)))"
  []
  (async/<! outgoing-chan))

;; =============================================================================
;; Monitoring and Debugging
;; =============================================================================

(defn tap-incoming
  "Tap the incoming channel for monitoring.

   Creates a new channel that receives copies of all incoming messages.
   Useful for logging, debugging, or metrics.

   Example:
   (def monitor-chan (async/chan 10))
   (tap-incoming monitor-chan)
   (go-loop []
     (when-let [msg (<! monitor-chan)]
       (log/info 'Incoming message:' msg)
       (recur)))"
  [tap-chan]
  (async/tap (async/mult incoming-chan) tap-chan))

(defn tap-outgoing
  "Tap the outgoing channel for monitoring.

   Creates a new channel that receives copies of all outgoing messages.
   Useful for logging, debugging, or metrics.

   Example:
   (def monitor-chan (async/chan 10))
   (tap-outgoing monitor-chan)
   (go-loop []
     (when-let [msg (<! monitor-chan)]
       (log/info 'Outgoing message:' msg)
       (recur)))"
  [tap-chan]
  (async/tap (async/mult outgoing-chan) tap-chan))

;; =============================================================================
;; Examples and Testing
;; =============================================================================

(comment
  ;; Put and take messages (blocking)
  (require '[org.goat.core.message-parse :as mp])

  (def test-msg (mp/create-message :chat-id 123 :sender "alice" :private? false :text "hello"))

  ;; Put incoming message
  (put-incoming! test-msg)

  ;; Take incoming message (in another thread/REPL)
  (take-incoming!)

  ;; Put outgoing message
  (put-outgoing! (mp/create-reply test-msg :text "hi alice!"))

  ;; Take outgoing message
  (take-outgoing!)

  ;; Check channel stats
  (channel-stats)
  ;; => {:incoming-buffer 0, :outgoing-buffer 0}

  ;; Async operations in go blocks
  (require '[clojure.core.async :refer [go go-loop <! >!]])

  (go
    (put-incoming-async! test-msg)
    (println "Put message asynchronously"))

  (go
    (let [msg (take-incoming-async!)]
      (println "Got message:" msg)))

  ;; Tap for monitoring
  (def monitor-chan (async/chan 10))
  (tap-incoming monitor-chan)

  (go-loop []
    (when-let [msg (<! monitor-chan)]
      (println "MONITOR:" (:message/text msg))
      (recur)))

  ;; Close channels (shutdown)
  (close-channels!)
  )
