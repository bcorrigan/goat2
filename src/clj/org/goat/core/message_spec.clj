(ns org.goat.core.message-spec
  "Clojure spec definitions for message maps.

   Messages are represented as plain Clojure maps with namespaced keywords.
   This namespace provides comprehensive specs for validation at boundaries
   (Telegram receive, tests) and documentation of the message structure."
  (:require [clojure.spec.alpha :as s])
  (:import [java.awt.image RenderedImage]))

;; =============================================================================
;; Core Message Fields
;; =============================================================================

(s/def :message/id uuid?)

(s/def :message/chat-id int?)

(s/def :message/sender string?)

(s/def :message/chatname string?)

(s/def :message/text string?)

(s/def :message/command keyword?)

(s/def :message/command-args string?)

(s/def :message/private? boolean?)

(s/def :message/directly-addressed? boolean?)

(s/def :message/authorized? boolean?)

(s/def :message/timestamp pos-int?)

;; =============================================================================
;; Attachment Fields
;; =============================================================================

(s/def :message.attachment/type
  (s/coll-of #{:image :document :text} :kind set :min-count 1))

(s/def :message.attachment/image
  #(instance? RenderedImage %))

(s/def :message.attachment/image-bytes bytes?)

(s/def :message.attachment/document-bytes bytes?)

(s/def :message.attachment/document-filename string?)

;; =============================================================================
;; Platform Fields
;; =============================================================================

(s/def :platform/type keyword?)

(s/def :platform/telegram map?)

;; =============================================================================
;; Reply Context
;; =============================================================================

(s/def :reply.context/chat-id int?)

(s/def :reply.context/is-private boolean?)

(s/def :reply.context/sender string?)

(s/def :reply/context
  (s/keys :req-un [:reply.context/chat-id
                   :reply.context/is-private]
          :opt-un [:reply.context/sender]))

;; =============================================================================
;; Composite Message Specs
;; =============================================================================

;; Spec for incoming messages from Telegram or other platforms.
;;
;; Required fields:
;; - :message/id - Unique message identifier
;; - :message/chat-id - Chat/channel ID
;; - :message/sender - Username who sent the message
;; - :message/private? - True if DM, false if group/channel
;; - :message/timestamp - Unix timestamp in milliseconds
;;
;; Optional fields:
;; - :message/text - Message text content
;; - :message/chatname - Name of the chat/channel
;; - :message/command - Parsed command keyword (e.g., :wordle)
;; - :message/command-args - Text after the command
;; - :message/directly-addressed? - True if 'goat, command' pattern
;; - :message/authorized? - True if from bot owner
;; - Attachment fields (image, document)
;; - Platform-specific metadata
(s/def :message/incoming
  (s/keys :req [:message/id
                :message/chat-id
                :message/sender
                :message/private?
                :message/timestamp]
          :opt [:message/text
                :message/chatname
                :message/command
                :message/command-args
                :message/directly-addressed?
                :message/authorized?
                :message.attachment/type
                :message.attachment/image
                :message.attachment/image-bytes
                :message.attachment/document-bytes
                :message.attachment/document-filename
                :platform/type
                :platform/telegram]))

;; Spec for outgoing messages to be sent via platform.
;;
;; Required fields:
;; - :message/chat-id - Destination chat ID
;; - :reply/context - Context for constructing the reply
;;
;; Optional fields:
;; - :message/text - Text to send
;; - Attachment fields (image, document)
;; - Platform-specific options
(s/def :message/outgoing
  (s/keys :req [:message/chat-id
                :reply/context]
          :opt [:message/text
                :message.attachment/type
                :message.attachment/image
                :message.attachment/image-bytes
                :message.attachment/document-bytes
                :message.attachment/document-filename
                :platform/type
                :platform/telegram]))

;; Spec for any message (incoming or outgoing).
(s/def :message/any
  (s/or :incoming :message/incoming
        :outgoing :message/outgoing))

;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defn valid-incoming?
  "Returns true if the message is a valid incoming message."
  [msg]
  (s/valid? :message/incoming msg))

(defn valid-outgoing?
  "Returns true if the message is a valid outgoing message."
  [msg]
  (s/valid? :message/outgoing msg))

(defn explain-incoming
  "Explains why an incoming message is invalid."
  [msg]
  (s/explain-data :message/incoming msg))

(defn explain-outgoing
  "Explains why an outgoing message is invalid."
  [msg]
  (s/explain-data :message/outgoing msg))

(defn validate-incoming!
  "Validates an incoming message, throwing an exception if invalid.
   Use at boundaries (Telegram receive) to catch malformed messages early."
  [msg]
  (when-not (valid-incoming? msg)
    (throw (ex-info "Invalid incoming message"
                    {:message msg
                     :explanation (explain-incoming msg)})))
  msg)

(defn validate-outgoing!
  "Validates an outgoing message, throwing an exception if invalid.
   Use in tests to ensure messages are well-formed before sending."
  [msg]
  (when-not (valid-outgoing? msg)
    (throw (ex-info "Invalid outgoing message"
                    {:message msg
                     :explanation (explain-outgoing msg)})))
  msg)

;; =============================================================================
;; Generative Testing Support
;; =============================================================================

(comment
  ;; Generate sample incoming messages for testing
  (require '[clojure.spec.gen.alpha :as gen])

  ;; Generate a single incoming message
  (gen/generate (s/gen :message/incoming))

  ;; Generate 5 incoming messages
  (gen/sample (s/gen :message/incoming) 5)

  ;; Generate outgoing messages
  (gen/generate (s/gen :message/outgoing))

  ;; Validate a message
  (valid-incoming? {:message/id (java.util.UUID/randomUUID)
                    :message/chat-id 123
                    :message/sender "alice"
                    :message/private? false
                    :message/timestamp (System/currentTimeMillis)})

  ;; Explain validation failure
  (explain-incoming {:message/chat-id "not-an-int"})
  )
