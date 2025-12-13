(ns org.goat.core.message-parse
  "Pure functions for parsing message text and creating message maps.

   This namespace extracts the parsing logic from Message.java constructor
   and provides clean, functional APIs for message creation."
  (:require [clojure.string :as str]
            [org.goat.core.message-spec :as spec])
  (:import [java.util UUID]))

;; =============================================================================
;; Command Parsing
;; =============================================================================

(defn parse-command
  "Parse text to extract command and arguments.

   Handles two patterns:
   1. Direct addressing: 'goat, command args' or 'goat command args'
   2. Regular command: 'command args'

   Returns a map with:
   - :command - keyword version of command (e.g., :wordle)
   - :command-args - text after the command
   - :directly-addressed? - true if bot was directly addressed

   Example:
   (parse-command 'goat, wordle 5' 'goat')
   => {:command :wordle
       :command-args '5'
       :directly-addressed? true}

   (parse-command 'stats' 'goat')
   => {:command :stats
       :command-args ''
       :directly-addressed? false}"
  [text bot-name]
  (if (or (nil? text) (str/blank? text))
    {:command nil
     :command-args ""
     :directly-addressed? false}
    (let [tokens (str/split (str/trim text) #"\s+")
          first-word (first tokens)
          bot-name-lower (str/lower-case bot-name)
          first-word-lower (str/lower-case first-word)

          ;; Check if first word is bot name with punctuation (e.g., "goat," or "goat!")
          ;; Pattern: bot name followed by non-word characters
          ;; Must NOT match bot name + word chars (to exclude "goats" etc.)
          directly-addressed? (boolean
                               (and (not (re-matches (re-pattern (str bot-name-lower "\\w+")) first-word-lower))
                                    (re-matches (re-pattern (str bot-name-lower "\\W*")) first-word-lower)))

          ;; If directly addressed, command is second word; otherwise first word
          command-word (if directly-addressed?
                        (second tokens)
                        first-word)

          ;; Command args are everything after the command word
          command-args-start (if directly-addressed? 2 1)
          command-args (str/join " " (drop command-args-start tokens))]

      {:command (when command-word (keyword (str/lower-case command-word)))
       :command-args (str/trim command-args)
       :directly-addressed? directly-addressed?})))

;; =============================================================================
;; Message Creation
;; =============================================================================

(defn create-message
  "Create a message map from parameters.

   Required keyword arguments:
   - :chat-id - Chat/channel ID (Long)
   - :sender - Username who sent the message
   - :private? - True if DM, false if group/channel

   Optional keyword arguments:
   - :text - Message text content
   - :chatname - Name of chat/channel
   - :authorized? - True if from bot owner (default false)
   - :bot-name - Bot name for parsing (default 'goat')
   - :image - RenderedImage attachment
   - :image-bytes - Image as byte array
   - :document-bytes - Document as byte array
   - :document-filename - Document filename
   - :platform-type - Platform type keyword (default :telegram)
   - :platform-data - Platform-specific metadata map
   - :timestamp - Override timestamp (default current time)
   - :id - Override message ID (default random UUID)

   Automatically parses :text to extract :command and :command-args.

   Example:
   (create-message :chat-id 123
                   :sender 'alice'
                   :private? false
                   :text 'goat, wordle 5 hard')
   => {:message/id <uuid>
       :message/chat-id 123
       :message/sender 'alice'
       :message/private? false
       :message/text 'goat, wordle 5 hard'
       :message/command :wordle
       :message/command-args '5 hard'
       :message/directly-addressed? true
       :message/authorized? false
       :message/timestamp <millis>
       ...}"
  [& {:keys [chat-id sender private? text chatname authorized? bot-name
             image image-bytes document-bytes document-filename
             platform-type platform-data timestamp id]
      :or {authorized? false
           bot-name "goat"
           platform-type :telegram
           timestamp (System/currentTimeMillis)
           id (UUID/randomUUID)}}]

  (let [;; Parse command from text
        parsed (when text (parse-command text bot-name))

        ;; Determine attachment types
        attachment-types (cond-> #{}
                          text (conj :text)
                          (or image image-bytes) (conj :image)
                          document-bytes (conj :document))

        ;; Build base message
        base-msg {:message/id id
                  :message/chat-id chat-id
                  :message/sender sender
                  :message/private? private?
                  :message/authorized? authorized?
                  :message/timestamp timestamp
                  :platform/type platform-type}]

    ;; Add optional fields
    (cond-> base-msg
      ;; Text and parsed command
      text (assoc :message/text text)
      (:command parsed) (assoc :message/command (:command parsed))
      (:command-args parsed) (assoc :message/command-args (:command-args parsed))
      parsed (assoc :message/directly-addressed? (boolean (:directly-addressed? parsed)))

      ;; Chat metadata
      chatname (assoc :message/chatname chatname)

      ;; Attachments
      (seq attachment-types) (assoc :message.attachment/type attachment-types)
      image (assoc :message.attachment/image image)
      image-bytes (assoc :message.attachment/image-bytes image-bytes)
      document-bytes (assoc :message.attachment/document-bytes document-bytes)
      document-filename (assoc :message.attachment/document-filename document-filename)

      ;; Platform data
      platform-data (assoc :platform/telegram platform-data))))

(defn create-reply
  "Create a reply message based on an existing message.

   Takes a source message and creates a new outgoing message with the same
   chat-id, is-private, etc. for proper reply context.

   Required arguments:
   - source-msg - The message being replied to

   Optional keyword arguments (at least one required):
   - :text - Reply text
   - :image - Reply image (RenderedImage)
   - :image-bytes - Reply image as bytes
   - :document-bytes - Reply document bytes
   - :document-filename - Reply document filename

   Example:
   (create-reply incoming-msg :text 'Hello, alice!')
   => {:message/id <uuid>
       :message/chat-id 123  ; from source
       :message/text 'Hello, alice!'
       :reply/context {:chat-id 123, :is-private false}
       ...}"
  [source-msg & {:keys [text image image-bytes document-bytes document-filename]}]

  (let [chat-id (:message/chat-id source-msg)
        is-private (:message/private? source-msg)
        sender (:message/sender source-msg)

        ;; Determine attachment types
        attachment-types (cond-> #{}
                          text (conj :text)
                          (or image image-bytes) (conj :image)
                          document-bytes (conj :document))

        ;; Build reply message
        reply-msg {:message/id (UUID/randomUUID)
                   :message/chat-id chat-id
                   :message/timestamp (System/currentTimeMillis)
                   :platform/type (:platform/type source-msg :telegram)
                   :reply/context {:chat-id chat-id
                                   :is-private is-private
                                   :sender sender}}]

    ;; Add content
    (cond-> reply-msg
      text (assoc :message/text text)
      (seq attachment-types) (assoc :message.attachment/type attachment-types)
      image (assoc :message.attachment/image image)
      image-bytes (assoc :message.attachment/image-bytes image-bytes)
      document-bytes (assoc :message.attachment/document-bytes document-bytes)
      document-filename (assoc :message.attachment/document-filename document-filename))))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn has-text?
  "Returns true if message has text content."
  [msg]
  (boolean (:message/text msg)))

(defn has-image?
  "Returns true if message has an image attachment."
  [msg]
  (contains? (:message.attachment/type msg) :image))

(defn has-document?
  "Returns true if message has a document attachment."
  [msg]
  (contains? (:message.attachment/type msg) :document))

(defn command-matches?
  "Returns true if message command matches any of the given commands.

   Example:
   (command-matches? msg :wordle :stats :help)"
  [msg & commands]
  (contains? (set commands) (:message/command msg)))

;; =============================================================================
;; Examples and Testing
;; =============================================================================

(comment
  ;; Parse command examples
  (parse-command "wordle 5" "goat")
  ;; => {:command :wordle, :command-args "5", :directly-addressed? false}

  (parse-command "goat, wordle 5 hard" "goat")
  ;; => {:command :wordle, :command-args "5 hard", :directly-addressed? true}

  (parse-command "goat wordle 5" "goat")
  ;; => {:command :wordle, :command-args "5", :directly-addressed? true}

  (parse-command "stats" "goat")
  ;; => {:command :stats, :command-args "", :directly-addressed? false}

  ;; Create message examples
  (create-message :chat-id 123
                  :sender "alice"
                  :private? false
                  :text "wordle 5")

  (create-message :chat-id 456
                  :sender "bob"
                  :private? true
                  :text "goat, help me"
                  :authorized? true)

  ;; Create reply examples
  (def incoming (create-message :chat-id 123 :sender "alice" :private? false :text "hello"))
  (create-reply incoming :text "Hi alice!")

  ;; Validate with spec
  (require '[org.goat.core.message-spec :as spec])
  (spec/valid-incoming? (create-message :chat-id 123 :sender "test" :private? false))
  )
