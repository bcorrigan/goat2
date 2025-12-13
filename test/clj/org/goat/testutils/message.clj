(ns org.goat.testutils.message
  "Message mocking utility for unit tests.

   Provides comprehensive mocking functionality for map-based messages,
   supporting all common usage patterns across modules."
  (:require [org.goat.core.message-parse :as msg-parse]
            [org.goat.core.channels :as channels]
            [clojure.string :as str])
  (:import (java.awt.image BufferedImage)
           (java.awt Graphics2D Color)))

;; =============================================================================
;; Reply Capture
;; =============================================================================

(def ^:private reply-log
  "Atom to capture all replies sent during testing"
  (atom []))

(defn get-replies
  "Get all replies sent by mock messages during testing.
   Returns vector of message maps."
  []
  @reply-log)

(defn clear-replies!
  "Clear the reply log for fresh test isolation."
  []
  (reset! reply-log []))

;; =============================================================================
;; Mock Message Creation
;; =============================================================================

(defn- create-mock-image
  "Creates a simple test image for mocking image replies"
  []
  (let [img (BufferedImage. 100 50 BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 100 50)
    (.setColor g Color/BLACK)
    (.drawString g "Test Image" 10 25)
    (.dispose g)
    img))

(defn mock-message
  "Creates a mock message map for testing.

   Options map supports:
   - :text              - The message text (default: 'test message')
   - :chat-id           - Chat ID as Long (default: 123)
   - :sender            - Who sent the message (default: 'test-user')
   - :chatname          - Name of chat room (default: 'test-chat')
   - :private?          - Whether message is private/1-1 (default: false)
   - :authorized?       - Whether from bot owner (default: false)
   - :document-bytes    - Byte array for document content (default: nil)
   - :document-filename - Filename for document (default: nil)
   - :bot-name          - Bot name for parsing (default: 'goat')

   Returns a message map that can be used with all message protocol functions.

   Example usage:
   (mock-message {:text 'wordle 5 hard' :sender 'alice' :chat-id 456})
   (mock-message {:text 'Check out https://example.com'})
   (mock-message {:sender 'bob' :private? true})
   (mock-message {:document-bytes csv-bytes :document-filename 'data.csv'})"

  ([] (mock-message {}))
  ([{:keys [text chat-id sender chatname private? authorized?
            document-bytes document-filename bot-name]
     :or {text "test message"
          chat-id 123
          sender "test-user"
          chatname "test-chat"
          private? false
          authorized? false
          bot-name "goat"}}]

   ;; Use msg-parse/create-message to build the message map
   ;; This ensures proper parsing of commands, etc.
   (msg-parse/create-message
    :chat-id chat-id
    :sender sender
    :private? private?
    :text text
    :chatname chatname
    :authorized? authorized?
    :bot-name bot-name
    :document-bytes document-bytes
    :document-filename document-filename)))

(defn mock-command-message
  "Convenience function to create a message with a bot command.

   Creates a message like 'goat command args' that would trigger module processing.

   Args:
   - command: The command name (e.g. 'wordle', 'stats')
   - args: Additional arguments (optional, can be string or collection)
   - opts: Additional options map (same as mock-message)

   Examples:
   (mock-command-message 'wordle' '5 hard')
   (mock-command-message 'stats' nil {:sender 'alice'})
   (mock-command-message 'wordle' ['challenge' 'bob'] {:chat-id 999})"

  ([command] (mock-command-message command nil {}))
  ([command args] (mock-command-message command args {}))
  ([command args opts]
   (let [bot-name (or (:bot-name opts) "goat")
         args-str (cond
                    (nil? args) ""
                    (string? args) args
                    (coll? args) (str/join " " args)
                    :else (str args))
         full-text (str bot-name " " command
                       (when (seq args-str) (str " " args-str)))]
     (mock-message (assoc opts :text full-text)))))

(defn mock-guess-message
  "Convenience function for wordle-style guess messages.

   Creates a message with just a word guess (no bot prefix).

   Example: (mock-guess-message 'HOUSE' {:sender 'alice'})"
  ([guess] (mock-guess-message guess {}))
  ([guess opts]
   (mock-message (assoc opts :text (str guess)))))

(defn mock-private-message
  "Convenience function for private/DM messages."
  ([text] (mock-private-message text {}))
  ([text opts]
   (mock-message (assoc opts :text text :private? true))))

;; =============================================================================
;; Reply Capture Mechanism
;; =============================================================================

(defn capture-outgoing-message!
  "Capture an outgoing message instead of sending it.
   Used by with-clean-replies to intercept replies during testing.
   Note: Must be public for with-clean-replies macro expansion."
  [msg]
  (swap! reply-log conj msg)
  true)

(defmacro with-clean-replies
  "Execute body with reply capture enabled.

   Automatically intercepts all outgoing messages (replies) and captures them
   for testing assertions. Clears reply log before and after execution.

   Example:
   (with-clean-replies
     (let [msg (mock-message {:text 'wordle 5'})]
       (process-message msg))
     (is (replied-with? 'game started'))
     (is (= 1 (reply-count))))"
  [& body]
  `(do
     (clear-replies!)
     (try
       (with-redefs [channels/put-outgoing! capture-outgoing-message!]
         ~@body)
       (finally
         (clear-replies!)))))

;; =============================================================================
;; Reply Assertion Utilities
;; =============================================================================

(defn replied-with?
  "Check if any reply contains the given text (case-insensitive substring match)."
  [text]
  (some #(and (:message/text %)
              (str/includes?
               (str/lower-case (:message/text %))
               (str/lower-case text)))
        (get-replies)))

(defn replied-with-image?
  "Check if any image reply was sent."
  []
  (some #(:message.attachment/image %) (get-replies)))

(defn replied-with-document?
  "Check if any document reply was sent."
  []
  (some #(:message.attachment/document-bytes %) (get-replies)))

(defn reply-count
  "Count total number of replies sent."
  []
  (count (get-replies)))

(defn text-reply-count
  "Count number of text replies sent."
  []
  (count (filter :message/text (get-replies))))

(defn image-reply-count
  "Count number of image replies sent."
  []
  (count (filter :message.attachment/image (get-replies))))

(defn document-reply-count
  "Count number of document replies sent."
  []
  (count (filter :message.attachment/document-bytes (get-replies))))

(defn get-document-reply
  "Get the first document reply, or nil if none."
  []
  (first (filter :message.attachment/document-bytes (get-replies))))

(defn get-document-content
  "Get the content (byte array) of the first document reply, or nil."
  []
  (:message.attachment/document-bytes (get-document-reply)))

(defn get-document-filename
  "Get the filename of the first document reply, or nil."
  []
  (:message.attachment/document-filename (get-document-reply)))

(defn get-text-replies
  "Get all text reply contents as a vector of strings."
  []
  (mapv :message/text
        (filter :message/text (get-replies))))

(defn get-first-reply-text
  "Get the text of the first reply, or nil if no replies."
  []
  (:message/text (first (get-replies))))

(defn get-last-reply-text
  "Get the text of the last reply, or nil if no replies."
  []
  (:message/text (last (get-replies))))

;; =============================================================================
;; Advanced Assertions
;; =============================================================================

(defn replied-exactly?
  "Check if exactly one reply was sent with the given text (exact match, case-sensitive)."
  [text]
  (and (= 1 (reply-count))
       (= text (:message/text (first (get-replies))))))

(defn replied-to-chat?
  "Check if any reply was sent to the specified chat-id."
  [chat-id]
  (some #(= chat-id (:message/chat-id %)) (get-replies)))

(defn reply-contains-all?
  "Check if any single reply contains all the given substrings."
  [& substrings]
  (some (fn [reply]
          (when-let [text (:message/text reply)]
            (let [lower-text (str/lower-case text)]
              (every? #(str/includes? lower-text (str/lower-case %))
                      substrings))))
        (get-replies)))

;; =============================================================================
;; Examples and Testing
;; =============================================================================

(comment
  ;; Create mock messages
  (def msg1 (mock-message {:text "hello world" :sender "alice"}))
  (def msg2 (mock-command-message "wordle" "5 hard" {:sender "bob"}))
  (def msg3 (mock-private-message "secret message" {:sender "charlie"}))

  ;; Test with reply capture
  (with-clean-replies
    (let [msg (mock-message {:text "test"})]
      (require '[org.goat.core.message :as m])
      (m/reply msg "This is a reply")
      (m/reply msg "Another reply"))

    (replied-with? "reply")        ;; => true
    (reply-count)                  ;; => 2
    (get-text-replies)             ;; => ["This is a reply" "Another reply"]
    (get-first-reply-text))        ;; => "This is a reply"
  )
