(ns org.goat.core.message
  "Clojurian wrapper and utilities for the Java Message class"
  (:import [org.goat.core Message]))

(defprotocol MessageContext
  "Protocol for interacting with messages in a Clojurian way"
  (reply [this text] "Send a reply to this message")
  (reply-image [this img] "Send an image reply to this message")
  (get-command [this] "Get the command as a keyword (e.g. :wordle)")
  (get-text [this] "Get the full text of the message")
  (get-mod-text [this] "Get the text after the command")
  (get-sender [this] "Get the sender of the message")
  (get-chat-id [this] "Get the chat ID")
  (get-chatname [this] "Get the chat name")
  (private? [this] "True if this is a private message")
  (has-text? [this] "True if this message has text")
  (has-image? [this] "True if this message has an image"))

(deftype MessageWrapper [^Message msg]
  MessageContext
  (reply [_ text]
    (.reply msg text))

  (reply-image [_ img]
    (.replyWithImage msg img))

  (get-command [_]
    (when-let [cmd (.getModCommand msg)]
      (keyword cmd)))

  (get-text [_]
    (.getText msg))

  (get-mod-text [_]
    (.getModText msg))

  (get-sender [_]
    (.getSender msg))

  (get-chat-id [_]
    (.getChatId msg))

  (get-chatname [_]
    (.getChatname msg))

  (private? [_]
    (.isPrivate msg))

  (has-text? [_]
    (.hasText msg))

  (has-image? [_]
    (.hasImage msg)))

(defn wrap-message
  "Wraps a Java Message object in a Clojurian MessageWrapper"
  [^Message msg]
  (MessageWrapper. msg))

;; Convenience functions that work with either wrapped or unwrapped messages
(defn unwrap
  "Extract the underlying Java Message from a wrapper, or return as-is if already unwrapped"
  [msg-or-wrapper]
  (if (instance? MessageWrapper msg-or-wrapper)
    (.-msg msg-or-wrapper)
    msg-or-wrapper))

(defn ensure-wrapped
  "Ensure we have a wrapped message, wrapping if necessary"
  [msg-or-wrapper]
  (if (instance? MessageWrapper msg-or-wrapper)
    msg-or-wrapper
    (wrap-message msg-or-wrapper)))

;; Additional helper functions for common patterns
(defn command-matches?
  "Check if the message command matches any of the given keywords"
  [msg-wrapper & commands]
  (contains? (set commands) (get-command msg-wrapper)))

(defn reply-when
  "Reply with text only if condition is true"
  [msg-wrapper condition text]
  (when condition
    (reply msg-wrapper text)))

(defn reply-format
  "Reply with formatted text (like format)"
  [msg-wrapper fmt & args]
  (reply msg-wrapper (apply format fmt args)))

(defn case-command
  "Convenience macro for command routing with keywords"
  [msg-wrapper & clauses]
  (let [cmd (get-command msg-wrapper)]
    (loop [remaining clauses]
      (cond
        (empty? remaining) nil
        (= (first remaining) cmd) ((second remaining))
        :else (recur (drop 2 remaining))))))

;; For working with multiple replies in sequence
(defn reply-all
  "Send multiple replies in sequence"
  [msg-wrapper & texts]
  (doseq [text texts]
    (reply msg-wrapper text)))

;; Additional convenience functions
(defn sender
  "Shorthand for get-sender"
  [msg-wrapper]
  (get-sender msg-wrapper))

(defn chat-id
  "Shorthand for get-chat-id"
  [msg-wrapper]
  (get-chat-id msg-wrapper))

(defn command
  "Shorthand for get-command"
  [msg-wrapper]
  (get-command msg-wrapper))

(defn text
  "Shorthand for get-text"
  [msg-wrapper]
  (get-text msg-wrapper))

(defn mod-text
  "Shorthand for get-mod-text"
  [msg-wrapper]
  (get-mod-text msg-wrapper))