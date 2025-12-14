(ns org.goat.core.message
  "Clojure message protocol and utilities for map-based messages.

   Messages are represented as Clojure maps with namespaced keywords.
   This namespace provides the MessageContext protocol for working with
   messages in a functional, idiomatic way."
  (:require [org.goat.core.channels :as channels]
            [org.goat.core.pager :as pager]
            [org.goat.core.message-parse :as msg-parse]))

(defprotocol MessageContext
  "Protocol for interacting with messages in a Clojurian way"
  (send-msg [this] "Send this message to the outgoing channel")
  (reply [this text] "Send a text reply to this message")
  (reply-image [this img] "Send an image reply to this message")
  (reply-document [this bytes filename] "Send a document reply to this message")
  (get-command [this] "Get the command as a keyword (e.g. :wordle)")
  (get-text [this] "Get the full text of the message")
  (get-mod-text [this] "Get the text after the command")
  (get-sender [this] "Get the sender of the message")
  (get-chat-id [this] "Get the chat ID")
  (get-chatname [this] "Get the chat name")
  (private? [this] "True if this is a private message")
  (has-text? [this] "True if this message has text")
  (has-image? [this] "True if this message has an image")
  (has-document? [this] "True if this message has a document")
  (get-document-bytes [this] "Get the document bytes")
  (get-document-filename [this] "Get the document filename")
  (has-next-page? [this] "True if this message has a next page for pagination")
  (create-next-page [this] "Create and return the next page message")
  (fmt [this] "Get the formatter for this message's platform"))

(extend-protocol MessageContext
  clojure.lang.IPersistentMap

  (send-msg [this]
    (channels/put-outgoing! this)
    this)

  (reply [this text]
    (let [reply-msg (pager/create-paged-reply this text)]
      (send-msg reply-msg)))

  (reply-image [this img]
    (let [reply-msg (msg-parse/create-reply this :image img)]
      (send-msg reply-msg)))

  (reply-document [this bytes filename]
    (let [reply-msg (msg-parse/create-reply this
                                           :document-bytes bytes
                                           :document-filename filename)]
      (send-msg reply-msg)))

  (get-command [this]
    (:message/command this))

  (get-text [this]
    (:message/text this))

  (get-mod-text [this]
    (or (:message/command-args this) ""))

  (get-sender [this]
    (:message/sender this))

  (get-chat-id [this]
    (:message/chat-id this))

  (get-chatname [this]
    (:message/chatname this))

  (private? [this]
    (:message/private? this))

  (has-text? [this]
    (boolean (:message/text this)))

  (has-image? [this]
    (msg-parse/has-image? this))

  (has-document? [this]
    (msg-parse/has-document? this))

  (get-document-bytes [this]
    (:message.attachment/document-bytes this))

  (get-document-filename [this]
    (:message.attachment/document-filename this))

  (has-next-page? [this]
    (pager/has-next-page? (:message/chat-id this)))

  (create-next-page [this]
    (pager/create-next-page-message this))

  (fmt [this]
    (require 'org.goat.core.format)
    ((resolve 'org.goat.core.format/formatter)
     (or (:platform/type this) :telegram))))

(defn command-matches?
  "Check if the message command matches any of the given keywords.

   Example:
   (command-matches? msg :wordle :stats :help)"
  [msg & commands]
  (contains? (set commands) (get-command msg)))

(defn reply-when
  "Reply with text only if condition is true.

   Example:
   (reply-when msg (authorized? msg) 'Admin command executed')"
  [msg condition text]
  (when condition
    (reply msg text)))

(defn reply-format
  "Reply with formatted text (like format).

   Example:
   (reply-format msg 'Hello, %s! Your score is %d' name score)"
  [msg fmt & args]
  (reply msg (apply format fmt args)))

(defn case-command
  "Convenience function for command routing with keywords.

   Example:
   (case-command msg
     :wordle #(handle-wordle msg)
     :stats #(handle-stats msg))"
  [msg & clauses]
  (let [cmd (get-command msg)]
    (loop [remaining clauses]
      (cond
        (empty? remaining) nil
        (= (first remaining) cmd) ((second remaining))
        :else (recur (drop 2 remaining))))))

(defn reply-all
  "Send multiple replies in sequence.

   Example:
   (reply-all msg 'Line 1' 'Line 2' 'Line 3')"
  [msg & texts]
  (doseq [text texts]
    (reply msg text)))

(defn sender
  "Shorthand for get-sender"
  [msg]
  (get-sender msg))

(defn chat-id
  "Shorthand for get-chat-id"
  [msg]
  (get-chat-id msg))

(defn command
  "Shorthand for get-command"
  [msg]
  (get-command msg))

(defn text
  "Shorthand for get-text"
  [msg]
  (get-text msg))

(defn mod-text
  "Shorthand for get-mod-text"
  [msg]
  (get-mod-text msg))

(defn document-bytes
  "Shorthand for get-document-bytes"
  [msg]
  (get-document-bytes msg))

(defn document-filename
  "Shorthand for get-document-filename"
  [msg]
  (get-document-filename msg))

(defn fmt
  "Shorthand for getting formatter from message.

   Returns a map with formatting keys like :bold, :end-bold, etc.
   appropriate for the message's platform.

   Example:
   (let [f (fmt m)]
     (str (:bold f) \"Hello\" (:end-bold f)))"
  [msg]
  (require 'org.goat.core.format)
  ((resolve 'org.goat.core.format/formatter)
   (or (:platform/type msg) :telegram)))

(defn wrap-message
  "No-op: messages are already maps. Provided for backward compatibility."
  [msg]
  msg)

(defn unwrap
  "No-op: messages are already maps. Provided for backward compatibility."
  [msg]
  msg)

(defn ensure-wrapped
  "No-op: messages are already maps. Provided for backward compatibility."
  [msg]
  msg)

(comment
  ;; Create a message
  (require '[org.goat.core.message-parse :as mp])
  (def msg (mp/create-message :chat-id 123
                              :sender "alice"
                              :private? false
                              :text "wordle 5"))

  ;; Access message fields
  (get-sender msg)         ;; => "alice"
  (sender msg)             ;; => "alice" (shorthand)
  (get-command msg)        ;; => :wordle
  (command msg)            ;; => :wordle (shorthand)
  (get-mod-text msg)       ;; => "5"
  (mod-text msg)           ;; => "5" (shorthand)

  ;; Send a reply
  (reply msg "Your wordle game has started!")

  ;; Send a formatted reply
  (reply-format msg "Hello, %s!" (sender msg))

  ;; Command routing
  (case-command msg
    :wordle #(println "Starting wordle")
    :stats #(println "Showing stats"))

  ;; Check command
  (command-matches? msg :wordle :stats :help)  ;; => true

  ;; Pagination
  (has-next-page? msg)     ;; => false (initially)
  (create-next-page msg)   ;; => next page message if available
  )
