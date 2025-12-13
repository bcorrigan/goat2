(ns org.goat.core.platform
  "Platform abstraction protocol for messaging platforms.

   This protocol defines the interface for sending messages and downloading
   attachments from different messaging platforms (Telegram, CLI, etc.).
   Keeps the core bot logic platform-agnostic."
  (:require [clojure.tools.logging :as log])
  (:import [org.telegram.telegrambots.meta.api.methods.send SendMessage SendPhoto SendDocument]
           [org.telegram.telegrambots.meta.api.objects InputFile]
           [org.telegram.telegrambots.client.okhttp OkHttpTelegramClient]
           [java.awt.image RenderedImage]
           [java.io ByteArrayOutputStream ByteArrayInputStream InputStream]
           [javax.imageio ImageIO]))

;; =============================================================================
;; Platform Protocol
;; =============================================================================

(defprotocol Platform
  "Protocol for messaging platform operations.

   Implementations provide platform-specific logic for sending messages
   and downloading attachments while presenting a uniform interface."

  (send-text [this chat-id text options]
    "Send a text message to the specified chat.

     Arguments:
     - chat-id: Platform-specific chat identifier
     - text: Message text content
     - options: Map of platform-specific options (e.g., {:parse-mode 'html'})

     Returns: Platform-specific response or nil on error")

  (send-image [this chat-id image options]
    "Send an image to the specified chat.

     Arguments:
     - chat-id: Platform-specific chat identifier
     - image: RenderedImage to send
     - options: Map of platform-specific options

     Returns: Platform-specific response or nil on error")

  (send-document [this chat-id bytes filename options]
    "Send a document to the specified chat.

     Arguments:
     - chat-id: Platform-specific chat identifier
     - bytes: Document content as byte array
     - filename: Document filename
     - options: Map of platform-specific options

     Returns: Platform-specific response or nil on error")

  (download-document [this file-id]
    "Download a document from the platform.

     Arguments:
     - file-id: Platform-specific file identifier

     Returns: Byte array of document content, or nil on error")

  (platform-name [this]
    "Returns the name of this platform as a keyword (e.g., :telegram, :cli)"))

;; =============================================================================
;; Telegram Platform Implementation
;; =============================================================================

(defn- rendered-image->input-file
  "Convert a RenderedImage to an InputFile for Telegram.
   Converts the image to PNG format in memory."
  [^RenderedImage image]
  (let [baos (ByteArrayOutputStream.)]
    (ImageIO/write image "png" baos)
    (let [istr (ByteArrayInputStream. (.toByteArray baos))]
      (InputFile. istr "image.png"))))

(defrecord TelegramPlatform [^OkHttpTelegramClient client]
  Platform

  (send-text [this chat-id text options]
    (try
      (let [send-msg (SendMessage. (str chat-id) text)]
        ;; Always use HTML parse mode for formatting support
        (.setParseMode send-msg "html")
        ;; Apply any additional options
        (doseq [[k v] options]
          (case k
            :parse-mode (.setParseMode send-msg (name v))
            ;; Add other options as needed
            (log/warn "Unknown send-text option:" k)))
        (.execute client send-msg))
      (catch Exception e
        (log/error e "Failed to send text message to chat" chat-id)
        nil)))

  (send-image [this chat-id image options]
    (try
      (let [input-file (rendered-image->input-file image)
            send-photo (SendPhoto. (str chat-id) input-file)]
        ;; Apply options if needed
        (doseq [[k v] options]
          (log/warn "Unknown send-image option:" k))
        (.execute client send-photo))
      (catch Exception e
        (log/error e "Failed to send image to chat" chat-id)
        nil)))

  (send-document [this chat-id bytes filename options]
    (try
      (let [istr (ByteArrayInputStream. bytes)
            input-file (InputFile. istr filename)
            send-doc (SendDocument. (str chat-id) input-file)]
        ;; Apply options if needed
        (doseq [[k v] options]
          (log/warn "Unknown send-document option:" k))
        (.execute client send-doc))
      (catch Exception e
        (log/error e "Failed to send document to chat" chat-id)
        nil)))

  (download-document [this file-id]
    (try
      ;; Get file path from Telegram
      (let [get-file (org.telegram.telegrambots.meta.api.methods.GetFile. file-id)
            file (.execute client get-file)
            file-path (.getFilePath file)]
        ;; Download file content
        (.downloadFileAsBytes client file-path))
      (catch Exception e
        (log/error e "Failed to download document" file-id)
        nil)))

  (platform-name [this]
    :telegram))

;; =============================================================================
;; CLI Platform Implementation (for testing)
;; =============================================================================

;; CLI platform for testing.
;; Captures all sends to an atom for verification.
(defrecord CLIPlatform [output-atom]
  Platform

  (send-text [this chat-id text options]
    (let [msg {:type :text
               :chat-id chat-id
               :text text
               :options options}]
      (swap! output-atom conj msg)
      (println (format "[CLI %s] %s" chat-id text))
      msg))

  (send-image [this chat-id image options]
    (let [msg {:type :image
               :chat-id chat-id
               :image image
               :options options}]
      (swap! output-atom conj msg)
      (println (format "[CLI %s] <image>" chat-id))
      msg))

  (send-document [this chat-id bytes filename options]
    (let [msg {:type :document
               :chat-id chat-id
               :filename filename
               :size (count bytes)
               :options options}]
      (swap! output-atom conj msg)
      (println (format "[CLI %s] <document: %s (%d bytes)>" chat-id filename (count bytes)))
      msg))

  (download-document [this file-id]
    (log/warn "CLI platform does not support document download")
    nil)

  (platform-name [this]
    :cli))

;; =============================================================================
;; Platform Creation Helpers
;; =============================================================================

(defn create-telegram-platform
  "Create a Telegram platform instance.

   Arguments:
   - client: OkHttpTelegramClient instance

   Returns: TelegramPlatform record"
  [^OkHttpTelegramClient client]
  (->TelegramPlatform client))

(defn create-cli-platform
  "Create a CLI platform instance for testing.

   Optional arguments:
   - output-atom: Atom to capture output (default: new atom)

   Returns: CLIPlatform record"
  ([]
   (create-cli-platform (atom [])))
  ([output-atom]
   (->CLIPlatform output-atom)))

;; =============================================================================
;; Examples and Testing
;; =============================================================================

(comment
  ;; Create CLI platform for testing
  (def cli-platform (create-cli-platform))

  ;; Send text via CLI
  (send-text cli-platform 123 "Hello, world!" {})

  ;; Send document via CLI
  (send-document cli-platform 123 (.getBytes "test content") "test.txt" {})

  ;; Check captured output
  @(:output-atom cli-platform)

  ;; Platform name
  (platform-name cli-platform)
  ;; => :cli

  ;; Create Telegram platform (requires actual client)
  (require '[org.goat.core BotStats])
  (def bot-token (BotStats/getInstance))
  ;; (def telegram-client (OkHttpTelegramClient. (.getBotKey bot-token)))
  ;; (def telegram-platform (create-telegram-platform telegram-client))
  )
