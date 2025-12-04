(ns org.goat.testutils.message
  "General Message mocking utility for unit tests.
   
   Provides comprehensive mocking functionality for org.goat.core.Message objects,
   supporting all common usage patterns across modules including Capture, Wordle, etc."
  (:import (org.goat.core Message)
           (java.awt.image RenderedImage BufferedImage)
           (java.awt Graphics2D Color)))

(def ^:private reply-log 
  "Atom to capture all replies sent during testing"
  (atom []))

(defn get-replies
  "Get all replies sent by mock messages during testing.
   Returns vector of {:text string :image RenderedImage} maps."
  []
  @reply-log)

(defn clear-replies!
  "Clear the reply log for fresh test isolation."
  []
  (reset! reply-log []))

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
  "Creates a mock org.goat.core.Message object for testing.

   Options map supports:
   - :text         - The message text (default: 'test message')
   - :chat-id      - Chat ID as Long (default: 123)
   - :sender       - Who sent the message (default: 'test-user')
   - :chat-name    - Name of chat room (default: 'test-chat')
   - :is-private   - Whether message is private/1-1 (default: false)
   - :capture-replies - Whether to capture .reply() calls (default: true)
   - :document-bytes - Byte array for document content (default: nil)
   - :document-filename - Filename for document (default: nil)

   The mock supports all key Message methods and captures reply calls for testing.

   Example usage:
   (mock-message {:text 'wordle 5 hard' :sender 'alice' :chat-id 456})
   (mock-message {:text 'Check out https://example.com'})
   (mock-message {:sender 'bob' :is-private true})
   (mock-message {:document-bytes csv-bytes :document-filename 'data.csv'})"

  ([] (mock-message {}))
  ([{:keys [text chat-id sender chat-name is-private capture-replies document-bytes document-filename]
     :or {text "test message"
          chat-id 123
          sender "test-user"
          chat-name "test-chat"
          is-private false
          capture-replies true
          document-bytes nil
          document-filename nil}}]
   
   ;; Create the actual Message object using the constructor
   ;; This handles all the parsing logic (modCommand, modText, etc.)
   (let [base-msg (Message. chat-id text is-private sender)]
     
     ;; If we need reply capture, wrap with a proxy
     (if capture-replies
       (let [mock-msg (proxy [Message] [chat-id text is-private sender]
                        ;; Delegate all getters to the real message
                        (getText [] (.getText base-msg))
                        (getChatId [] (.getChatId base-msg))
                        (getSender [] (.getSender base-msg))
                        (getChatname [] (.getChatname base-msg))
                        (getModCommand [] (.getModCommand base-msg))
                        (getModText [] (.getModText base-msg))
                        (isPrivate [] (.isPrivate base-msg))
                        (hasText [] (.hasText base-msg))
                        (hasImage [] (.hasImage base-msg))
                        (hasDocument [] (some? document-bytes))
                        (getDocumentBytes [] document-bytes)
                        (getDocumentFilename [] document-filename)

                        ;; Override reply methods to capture
                        (reply [msg-text]
                          (swap! reply-log conj {:type :text :content msg-text}))

                        (replyWithImage [img]
                          (swap! reply-log conj {:type :image :content img}))

                        (replyWithDocument [bytes filename]
                          (swap! reply-log conj {:type :document :content bytes :filename filename})))]
         ;; Document data is already handled via proxy methods above
         mock-msg)

       ;; Otherwise just return the real message (possibly with document set)
       (let [msg base-msg]
         (when (and document-bytes document-filename)
           (.setIncomingDocument msg document-filename document-bytes))
         msg)))))

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
   (let [bot-name "goat" ; Could be made configurable
         args-str (cond
                    (nil? args) ""
                    (string? args) args
                    (coll? args) (clojure.string/join " " args)
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
   (mock-message (assoc opts :text text :is-private true))))

;; Testing utilities

(defmacro with-clean-replies
  "Execute body with a clean reply log, automatically clearing before and after."
  [& body]
  `(do
     (clear-replies!)
     (try
       ~@body
       (finally
         (clear-replies!)))))

(defn replied-with?
  "Check if any reply contains the given text (case-insensitive substring match)."
  [text]
  (some #(and (= :text (:type %))
             (clojure.string/includes? 
              (clojure.string/lower-case (:content %))
              (clojure.string/lower-case text)))
        (get-replies)))

(defn replied-with-image?
  "Check if any image reply was sent."
  []
  (some #(= :image (:type %)) (get-replies)))

(defn reply-count
  "Count total number of replies sent."
  []
  (count (get-replies)))

(defn text-reply-count  
  "Count number of text replies sent."
  []
  (count (filter #(= :text (:type %)) (get-replies))))

(defn image-reply-count
  "Count number of image replies sent."
  []
  (count (filter #(= :image (:type %)) (get-replies))))

(defn replied-with-document?
  "Check if any document reply was sent."
  []
  (some #(= :document (:type %)) (get-replies)))

(defn document-reply-count
  "Count number of document replies sent."
  []
  (count (filter #(= :document (:type %)) (get-replies))))

(defn get-document-reply
  "Get the first document reply, or nil if none.
   Returns map with :content (bytes) and :filename."
  []
  (first (filter #(= :document (:type %)) (get-replies))))

(defn get-document-content
  "Get the content (byte array) of the first document reply, or nil."
  []
  (:content (get-document-reply)))

(defn get-document-filename
  "Get the filename of the first document reply, or nil."
  []
  (:filename (get-document-reply)))