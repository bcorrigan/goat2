(ns org.goat.core.pager
  "Pagination logic for long messages.

   Long messages are automatically split into pages. This namespace manages
   the pagination cache and provides functions to create paged messages."
  (:require [org.goat.core.message-parse :as msg-parse]
            [clojure.string :as str])
  (:import [java.nio.charset Charset CharsetEncoder]
           [java.nio CharBuffer ByteBuffer]))

(def ^:private utf-8-charset (Charset/forName "UTF-8"))
(def ^:private default-max-bytes 4096) ; Telegram max is 4096 characters
(def ^:private inner-pre "…")
(def ^:private inner-post " [more]")
(def ^:private max-walkback 32)
(def ^:private bold "\u0002")
(def ^:private end-bold "\u0002")
(def ^:private input-exceeded-msg
  (str " " bold "[" end-bold "no more — buffer protection engaged" bold "]" end-bold))
(def ^:private max-input-chars (- (* default-max-bytes 17) (count input-exceeded-msg)))

(defn- byte-length
  "Calculate the byte length of a string in UTF-8 encoding."
  [^String s]
  (count (.getBytes s utf-8-charset)))

(defn- max-encodeable
  "Find the largest substring of s that fits in maxBytes when UTF-8 encoded.

   Uses CharsetEncoder to properly handle multibyte UTF-8 characters."
  [^String s max-bytes]
  (let [char-buf (CharBuffer/wrap s)
        byte-buf (ByteBuffer/wrap (byte-array max-bytes))
        encoder (.newEncoder utf-8-charset)]
    (.encode encoder char-buf byte-buf true)
    (.substring s 0 (.position char-buf))))

(defn- trim-whitespace
  "Trim leading and trailing whitespace (\\s characters) from string.

   Like String.trim() but only removes \\n, \\t, space, \\f, \\r characters,
   preserving other formatting characters."
  [^String s]
  (-> s
      (str/replace #"^\s+" "")
      (str/replace #"\s+$" "")))

(defn- smush
  "Process text for pagination. Currently a no-op."
  [text]
  text)

(defrecord PagerState
  [buffer        ; String - remaining text to paginate
   untapped      ; Boolean - true if this is the first page
   max-bytes])   ; Int - maximum bytes per page

(defn- create-pager
  "Create a new pager state for the given text.

   Truncates text if it exceeds maximum input length."
  ([text] (create-pager text default-max-bytes))
  ([text max-bytes]
   (let [processed-text (smush text)
         truncated-text (if (> (count processed-text) max-input-chars)
                         (str (subs processed-text 0 max-input-chars) input-exceeded-msg)
                         processed-text)]
     (->PagerState truncated-text true max-bytes))))

(defn- pager-empty?
  "Returns true if the pager buffer is empty."
  [pager]
  (str/blank? (:buffer pager)))

(defn- pager-remaining
  "Returns the number of bytes remaining in the pager buffer."
  [pager]
  (byte-length (:buffer pager)))

(defn- first-max [num-bytes] (- num-bytes (byte-length inner-post)))
(defn- last-max [num-bytes] (- num-bytes (byte-length inner-pre)))
(defn- mid-max [num-bytes] (- num-bytes (byte-length inner-pre) (byte-length inner-post)))

(defn- get-chunk
  "Extract a chunk of text from the pager buffer.

   Removes the chunk from buffer and returns [new-pager chunk-text].
   Breaks at form-feed character (\\f) if present."
  [pager num-bytes]
  (let [buffer (:buffer pager)
        remaining (pager-remaining pager)]
    (if (<= remaining num-bytes)
      ;; Return entire buffer
      (let [chunk (trim-whitespace buffer)]
        [(assoc pager :buffer "") chunk])
      ;; Extract substring that fits in num-bytes
      (let [chunk-text (max-encodeable buffer num-bytes)
            trimmed (trim-whitespace chunk-text)]
        ;; Check for form-feed character
        (if (str/includes? trimmed "\f")
          (let [ff-idx (str/index-of trimmed "\f")
                before-ff (subs buffer 0 ff-idx)
                after-ff (subs buffer (inc ff-idx))
                new-buffer (trim-whitespace after-ff)]
            [(assoc pager :buffer new-buffer) before-ff])
          ;; No form-feed, split normally
          (let [chunk-len (count trimmed)
                new-buffer (trim-whitespace (subs buffer chunk-len))]
            [(assoc pager :buffer new-buffer) trimmed]))))))

(defn- get-polite-chunk
  "Extract a chunk, trying to break at whitespace boundary.

   Walks back up to max-walkback characters to find whitespace."
  [pager num-bytes]
  (let [buffer (:buffer pager)
        remaining (pager-remaining pager)]
    (if (<= remaining num-bytes)
      (get-chunk pager num-bytes)
      ;; Try to find polite break point
      (let [rude-chunk (max-encodeable buffer num-bytes)
            chunk-len (count rude-chunk)
            ;; Search backwards for whitespace
            polite-pos (loop [i (dec chunk-len)]
                        (cond
                          (< i 0) 0
                          (< i (- chunk-len max-walkback)) 0
                          (re-matches #"\s" (str (get buffer i))) i
                          :else (recur (dec i))))]
        (if (pos? polite-pos)
          (get-chunk pager (byte-length (subs buffer 0 polite-pos)))
          (get-chunk pager num-bytes))))))

(defn- extract-page
  "Get the next page from pager state.

   Returns [new-pager page-text].
   Automatically adds … prefix and [more] suffix as appropriate."
  [pager]
  (let [max-bytes (:max-bytes pager)
        untapped (:untapped pager)
        remaining (pager-remaining pager)]
    (try
      (cond
        ;; First page
        untapped
        (let [num-bytes (if (<= remaining max-bytes)
                         max-bytes
                         (first-max max-bytes))
              [new-pager chunk] (get-polite-chunk pager num-bytes)
              page-text (if-not (pager-empty? new-pager)
                         (str chunk inner-post)
                         chunk)]
          [(assoc new-pager :untapped false) page-text])

        ;; Last page
        (<= remaining (last-max max-bytes))
        (let [[new-pager chunk] (get-polite-chunk pager (last-max max-bytes))
              page-text (str inner-pre chunk)]
          [new-pager page-text])

        ;; Middle page
        :else
        (let [[new-pager chunk] (get-polite-chunk pager (mid-max max-bytes))
              page-text (str inner-pre chunk inner-post)]
          [new-pager page-text]))

      (catch Exception e
        (println "ERROR: Pager encoding exception:" (.getMessage e))
        (.printStackTrace e)
        [(assoc pager :buffer "") "I didn't feel like encoding that for you."]))))

;; Cache mapping chat-id to Pager instances.
;; When a long message is sent, the Pager is stored here and subsequent
;; pages are retrieved using the More module.
(defonce pager-cache (atom {}))

(defn should-paginate?
  "Returns true if the text is long enough to require pagination."
  [text]
  (> (byte-length text) default-max-bytes))

(defn create-paged-reply
  "Create a paged reply message from source message and text.

   If text is short enough, returns a normal reply.
   If text requires pagination:
   - Creates a Pager and stores it in cache
   - Returns a message with the first page
   - Subsequent pages retrieved via get-next-page

   Example:
   (create-paged-reply source-msg very-long-text)
   => {:message/text <first-page>
       :message/chat-id 123
       ...}"
  [source-msg text]
  (let [chat-id (:message/chat-id source-msg)]
    (if (should-paginate? text)
      ;; Create pager, cache it, return first page
      (let [pager (create-pager text)
            [new-pager first-page] (extract-page pager)]
        (swap! pager-cache assoc chat-id new-pager)
        (msg-parse/create-reply source-msg :text first-page))
      ;; Text is short, just return normal reply
      (msg-parse/create-reply source-msg :text text))))

(defn has-next-page?
  "Returns true if there are more pages available for the given chat-id."
  [chat-id]
  (when-let [pager (get @pager-cache chat-id)]
    (if (pager-empty? pager)
      (do
        ;; Clean up empty pager
        (swap! pager-cache dissoc chat-id)
        false)
      true)))

(defn get-next-page
  "Get the next page for the given chat-id.

   Returns the next page text, or nil if no more pages.
   Automatically removes pager from cache when empty.

   Example:
   (get-next-page 123)
   => 'This is page 2 of the long message...'"
  [chat-id]
  (when (has-next-page? chat-id)
    (let [pager (get @pager-cache chat-id)
          [new-pager page] (extract-page pager)]
      ;; Update cache with new pager state
      (swap! pager-cache assoc chat-id new-pager)
      ;; Remove from cache if now empty
      (when (pager-empty? new-pager)
        (swap! pager-cache dissoc chat-id))
      page)))

(defn create-next-page-message
  "Create a message containing the next page for the source message.

   Returns a reply message with the next page text, or a message with
   empty text if no more pages available.

   Example:
   (create-next-page-message source-msg)
   => {:message/text <next-page>, :message/chat-id 123, ...}"
  [source-msg]
  (let [chat-id (:message/chat-id source-msg)
        next-page-text (get-next-page chat-id)]
    (msg-parse/create-reply source-msg
                           :text (or next-page-text ""))))

(defn clear-pager!
  "Clear the pager for a specific chat-id.
   Useful for testing or manual cleanup."
  [chat-id]
  (swap! pager-cache dissoc chat-id))

(defn clear-all-pagers!
  "Clear all pagers from cache.
   Useful for testing."
  []
  (reset! pager-cache {}))

(defn active-pagers
  "Returns a set of chat-ids that currently have active pagers.
   Useful for debugging and monitoring."
  []
  (set (keys @pager-cache)))

(comment
  ;; Check if text needs pagination
  (should-paginate? "short text")
  ;; => false

  (should-paginate? (apply str (repeat 1000 "long text ")))
  ;; => true

  ;; Create paged reply
  (require '[org.goat.core.message-parse :as mp])
  (def source-msg (mp/create-message :chat-id 123 :sender "alice" :private? false))

  (def long-text (apply str (repeat 1000 "This is a very long message. ")))
  (def first-page-msg (create-paged-reply source-msg long-text))

  ;; Check if more pages available
  (has-next-page? 123)
  ;; => true

  ;; Get next page
  (get-next-page 123)
  ;; => "Page 2 content..."

  ;; Create next page message
  (create-next-page-message source-msg)

  ;; View active pagers
  (active-pagers)
  ;; => #{123}

  ;; Clear pager
  (clear-pager! 123)
  (has-next-page? 123)
  ;; => false
  )
