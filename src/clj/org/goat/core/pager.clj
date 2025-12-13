(ns org.goat.core.pager
  "Pagination logic for long messages.

   Long messages are automatically split into pages. This namespace manages
   the pagination cache and provides functions to create paged messages."
  (:require [org.goat.core.message-parse :as msg-parse])
  (:import [org.goat.util Pager]))

;; =============================================================================
;; Pagination State
;; =============================================================================

;; Cache mapping chat-id to Pager instances.
;; When a long message is sent, the Pager is stored here and subsequent
;; pages are retrieved using the More module.
(defonce pager-cache (atom {}))

;; =============================================================================
;; Pagination Functions
;; =============================================================================

(defn should-paginate?
  "Returns true if the text is long enough to require pagination."
  [text]
  (Pager/shouldPaginate text))

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
      (let [pager (Pager. text)
            first-page (.getNext pager)]
        (swap! pager-cache assoc chat-id pager)
        (msg-parse/create-reply source-msg :text first-page))
      ;; Text is short, just return normal reply
      (msg-parse/create-reply source-msg :text text))))

(defn has-next-page?
  "Returns true if there are more pages available for the given chat-id."
  [chat-id]
  (when-let [pager (get @pager-cache chat-id)]
    (if (.isEmpty pager)
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
          page (.getNext pager)]
      ;; Remove from cache if now empty
      (when (.isEmpty pager)
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

;; =============================================================================
;; Examples and Testing
;; =============================================================================

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
