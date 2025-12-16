(ns org.goat.module.More
  "Pagination module - displays the next page of a long message.
  When a message has been split into multiple pages, users can type
  'more' or 'moar' to retrieve the next page."
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESSAGE HANDLER ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-more
  "Handle more/moar command - send next page if available."
  [m]
  (let [mod-text (msg/mod-text m)]
    ;; Only process if no additional text was provided
    (when (str/blank? mod-text)
      (when (msg/has-next-page? m)
        (-> m
            msg/create-next-page
            msg/send-msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE DEFINITION ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule More
  :commands [:more :moar]

  (defn process-message [m]
    (handle-more m)))
