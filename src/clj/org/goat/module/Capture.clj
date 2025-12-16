(ns org.goat.module.Capture
  (:require [org.goat.db.urls :as urls]
            [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as message]
            [clojure.string :as str]))

(defn extract-url
  "If there's a url in the msg, extract it and also return original msg & timestamp too"
  [m]
  (let [msg (message/text m)]
    (when (and msg (not (str/blank? msg)))
      (let [url-pattern #"https?://[^\s]+|www\.[^\s]+"
            found-url (re-find url-pattern msg)]
        (when found-url
          {:url found-url
           :msg msg
           :chatname (message/get-chatname m)
           :chatid (message/chat-id m)
           :sender (message/sender m)
           :time (System/currentTimeMillis)})))))

;; Inspect each message, if it contains a url lets save it with timestamp to a sqlite url db
;; along with timestamp, chat it was on etc.
(defmodule Capture
  :commands []
  :receive-messages :all

  (defn process-message [m]
    (let [urlinfo (extract-url m)
          cmd (message/command m)
          note (and cmd (= :note cmd))]
      (if urlinfo
        (urls/save-url urlinfo)
        (when note
          (urls/save-url {:url nil
                          :msg (message/text m)
                          :chatname (message/get-chatname m)
                          :chatid (message/chat-id m)
                          :sender (message/sender m)
                          :time (System/currentTimeMillis)}))))))
