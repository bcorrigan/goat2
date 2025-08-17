(ns org.goat.module.Capture
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:require [org.goat.db.urls :as urls]))

(defn extract-url
  "If there's a url in the msg, extract it and also return original msg & timestamp too"
  [m]
  (let [url-pattern #"https?://[^\s]+|www\.[^\s]+"
        msg (.getText m)
        found-url (re-find url-pattern msg)]
    (when found-url
      {:url found-url
       :msg msg
       :chatname (.getChatname m)
       :chatid (.getChatId m)
       :sender (.getSender m)
       :time (System/currentTimeMillis)})))

;; Inspect each message, if it contains a url lets save it with timestamp to a sqlite url db
;; along with timestamp, chat it was on etc.
(defn -processChannelMessage
  [_ m]
  (let [urlinfo  (extract-url m)]
    (when urlinfo
      (urls/save-url urlinfo))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '()))

