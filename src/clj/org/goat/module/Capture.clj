(ns org.goat.module.Capture
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:import
   (org.goat.core Module))
  (:require [org.goat.db.urls :as urls]
            [clojure.string :as str]))

(defn extract-url
  "If there's a url in the msg, extract it and also return original msg & timestamp too"
  [m]
  (let [msg (.getText m)]
    (when (and msg (not (str/blank? msg)))
      (let [url-pattern #"https?://[^\s]+|www\.[^\s]+"
            found-url (re-find url-pattern msg)]
        (when found-url
          {:url found-url
           :msg msg
           :chatname (.getChatname m)
           :chatid (.getChatId m)
           :sender (.getSender m)
           :time (System/currentTimeMillis)})))))

;; Inspect each message, if it contains a url lets save it with timestamp to a sqlite url db
;; along with timestamp, chat it was on etc.
(defn -processChannelMessage
  [_ m]
  (let [urlinfo  (extract-url m)
        mod-command (.getModCommand m)
        note (and mod-command (= "note" (str/lower-case mod-command)))]
    (if urlinfo
      (urls/save-url urlinfo)
      (when note
        (urls/save-url {:url nil
                        :msg (.getText m)
                        :chatname (.getChatname m)
                        :chatid (.getChatId m)
                        :sender (.getSender m)
                        :time (System/currentTimeMillis)})))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '()))

(defn -messageType [_]
  Module/WANT_ALL_MESSAGES)
