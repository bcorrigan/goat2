(ns main.clojure.org.goat.module.CoreCommands
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:require [org.goat.db.users :as users])
  (:import  [org.goat.core BotStats]
            [org.goat.util StringUtil]
            [org.goat.core Constants]
            ))

(def init (System/currentTimeMillis))

(defn -processChannelMessage
  [_ m]
  (cond (= (.getModCommand m) "mem")
            (let [mem-bytes (. (Runtime/getRuntime) totalMemory)
                  mem-kb (/ (mem-bytes 1024))]
                  (.reply m (str mem-kb "kb")))
        (= (.getModCommand m) "uptime")
            (let [now (System/currentTimeMillis)
                  uptime (- now init)
                  uptime-str (StringUtil/vshortDurationString uptime)]
              (.reply m uptime-str))
        (= (.getModCommand m) "goat")
            (.reply m (str (Constants/BOLD) "Goat!" (Constants/END_BOLD)))
        (= (.getModCommand m) "version")
            (let [pkg (. (class Runtime) getPackage)
                  version (str " Version: " (.getImplementationVersion pkg))
                  title (str " Title: " (.getImplementationTitle pkg))
                  os-version (System/getProperty "os.name")]
              (.reply m (str version title os-version)))
        (= (.getModCommand m) "setchat")
            (do
              (users/user-add (.getSender m) (keyword (str (.getChatId m))))
              (.reply m "OK. I've set this chat as your private chat."))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '("mem" "uptime", "goat", "version", "setchat")))

(defn -messageType [_] 0)
