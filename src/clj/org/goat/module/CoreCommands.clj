(ns org.goat.module.CoreCommands
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.users :as users])
  (:import [org.goat.util StringUtil]
           [org.goat.core Constants]))

(def init (System/currentTimeMillis))

(defn get-detailed-memory-info
  "Get comprehensive memory information"
  []
  (let [runtime (Runtime/getRuntime)
        mb 1048576.0
        total-memory (/ (.totalMemory runtime) mb)
        free-memory (/ (.freeMemory runtime) mb)
        used-memory (- total-memory free-memory)
        max-memory (/ (.maxMemory runtime) mb)
        available-processors (.availableProcessors runtime)]
    (format "🧠 <b>Memory Stats</b>\n• Used: %.1f MB\n• Free: %.1f MB\n• Total: %.1f MB\n• Max: %.1f MB\n• CPU Cores: %d"
            used-memory free-memory total-memory max-memory available-processors)))

(defn get-detailed-version-info
  "Get comprehensive system and runtime information"
  []
  (let [runtime-version (str (Runtime/version))
        java-version (System/getProperty "java.version")
        java-vendor (System/getProperty "java.vendor")
        os-name (System/getProperty "os.name")
        os-version (System/getProperty "os.version")
        os-arch (System/getProperty "os.arch")
        user-name (System/getProperty "user.name")
        user-home (System/getProperty "user.home")
        working-dir (System/getProperty "user.dir")
        clojure-version (clojure-version)
        jvm-name (System/getProperty "java.vm.name")
        jvm-vendor (System/getProperty "java.vm.vendor")]
    (format "🤖 <b>System Information</b>\n• Java: %s (%s)\n• JVM: %s\n• Clojure: %s\n• OS: %s %s (%s)\n• User: %s\n• Working Dir: %s\n• Runtime: %s"
            java-version java-vendor jvm-name clojure-version
            os-name os-version os-arch user-name working-dir runtime-version)))

(defn get-enhanced-uptime
  "Get detailed uptime information"
  []
  (let [now (System/currentTimeMillis)
        uptime-ms (- now init)
        uptime-str (StringUtil/vshortDurationString uptime-ms)
        uptime-seconds (double (/ uptime-ms 1000))
        uptime-minutes (double (/ uptime-seconds 60))
        uptime-hours (double (/ uptime-minutes 60))
        uptime-days (double (/ uptime-hours 24))]
    (format "⏰ <b>Uptime</b>\n• %s\n• %.1f days, %.1f hours, %.1f minutes"
            uptime-str uptime-days uptime-hours uptime-minutes)))

(defmodule CoreCommands
  :commands [:gc :mem :uptime :goat :version :setchat]
  :message-type org.goat.core.Module/WANT_ALL_MESSAGES

  (defn process-channel-message [m]
    (case (msg/command m)
      :mem (msg/reply m (get-detailed-memory-info))

      :gc (do
            (System/gc)
            (msg/reply m "🗑️ Performed garbage collection."))

      :uptime (msg/reply m (get-enhanced-uptime))

      :goat (msg/reply m (str Constants/BOLD "🐐 Goat!" Constants/END_BOLD))

      :version (msg/reply m (get-detailed-version-info))

      :setchat (do
                 (users/user-add (msg/sender m) (msg/chat-id m))
                 (msg/reply m "✅ OK. I've set this chat as your private chat."))

      nil ; no default action
      )))