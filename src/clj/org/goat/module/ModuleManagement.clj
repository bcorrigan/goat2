(ns org.goat.module.ModuleManagement
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.registry :as registry]
            [org.goat.db.module-settings :as db]
            [clojure.string :as str]))

(defn- validate-module-name
  "Check if module name exists in registry"
  [module-name]
  (registry/get-module-by-name module-name))

(defn- handle-ignore
  "Handle chatignore command"
  [m module-name]
  (let [chat-id (msg/chat-id m)
        username (msg/sender m)]
    (cond
      (str/blank? module-name)
      (msg/reply m "Usage: chatignore <ModuleName>")

      (not (validate-module-name module-name))
      (msg/reply m (format "❌ Module '%s' not found. Use 'chatlist all' to see available modules." module-name))

      (db/is-module-ignored? chat-id module-name)
      (msg/reply m (format "ℹ️ Module '%s' is already ignored in this chat." module-name))

      :else
      (do
        (db/ignore-module! chat-id module-name username)
        (msg/reply m (format "✅ Module '%s' will now ignore messages in this chat." module-name))))))

(defn- handle-unignore
  "Handle chatunignore command"
  [m module-name]
  (let [chat-id (msg/chat-id m)]
    (cond
      (str/blank? module-name)
      (msg/reply m "Usage: chatunignore <ModuleName>")

      (not (validate-module-name module-name))
      (msg/reply m (format "❌ Module '%s' not found." module-name))

      (not (db/is-module-ignored? chat-id module-name))
      (msg/reply m (format "ℹ️ Module '%s' is not currently ignored in this chat." module-name))

      :else
      (do
        (db/unignore-module! chat-id module-name)
        (msg/reply m (format "✅ Module '%s' will now receive messages in this chat." module-name))))))

(defn- handle-list
  "Handle chatlist command - show ignored modules or all modules"
  [m args]
  (let [chat-id (msg/chat-id m)
        show-all? (= (str/trim (str/lower-case args)) "all")]
    (if show-all?
      ;; Show all available modules
      (let [modules (registry/get-modules)
            module-names (sort (map :module-name modules))]
        (msg/reply m
          (str "📋 <b>Available Modules:</b>\n\n"
               (str/join "\n" (map #(str "  • " %) module-names)))))
      ;; Show ignored modules in this chat
      (let [ignored (db/get-ignored-modules chat-id)]
        (if (empty? ignored)
          (msg/reply m "✅ No modules are ignored in this chat.")
          (msg/reply m
            (str "🚫 <b>Ignored Modules in This Chat:</b>\n\n"
                 (str/join "\n" (map #(str "  • " %) ignored)))))))))

(defmodule ModuleManagement
  :commands [:chatignore :chatunignore :chatlist]
  :receive-messages :commands

  (defn process-message [m]
    (let [command (msg/command m)
          args (str/trim (msg/mod-text m))]
      (case command
        :chatignore (handle-ignore m args)
        :chatunignore (handle-unignore m args)
        :chatlist (handle-list m args)))))
