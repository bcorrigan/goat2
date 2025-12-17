(ns org.goat.core.config
  "Bot configuration and secrets management.

   Provides access to bot configuration like Telegram token and bot name.
   Loads sensitive data from password file."
  (:require [clojure.java.io :as io])
  (:import [java.util Properties]
           [java.io FileInputStream]))

(def ^:private passwords-file "config/passwords.properties")
(def ^:private goat-props-file "config/goat.properties")

(defn- load-properties-file
  "Load a Java properties file and return as a Clojure map.

   Returns empty map if file doesn't exist or can't be loaded."
  [filename]
  (let [props (Properties.)]
    (try
      (with-open [input (FileInputStream. filename)]
        (.load props input))
      (into {} props)
      (catch Exception e
        (println "WARNING: Could not load properties from file" filename)
        (.printStackTrace e)
        {}))))

(defn- load-secrets
  "Load secrets from passwords.properties file.

   Returns a map of all password/secret values."
  []
  (load-properties-file passwords-file))

(defn get-secret
  "Get a secret value by key.

   Supports both string keys and keyword keys:
   (get-secret \"telegram.token\")
   (get-secret :telegram-token)

   Keyword keys are converted to dot-notation:
   :telegram-token → 'telegram.token'
   :api-key → 'api.key'"
  [key]
  (let [secrets (load-secrets)
        key-str (if (keyword? key)
                  (-> (name key)
                      (clojure.string/replace #"-" "."))
                  key)]
    (get secrets key-str)))

(defonce config
  (atom {:bot-name "goat"
         :token nil
         :test-mode? false}))

(defn get-token
  "Get the Telegram bot token."
  []
  (:token @config))

(defn get-bot-name
  "Get the bot name (default: 'goat')."
  []
  (:bot-name @config))

(defn test-mode?
  "Returns true if running in test mode (CLI instead of Telegram)."
  []
  (:test-mode? @config))

(defn set-token!
  "Set the Telegram bot token."
  [token]
  (swap! config assoc :token token))

(defn set-bot-name!
  "Set the bot name."
  [name]
  (swap! config assoc :bot-name name))

(defn set-test-mode!
  "Enable or disable test mode."
  [enabled?]
  (swap! config assoc :test-mode? enabled?))

(defn load-config!
  "Load configuration from password file and environment.

   Loads:
   - Telegram token from password file (key: 'telegram.token')
   - Bot name from password file (key: 'nick', default: 'goat')"
  []
  (let [token (get-secret :telegram-token)
        nick (get-secret :nick)]
    (set-token! token)
    (when nick
      (set-bot-name! nick))))

;; Load config when namespace is required
(load-config!)

