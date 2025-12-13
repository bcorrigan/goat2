(ns org.goat.core.config
  "Bot configuration - replaces BotStats.java.

   Provides access to bot configuration like Telegram token and bot name.
   Loads sensitive data from password file."
  (:import [org.goat.util Passwords]))

;; =============================================================================
;; Configuration State
;; =============================================================================

(defonce config
  (atom {:bot-name "goat"
         :token nil
         :test-mode? false}))

;; =============================================================================
;; Configuration Access
;; =============================================================================

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

;; =============================================================================
;; Initialization
;; =============================================================================

(defn load-config!
  "Load configuration from password file and environment.

   Loads:
   - Telegram token from password file (key: 'telegram.token')
   - Bot name from password file (key: 'nick', default: 'goat')"
  []
  (let [token (Passwords/getPassword "telegram.token")
        nick (Passwords/getPassword "nick")]
    (set-token! token)
    (when nick
      (set-bot-name! nick))))

;; Load config when namespace is required
(load-config!)

;; =============================================================================
;; Examples
;; =============================================================================

(comment
  ;; Get current config
  @config

  ;; Get token
  (get-token)

  ;; Get bot name
  (get-bot-name)

  ;; Set test mode
  (set-test-mode! true)
  (test-mode?)

  ;; Reload config
  (load-config!)
  )
