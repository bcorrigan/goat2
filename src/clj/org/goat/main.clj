(ns org.goat.main
  "Main entry point for the Goat bot.

   Replaces Goat.java with idiomatic Clojure.
   Handles initialization, argument parsing, and startup."
  (:require [org.goat.core.init :as init]
            [org.goat.core.connection :as conn]
            [org.goat.core.config :as config]
            [clojure.tools.logging :as log])
  (:import [java.util Locale])
  (:gen-class))

;; =============================================================================
;; Command Line Arguments
;; =============================================================================

(defn parse-args
  "Parse command line arguments.

   Supported arguments:
   - -name <name>  : Set bot name
   - -test         : Run in CLI test mode
   - -help         : Show help"
  [args]
  (loop [remaining args
         opts {}]
    (if (empty? remaining)
      opts
      (let [arg (first remaining)
            rest-args (rest remaining)]
        (case arg
          "-name"
          (if (seq rest-args)
            (recur (rest rest-args)
                   (assoc opts :bot-name (first rest-args)))
            (do
              (println "ERROR: -name requires an argument")
              (assoc opts :show-help true)))

          "-test"
          (recur rest-args
                 (assoc opts :test-mode true))

          "-help"
          (recur rest-args
                 (assoc opts :show-help true))

          ;; Unknown argument
          (do
            (println "ERROR: Unknown argument:" arg)
            (recur rest-args
                   (assoc opts :show-help true))))))))

(defn show-help
  "Display command line help."
  []
  (println "Usage: java -jar goat.jar [options]")
  (println)
  (println "Options:")
  (println "  -name <name>   Set the bot's name [default: goat]")
  (println "  -test          Run in CLI test mode (stdin/stdout instead of Telegram)")
  (println "  -help          Show this help message"))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn initialize!
  "Initialize the bot system.

   Steps:
   1. Set locale to UK
   2. Load configuration (token, bot name, etc.)
   3. Initialize Clojure module system
   4. Start connection (Telegram or CLI test mode)"
  [opts]
  ;; Set locale
  (Locale/setDefault Locale/UK)
  (log/info "Locale set to UK")

  ;; Apply command-line options
  (when (:bot-name opts)
    (config/set-bot-name! (:bot-name opts)))

  (when (:test-mode opts)
    (config/set-test-mode! true))

  ;; Initialize module system
  (log/info "Initializing Clojure module system...")
  (init/init!)

  ;; Start connection
  (if (config/test-mode?)
    (do
      (println "\n=== CLI Test Mode ===")
      (println "Type commands directly (e.g., 'roll 3d6' or 'goat, wordle')")
      (println "Commands can be with or without 'goat,' prefix")
      (println "Press Ctrl+C to exit")
      (println "=====================\n")
      (flush)

      (if (conn/start-cli-connection :debug? false)
        (log/info "CLI test mode started successfully")
        (do
          (println "Failed to start CLI test mode")
          (System/exit 1))))

    (do
      (print "Connecting to Telegram... ")
      (flush)
      (if (conn/start-connection)
        (do
          (println "We appear to be connected.\n")
          (log/info "Bot started successfully"))
        (do
          (println "Failed to connect to Telegram")
          (System/exit 1))))))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn -main
  "Main entry point for the Goat bot."
  [& args]
  (let [opts (parse-args (or args []))]
    (if (:show-help opts)
      (do
        (show-help)
        (System/exit 0))
      (do
        (initialize! opts)
        ;; Keep main thread alive (core.async go-loops are daemon threads)
        (try
          (.. Thread currentThread join)
          (catch InterruptedException e
            (log/info "Bot shutting down...")
            (System/exit 0)))))))

;; =============================================================================
;; Examples
;; =============================================================================

(comment
  ;; Parse args
  (parse-args ["-name" "testbot"])
  ;; => {:bot-name "testbot"}

  (parse-args ["-test"])
  ;; => {:test-mode true}

  (parse-args ["-help"])
  ;; => {:show-help true}

  (parse-args ["-name" "mybot" "-test"])
  ;; => {:bot-name "mybot", :test-mode true}

  ;; Show help
  (show-help)
  )
