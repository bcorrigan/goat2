(ns org.goat.core.init
  "Clojure module system initialization.

  This namespace is responsible for initializing the pure Clojure module
  system, loading all modules, and starting the dispatcher.

  Called from Java Goat.java constructor via reflection."
  (:require [org.goat.core.dispatcher :as dispatcher]
            [org.goat.core.registry :as registry]
            [clojure.string :as str]
            ;; Require all module namespaces to trigger registration
            ;; Each module's defmodule-clj macro auto-registers when loaded
            [org.goat.module.CoreCommands]
            [org.goat.module.DiceRoll]
            [org.goat.module.Weather]
            [org.goat.module.Calc]
            [org.goat.module.Capture]
            [org.goat.module.Countdown]
            [org.goat.module.WordStats]
            [org.goat.module.Freezer]
            [org.goat.module.Remind]
            [org.goat.module.Define]
            [org.goat.module.More]
            [org.goat.module.Wordle])
  (:import [org.goat Goat]))

(defn init!
  "Initialize the Clojure module system.

   Steps:
   1. Modules are already registered via namespace loading (defmodule-clj auto-registers)
   2. Start the core.async dispatcher
   3. Log module count and details

   This function is called once from Java Goat constructor."
  []
  (println "\n========================================")
  (println "Initializing Clojure module system...")
  (println "========================================")

  ;; Modules are already registered via namespace loading
  (let [modules (registry/get-modules)
        commands (registry/get-commands)
        {:keys [all unclaimed commands-type]} (registry/get-modules-by-type)]

    (println (format "\n✓ Registered %d modules:" (count modules)))
    (doseq [m modules]
      (println (format "  • %s [%s] - commands: %s"
                       (:module-name m)
                       (name (:message-type m))
                       (if (seq (:commands m))
                         (str/join ", " (map name (:commands m)))
                         "none"))))

    (println (format "\n✓ Total commands registered: %d" (count commands)))

    (when (seq all)
      (println (format "  - Modules receiving ALL messages: %s"
                       (str/join ", " (map :module-name all)))))

    (when (seq unclaimed)
      (println (format "  - Modules receiving UNCLAIMED messages: %s"
                       (str/join ", " (map :module-name unclaimed)))))

    (when (seq commands-type)
      (println (format "  - Modules receiving COMMAND messages: %d modules"
                       (count commands-type))))

    ;; Start dispatcher
    (println "\n✓ Starting core.async dispatcher...")
    (dispatcher/start! Goat/inqueue)

    (println "\n========================================")
    (println "Clojure module system initialized!")
    (println "========================================\n")))

(defn -init
  "Called via reflection from Java: (init-method.invoke(null))"
  []
  (init!))
