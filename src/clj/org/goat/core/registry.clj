(ns org.goat.core.registry
  "Static module registry for all loaded modules.

  Replaces the module tracking functionality from BotStats.java and
  ModuleController.java. All modules are registered at startup (no dynamic
  loading/unloading)."
  (:require [org.goat.core.module-protocol :as mp]))

;; Atom holding vector of ModuleMetadata records
(defonce ^:private modules-atom (atom []))

;; Atom holding set of all registered command keywords
(defonce ^:private commands-atom (atom #{}))

(defn register-module!
  "Register a module at startup. Thread-safe.
   Called automatically by defmodule macro when namespace is loaded."
  [module-metadata]
  {:pre [(some? module-metadata)
         (map? module-metadata)]}
  (swap! modules-atom conj module-metadata)
  (swap! commands-atom into (:commands module-metadata))
  nil)

(defn get-modules
  "Return all registered modules as a vector of ModuleMetadata records"
  []
  @modules-atom)

(defn get-commands
  "Return set of all registered command keywords"
  []
  @commands-atom)

(defn get-modules-by-type
  "Return map of {:all [...] :unclaimed [...] :commands [...]} grouping
   modules by their message type preference"
  []
  (group-by :message-type (get-modules)))

(defn get-module-by-name
  "Return module with given name, or nil if not found"
  [module-name]
  (first (filter #(= (:module-name %) module-name) (get-modules))))

(defn module-count
  "Return count of registered modules"
  []
  (count @modules-atom))

(defn command-registered?
  "Return true if command keyword is registered by any module"
  [cmd-keyword]
  (contains? @commands-atom cmd-keyword))

(defn clear-registry!
  "Clear all modules - for testing only"
  []
  (reset! modules-atom [])
  (reset! commands-atom #{})
  nil)
