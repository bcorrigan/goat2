(ns org.goat.module.Define
  "Module for dictionary lookups using DICT protocol"
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.command-parser :as parser]
            [org.goat.util.dict-client :as dict]
            [org.goat.util.str :as str-util]
            [clojure.string :as str])
  (:import [org.goat Goat]
           [org.goat.core Constants]
           [java.net ConnectException UnknownHostException]))

;; Configuration
(def ^:private dict-host "dict.org")
(def ^:private dict-port 2628)

;; Connection Management

(defn- with-dict-connection
  "Execute function f with a DICT connection, handling errors gracefully.
   Returns result of f, or sends error message and returns nil"
  [m f]
  (try
    (let [conn (dict/connect dict-host dict-port)]
      (try
        (f conn)
        (finally
          (dict/close conn))))
    (catch UnknownHostException e
      (msg/reply m (str "Couldn't talk to dict server: host \"" dict-host "\" unknown"))
      nil)
    (catch ConnectException e
      (msg/reply m "Couldn't talk to dict server.")
      nil)))

;; Response Formatting

(defn- format-definition
  "Format a single definition for display with nice formatting"
  [def]
  (format "📖 %s%s%s (%s%s%s):\n%s"
          Constants/BOLD
          (str-util/escape-html (:word def))
          Constants/END_BOLD
          Constants/BOLD
          (:database-short def)
          Constants/END_BOLD
          (str-util/escape-html (:definition def))))

(defn- format-availability
  "Format availability message showing definition counts per dictionary"
  [defs]
  (when (> (count defs) 1)
    (let [grouped (group-by :database-short defs)
          formatted (str/join " • "
                             (map (fn [[db defs]]
                                    (str Constants/BOLD db Constants/END_BOLD
                                         " (" (count defs) ")"))
                                  grouped))]
      (str "📚 " formatted))))

(defn- format-suggestions
  "Format spelling suggestions with nice formatting"
  [matches]
  (when (seq matches)
    (let [words (->> matches
                     (map :match)
                     (map #(str/replace % "\"" ""))
                     distinct
                     (take 10)  ; Limit to 10 suggestions
                     (map str-util/escape-html)
                     (map #(str Constants/BOLD % Constants/END_BOLD))
                     (str/join ", "))]
      (str "✨ Did you mean: " words "?"))))

;; Command Handlers

(defn- handle-oed
  "Generate OED URL for a word"
  [m]
  (let [word (msg/mod-text m)]
    (if (str/blank? word)
      (msg/reply m "❓ Er, look up what, exactly?")
      (msg/reply m (str "📕 Oxford English Dictionary: http://dictionary.oed.com/cgi/findword?query_type=word&queryword="
                       (str/replace word " " "%20"))))))

(defn- handle-randef
  "Random definition (not yet implemented)"
  [m]
  (msg/reply m "🎲 Not implemented yet, please stand by..."))

(defn- handle-dictionaries
  "List all available dictionaries"
  [m]
  (with-dict-connection m
    (fn [conn]
      (try
        (let [dbs (dict/get-databases conn)
              db-names (map :short dbs)
              formatted-names (map #(str Constants/BOLD % Constants/END_BOLD) db-names)
              result (str "📚 Available dictionaries:\n"
                         (str/join ", " formatted-names))]
          (msg/reply m result))
        (catch Exception e
          (msg/reply m "⚠️ Couldn't talk to dict server."))))))

(defn- handle-dictionary
  "Show information about a specific dictionary"
  [m]
  (let [code (str/trim (msg/mod-text m))]
    (cond
      (str/blank? code)
      (msg/reply m "❓ Which dictionary did you want to know about?")

      :else
      (with-dict-connection m
        (fn [conn]
          (try
            (let [db-info (dict/get-db-info conn code)
                  escaped-info (str-util/escape-html db-info)
                  line (str "📗 " Constants/BOLD code Constants/END_BOLD ":\n" escaped-info)]
              (msg/reply m line))
            (catch IllegalArgumentException e
              ;; Database not found - show available databases
              (let [dbs (dict/get-databases conn)
                    db-names (map :short dbs)
                    formatted-names (map #(str Constants/BOLD % Constants/END_BOLD) db-names)
                    all-dbs (str/join ", " formatted-names)
                    line (str "❌ Dictionary " Constants/BOLD code Constants/END_BOLD " not found.\n"
                             "📚 Available dictionaries: " all-dbs)]
                (msg/reply m line)))
            (catch Exception e
              (msg/reply m (str "⚠️ Error: " (.getMessage e))))))))))

(defn- handle-define
  "Main definition lookup handler"
  [m thesaurus?]
  (let [text (msg/mod-text m)
        parsed (parser/parse-parameters text)
        params (:params parsed)
        ;; Get dictionary parameter
        dict-param (parser/get-param params :dict :dictionary)
        dictionary (cond
                     thesaurus? "moby-thesaurus"
                     dict-param dict-param
                     :else "*")
        ;; Get number parameter
        num-param (parser/get-param params :num :number)
        num (parser/parse-int num-param 1)
        ;; Get word to define
        word-text (:text parsed)
        word (when-not (str/blank? word-text) word-text)]

    ;; Validate number parameter
    (cond
      (<= num 0)
      (msg/reply m "😏 Very funny. Go back to your cubicle, nerd.")

      (nil? word)
      (msg/reply m "❓ Er, define what, exactly?")

      (str/blank? word)
      (msg/reply m "🤔 I can't decide which word I'm supposed to define for you.")

      ;; DICT server lookup
      :else
      (with-dict-connection m
        (fn [conn]
          (try
            ;; Get available databases for validation
            (let [all-dbs (dict/get-databases conn)
                  db-names (set (map :short all-dbs))]

              ;; Validate dictionary if specified
              (when (and (not= dictionary "*")
                        (not (contains? db-names dictionary)))
                (msg/reply m (str "❌ " Constants/BOLD dictionary Constants/END_BOLD " is not a valid dictionary."))
                (handle-dictionaries m)
                (throw (ex-info "Invalid dictionary" {:dictionary dictionary})))

              ;; Get definitions and matches
              (let [definitions (dict/get-definitions conn [dictionary] word)
                    matches (dict/get-matches conn [dictionary] "." word)]

                (cond
                  ;; No definitions found
                  (empty? definitions)
                  (let [reply (str "❌ No definitions found for " Constants/BOLD word Constants/END_BOLD
                                  (when (not= dictionary "*")
                                    (str " in dictionary " Constants/BOLD dictionary Constants/END_BOLD))
                                  ".")]
                    (if (empty? matches)
                      (msg/reply m (str reply "\nCouldn't find any alternate spelling suggestions."))
                      (msg/reply m (str reply "\n" (format-suggestions matches)))))

                  ;; Requested definition number too high
                  (> num (count definitions))
                  (let [line (str "❌ I don't have " num " definitions for " Constants/BOLD word Constants/END_BOLD
                                 (when (not= dictionary "*")
                                   (str " in dictionary " Constants/BOLD dictionary Constants/END_BOLD))
                                 ".")]
                    (msg/reply m line))

                  ;; Return requested definition
                  :else
                  (let [def (nth definitions (dec num))
                        text (format-definition def)]
                    (msg/reply m text)
                    ;; Show availability if multiple definitions
                    (when-let [avail (format-availability definitions)]
                      (msg/reply m avail))))))
            (catch clojure.lang.ExceptionInfo e
              ;; Already handled - validation error
              nil)
            (catch IllegalArgumentException e
              (msg/reply m (str "⚠️ Error: " (.getMessage e))))
            (catch Exception e
              (msg/reply m (str "⚠️ Something went wrong: " (.getMessage e))))))))))

;; Module Definition

(defmodule Define
  :commands [:define :randef :dictionaries :dictionary :oed :thesaurus]

  (defn process-channel-message [m]
    (case (msg/command m)
      :define (handle-define m false)
      :thesaurus (handle-define m true)
      :dictionaries (handle-dictionaries m)
      :dictionary (handle-dictionary m)
      :oed (handle-oed m)
      :randef (handle-randef m)
      nil)))
