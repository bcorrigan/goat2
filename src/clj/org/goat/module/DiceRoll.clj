(ns org.goat.module.DiceRoll
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.format :as fmt]
            [clojure.string :as str]))

;; ============================================================================
;; Parsing Functions
;; ============================================================================

(defn- parse-term
  "Parse a single dice term like '10d6' or 'd20'.
   Returns {:count n :size m} or nil if invalid."
  [term]
  (let [trimmed (str/trim term)]
    (when (re-matches #"\d*d\d+" trimmed)
      (let [[count-str size-str] (str/split trimmed #"d")]
        (try
          (let [count (if (empty? count-str) 1 (Integer/parseInt count-str))
                size (Integer/parseInt size-str)]
            {:count count :size size})
          (catch NumberFormatException _
            nil))))))

(defn- parse-dice-notation
  "Parse dice notation like '10d6 + 5d20'.
   Returns vector of term maps [{:count n :size m} ...] or nil if invalid."
  [text]
  (when-not (str/blank? text)
    (let [terms (if (str/includes? text "+")
                  (str/split text #"\+")
                  [text])
          parsed (map parse-term terms)]
      (when (every? some? parsed)
        (vec parsed)))))

(defn- validate-roll
  "Check if the total number of dice is within the limit (≤100).
   Returns :ok or error keyword."
  [terms]
  (let [total (reduce + 0 (map :count terms))]
    (cond
      (> total 100) :too-many-dice
      :else :ok)))

(defn- estimate-size
  "Estimate the total number of dice in a roll string.
   Returns the count or throws exception."
  [text]
  (if-let [terms (parse-dice-notation text)]
    (reduce + 0 (map :count terms))
    (throw (Exception. "Error parsing roll"))))

;; ============================================================================
;; Rolling Functions
;; ============================================================================

(defn- roll-die
  "Roll a single die of given size.
   Returns {:size n :result r}."
  [size]
  {:size size
   :result (inc (rand-int size))})

(defn- roll-dice
  "Roll count dice of given size.
   Returns vector of die maps."
  [count size]
  (vec (repeatedly count #(roll-die size))))

(defn- calculate-total
  "Calculate total of all dice results in a group."
  [dice]
  (reduce + 0 (map :result dice)))

(defn- roll-groups
  "Roll all dice groups from parsed terms.
   Returns vector of group maps with :size, :count, :dice, and :total."
  [terms]
  (for [{:keys [count size]} terms]
    (let [dice (roll-dice count size)]
      {:size size
       :count count
       :dice dice
       :total (calculate-total dice)})))

;; ============================================================================
;; Formatting Functions
;; ============================================================================

(defn- format-dice-results
  "Format individual dice results like [3, 2, 5, 4]."
  [dice]
  (str "[" (str/join ", " (map :result dice)) "]"))

(defn- format-group
  "Format a single dice group like '• 10d6: [3, 2, 5, 4, 1, 6, 2, 3, 4, 2] = 32'."
  [f group]
  (str "  • " (:count group) "d" (:size group) ": "
       (format-dice-results (:dice group))
       " = " (fmt/bold f (:total group))))

(defn- format-roll
  "Format complete roll output with emoji and all groups."
  [f username groups]
  (let [grand-total (reduce + 0 (map :total groups))
        group-lines (map (partial format-group f) groups)
        total-line (str "  " (fmt/bold f (str "Total: " grand-total)))]
    (str "🎲 " username " rolled:\n"
         (str/join "\n" group-lines) "\n"
         total-line)))

(defn- format-coin-toss
  "Format coin toss result."
  [username result]
  (str "🪙 " username ": " result))

;; ============================================================================
;; Command Handlers
;; ============================================================================

(defn- handle-roll
  "Handle the roll command."
  [m]
  (let [text (msg/mod-text m)
        username (msg/sender m)]
    (try
      ;; Pre-check size
      (let [size (estimate-size text)]
        (if (> size 100)
          (msg/reply m "I'm not rolling that many dice, I'd be here all day!")
          ;; Parse and validate
          (if-let [terms (parse-dice-notation text)]
            (let [validation (validate-roll terms)]
              (if (= validation :ok)
                ;; Roll and format
                (let [groups (roll-groups terms)
                      output (format-roll (msg/fmt m) username groups)]
                  (msg/reply m output))
                ;; Validation failed
                (msg/reply m "It is so funny to make me try and throw more dice than exist in the universe.")))
            ;; Parse failed
            (msg/reply m "Sorry, I don't know how to do that."))))
      (catch NumberFormatException _
        (msg/reply m "I'm not rolling a sphere, sorry."))
      (catch Exception e
        (let [message (.getMessage e)]
          (cond
            (= message "Throw size too big")
            (msg/reply m "It is so funny to make me try and throw more dice than exist in the universe.")

            (= message "Dice size too big")
            (msg/reply m "I'm not rolling a sphere, sorry.")

            (= message "Error parsing roll")
            (msg/reply m "Sorry, I don't know how to do that.")

            :else
            (msg/reply m (str "An unidentified error occurred with roll: " message))))))))

(defn- handle-toss
  "Handle the coin toss command."
  [m]
  (let [username (msg/sender m)
        text (msg/mod-text m)]
    ;; Only handle if text is empty or contains "coin"
    (when (or (str/blank? text) (str/includes? text "coin"))
      (let [die (roll-die 2)
            result (if (= (:result die) 2) "Heads" "Tails")
            output (format-coin-toss username result)]
        (msg/reply m output)))))

;; ============================================================================
;; Module Definition
;; ============================================================================

(defmodule DiceRoll
  :commands [:roll :toss]

  (defn process-message [m]
    (case (msg/command m)
      :roll (handle-roll m)
      :toss (handle-toss m)
      nil)))
