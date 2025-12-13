(ns org.goat.module.Countdown
  "Number puzzle game from the Channel 4 TV show Countdown.
  Players have 60 seconds to reach a target number (101-999) using 6 source
  numbers and basic math operators (+, -, *, /, parentheses)."
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.message-parse :as msg-parse]
            [clojure.string :as str])
  (:import [org.goat.core Constants]
           [org.goat.jcalc Calculator CalculatorException]
           [org.goat.util CountdownSolver]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def state
  "Map of chat-key to game state.
  Each game state contains:
    :game-on - boolean, is game active
    :target-number - int, the target to reach (101-999)
    :source-numbers - vector of 6 ints available for calculations
    :best-answer - map with :value :username :expression
    :best-possible - int, best possible answer from solver
    :chat-id - long, chat ID for replies during timer
    :timer-future - future, handle to cancel timer"
  (atom {}))

(def calculator
  "Shared Calculator instance for evaluating expressions"
  (Calculator.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS - POOL MANAGEMENT ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- initialize-pools
  "Create and shuffle pools of big and small numbers.
  Returns map with :big-pool and :small-pool."
  []
  {:big-pool (shuffle [25 50 75 100])
   :small-pool (shuffle [1 2 3 4 5 6 7 8 9 10
                         1 2 3 4 5 6 7 8 9 10])})

(defn- draw-numbers
  "Draw 6 numbers from pools.
  80% chance: 1 big + 5 small
  20% chance: 2 big + 4 small
  Returns vector of 6 numbers."
  [pools]
  (let [big-count (if (< (rand) 0.8) 1 2)
        small-count (- 6 big-count)]
    (vec (concat (take big-count (:big-pool pools))
                 (take small-count (:small-pool pools))))))

(defn- generate-target
  "Generate random target number between 101 and 999 inclusive."
  []
  (+ 101 (rand-int 899)))

(defn- format-numbers
  "Format vector of numbers as space-separated string."
  [numbers]
  (str/join " " numbers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS - VALIDATION ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- extract-numbers
  "Extract all numbers from expression using regex.
  Returns sequence of integers."
  [expr]
  (map #(Integer/parseInt %) (re-seq #"\d+" expr)))

(defn- numbers-valid?
  "Check that user used only available source numbers, each at most once.
  Uses immutable approach with loop/recur."
  [user-nums source-nums]
  (loop [remaining (vec source-nums)
         to-check user-nums]
    (if (empty? to-check)
      true
      (let [num (first to-check)
            idx (.indexOf remaining num)]
        (if (>= idx 0)
          (recur (assoc remaining idx nil) (rest to-check))
          false)))))

(defn- expression-valid?
  "Check expression contains only allowed operators: + - * / ( )
  Strip all allowed characters and check nothing remains."
  [expr]
  (let [cleaned (-> expr
                    (str/replace #"\d" "")       ; strip all numbers
                    (str/replace #"/" "")        ; strip /
                    (str/replace #"\*" "")       ; strip *
                    (str/replace #"\+" "")       ; strip +
                    (str/replace #"-" "")        ; strip -
                    (str/replace #"\)" "")       ; strip )
                    (str/replace #"\(" "")       ; strip (
                    (str/replace #"\s" ""))]     ; strip spaces
    (empty? cleaned)))

(defn- validate-answer
  "Validate that answer uses correct numbers and operators.
  Returns {:valid true} or {:valid false :error \"...\"}"
  [expr source-nums]
  (let [user-nums (extract-numbers expr)]
    (cond
      (not (numbers-valid? user-nums source-nums))
      {:valid false :error "You used a number not in the selection!"}

      (not (expression-valid? expr))
      {:valid false :error "Invalid operators in expression"}

      :else
      {:valid true})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS - EVALUATION ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- evaluate-answer
  "Evaluate mathematical expression using Calculator.
  Returns {:success true :value int} or {:success false :error \"...\"}"
  [expr]
  (try
    (let [result (.evaluate_equation calculator expr)
          value (Integer/parseInt result)]
      {:success true :value value})
    (catch NumberFormatException _
      {:success false :error "You used a formula that resulted in a non-int answer. This is not allowed!"})
    (catch CalculatorException e
      {:success false :error (.getLocalizedMessage e)})
    (catch InterruptedException _
      {:success false :error "I was interrupted before I could calculate that"})))

(defn- distance-from-target
  "Calculate absolute distance between value and target."
  [value target]
  (Math/abs (- target value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- game-active?
  "Check if a game is currently active for this chat."
  [chat-key]
  (boolean (get-in @state [chat-key :game-on])))

(defn- any-game-active?
  "Check if any game is active in any chat."
  []
  (boolean (some :game-on (vals @state))))

(defn- get-game-state
  "Get entire game state for a chat."
  [chat-key]
  (get @state chat-key))

(defn- start-new-game!
  "Initialize new game state for chat.
  Returns map with :target-number and :source-numbers for display."
  [chat-key chat-id]
  (let [pools (initialize-pools)
        source-numbers (draw-numbers pools)
        target (generate-target)
        best-val (CountdownSolver/getBestVal (int-array source-numbers) target)]
    (swap! state assoc chat-key
           {:game-on true
            :chat-id chat-id
            :target-number target
            :source-numbers source-numbers
            :best-answer nil
            :best-possible best-val
            :timer-future nil})
    {:target-number target
     :source-numbers source-numbers}))

(defn- update-best-answer!
  "Update best answer in game state."
  [chat-key value username expr]
  (swap! state assoc-in [chat-key :best-answer]
         {:value value
          :username username
          :expression expr}))

(defn- clear-game!
  "Cancel timer and remove all game state for chat."
  [chat-key]
  (when-let [timer (get-in @state [chat-key :timer-future])]
    (future-cancel timer))
  (swap! state dissoc chat-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIMER MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- reply-to-game
  "Send reply to game's chat using stored chat-id.
  Creates new Message for timer replies."
  [chat-key text]
  (when-let [chat-id (get-in @state [chat-key :chat-id])]
    (let [m (msg-parse/create-message :chat-id chat-id :sender "goat" :private? false :text "")]
      (msg/reply m text))))

(defn- finalize-game
  "End game and display solution/winner.
  Called when timer expires or best possible answer is achieved."
  [chat-key]
  (let [game (get-game-state chat-key)
        best-answer (:best-answer game)
        target (:target-number game)
        source (:source-numbers game)
        best-possible (:best-possible game)]

    ;; Mark game as no longer active
    (swap! state assoc-in [chat-key :game-on] false)

    (if (nil? best-answer)
      ;; No answers submitted
      (reply-to-game chat-key
                     (str "Nobody got an answer. The best answer was: "
                          (CountdownSolver/Solve (int-array source) target)))

      ;; Someone answered
      (if (= (:value best-answer) best-possible)
        ;; Best possible achieved
        (let [reply-text (str Constants/BOLD (:username best-answer) Constants/END_BOLD
                              " has won with " (:value best-answer) "!")]
          (reply-to-game chat-key
                         (if (not= best-possible target)
                           (str reply-text " This was the best possible answer.")
                           reply-text)))

        ;; Suboptimal answer
        (reply-to-game chat-key
                       (str "The best answer was " (:value best-answer)
                            " by " (:username best-answer) "."
                            " But the best possible answer was: "
                            (CountdownSolver/Solve (int-array source) target)))))

    (clear-game! chat-key)))

(defn- start-game-timer!
  "Start 60-second timer: 50s active + 10s warning.
  Stores future in state for cancellation."
  [chat-key]
  (let [timer (future
                ;; 50 second active period
                (Thread/sleep 50000)
                (when (game-active? chat-key)
                  (reply-to-game chat-key
                                 (str Constants/BOLD "10 secs.." Constants/END_BOLD))

                  ;; 10 second warning period
                  (Thread/sleep 10000)

                  (when (game-active? chat-key)
                    (finalize-game chat-key))))]
    (swap! state assoc-in [chat-key :timer-future] timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME FLOW HANDLERS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-start-game
  "Handle starting a new game.
  Check not already playing, initialize, start timer, display numbers."
  [m chat-key]
  (if (game-active? chat-key)
    (msg/reply m "We're already playing a game, smart one.")
    (let [chat-id (msg/chat-id m)
          {:keys [target-number source-numbers]} (start-new-game! chat-key chat-id)]
      (start-game-timer! chat-key)
      (msg/reply m
                 (str Constants/UNDERLINE Constants/BOLD "***"
                      Constants/END_BOLD Constants/END_UNDERLINE
                      " New Numbers: " Constants/BOLD
                      (format-numbers source-numbers) Constants/END_BOLD
                      " Target: " Constants/BOLD target-number Constants/END_BOLD)))))

(defn- handle-answer-attempt
  "Handle user's answer attempt during active game.
  Validate, evaluate, track best, possibly end game."
  [m chat-key attempt username]
  (let [game (get-game-state chat-key)
        source (:source-numbers game)
        target (:target-number game)
        validation (validate-answer attempt source)]

    (if-not (:valid validation)
      ;; Invalid answer format
      (msg/reply m (:error validation))

      ;; Valid format - try to evaluate
      (let [eval-result (evaluate-answer attempt)]
        (if-not (:success eval-result)
          ;; Evaluation failed
          (msg/reply m (:error eval-result))

          ;; Success - check if it's the best answer so far
          (let [value (:value eval-result)
                current-best (:best-answer game)
                current-dist (if current-best
                               (distance-from-target (:value current-best) target)
                               Integer/MAX_VALUE)
                this-dist (distance-from-target value target)]

            (when (< this-dist current-dist)
              ;; New best answer
              (update-best-answer! chat-key value username attempt)

              (if (= value (:best-possible game))
                ;; Perfect answer - end game immediately
                (finalize-game chat-key)

                ;; Good but not perfect - acknowledge
                (msg/reply m
                           (str Constants/BOLD username Constants/END_BOLD
                                " has the best answer so far: " value
                                ". Just " this-dist " off the target of "
                                target "!"))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE DEFINITION ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule Countdown
  :commands [:countdown]
  :receive-messages :unclaimed

  (defn process-channel-message [m]
    (let [chat-key (keyword (str (msg/chat-id m)))]
      (if (= (msg/command m) :countdown)
        ;; Start new game
        (handle-start-game m chat-key)

        ;; Check if we're in an active game (treat as answer attempt)
        (when (game-active? chat-key)
          (handle-answer-attempt m chat-key (msg/get-text m) (msg/sender m)))))))
