(ns org.goat.module.Calc
  "Calculator module - wraps the jcalc Calculator with timeout protection.
  Evaluates mathematical expressions with a 2-second timeout to prevent
  long-running calculations from blocking."
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg])
  (:import [org.goat.jcalc Calculator CalculatorException]
           [java.util.concurrent TimeoutException ExecutionException
            CancellationException]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALCULATOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def calculator
  "Shared Calculator instance for evaluating expressions"
  (Calculator.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVALUATION LOGIC ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- evaluate-with-timeout
  "Evaluate expression with 2-second timeout.
  Returns result string or error message."
  [expression]
  (let [computation (future
                      (try
                        (.evaluate_equation calculator expression)
                        (catch CalculatorException e
                          {:error (.getLocalizedMessage e)})
                        (catch InterruptedException e
                          {:error "I'm sorry, where were we before we were so rudely interrupted?"})))]
    (try
      ;; Wait up to 2000ms for result
      (let [result (deref computation 2000 :timeout)]
        (if (= result :timeout)
          (do
            ;; Cancel the future to interrupt the calculation
            (future-cancel computation)
            "I'm not thinking that hard, wanker.")
          ;; Check if we got an error map or a string result
          (if (map? result)
            (:error result)
            result)))
      (catch CancellationException _
        "I've gone ahead and cancelled that computation for you.  Asshat.")
      (catch ExecutionException e
        (if-let [cause (.getCause e)]
          (.getLocalizedMessage cause)
          "An error occurred during calculation"))
      (catch Exception e
        (.getLocalizedMessage e)))))

(defn- format-reply
  "Format reply, prepending digit count if longer than 256 chars."
  [reply]
  (if (> (count reply) 256)
    (str (count reply) " digits:  " reply)
    reply))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESSAGE HANDLER ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-calc
  "Handle calc command - evaluate the expression and reply."
  [m]
  (let [expression (msg/mod-text m)]
    (if (empty? expression)
      (msg/reply m "Please provide an expression to calculate.")
      (let [result (evaluate-with-timeout expression)
            formatted (format-reply result)]
        (msg/reply m formatted)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE DEFINITION ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule Calc
  :commands [:calc]

  (defn process-message [m]
    (handle-calc m)))
