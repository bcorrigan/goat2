(ns org.goat.core.macros
  "Macros for simplifying Goat module creation"
  (:require [org.goat.core.message :as msg]
            [org.goat.core.module-protocol :as mp]
            [org.goat.core.registry :as registry]
            [clojure.string :as str]))

(defn- extract-options
  "Extract options from defmodule body, returning [options remaining-body]"
  [body]
  (let [options-map (loop [remaining body
                           opts {}]
                      (if (and (seq remaining)
                               (keyword? (first remaining))
                               (seq (rest remaining)))
                        (recur (drop 2 remaining)
                               (assoc opts (first remaining) (second remaining)))
                        opts))
        options-count (* 2 (count options-map))
        remaining-body (drop options-count body)]
    [options-map remaining-body]))

(defn- find-defn-by-name
  "Find a defn form by its function name"
  [body fname]
  (first (filter #(and (seq? %)
                       (= 'defn (first %))
                       (= fname (second %)))
                 body)))

(defmacro defmodule
  "Define a pure Clojure Goat module.

  Generates pure Clojure code implementing Module protocol.
  Modules are registered in the static registry at namespace load time.

  Usage:
  (defmodule ModuleName
    :commands [:cmd1 :cmd2]
    :wants-private true
    :receive-messages :all  ; or :unclaimed or :commands (default)

    (defn process-message [m]
      ; m is automatically wrapped - use (msg/command m), (msg/reply m text), etc.
      (case (msg/command m)
        :cmd1 (msg/reply m \"Hello!\")
        :cmd2 (msg/reply m \"World!\"))))"
  [module-name & body]
  (let [[options remaining-body] (extract-options body)
        commands (or (:commands options) [])
        wants-private (get options :wants-private true)
        receive-messages (or (:receive-messages options) :commands)
        message-type (case receive-messages
                       :all :all
                       :unclaimed :unclaimed
                       :commands :commands)

        ; Find the process-message function
        process-msg-fn (find-defn-by-name remaining-body 'process-message)
        metadata-name (symbol (str (name module-name) "-metadata"))

        ; Extract and wrap message parameter
        wrapped-process-msg (when process-msg-fn
                             (let [[_ _ [msg-param] & fn-body] process-msg-fn]
                               `(defn ~'process-message [~msg-param]
                                  (let [~msg-param (msg/wrap-message ~msg-param)]
                                    ~@fn-body))))]

    `(do
       ; Define process-message function first (exposed for testing)
       ~wrapped-process-msg

       ; Include helper functions
       ~@(filter #(not (and (seq? %)
                           (= 'defn (first %))
                           (= 'process-message (second %))))
                remaining-body)

       ; Create module metadata with process-message as process-fn
       (def ~metadata-name
         (mp/map->ModuleMetadata
           {:module-name ~(name module-name)
            :commands ~(vec commands)
            :message-type ~message-type
            :wants-private ~wants-private
            :process-fn ~'process-message}))

       ; Register module when namespace loads
       (registry/register-module! ~metadata-name))))