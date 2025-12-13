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

(defn- transform-message-fn
  "Transform a function definition to auto-wrap the message parameter"
  [defn-form java-method-name]
  (let [[_ fn-name [msg-param] & fn-body] defn-form]
    `(defn ~(symbol (str "-" java-method-name)) [~'this ~msg-param]
       (let [~msg-param (msg/wrap-message ~msg-param)]
         ~@fn-body))))

(defn- generate-delegation
  "Generate a delegation from one method to another"
  [from-method to-method]
  `(defn ~(symbol (str "-" from-method)) [~'this ~'m]
     (~(symbol (str "-" to-method)) ~'this ~'m)))

(defmacro defmodule
  "Define a pure Clojure Goat module.

  Generates pure Clojure code implementing IModule protocol.
  Modules are registered in the static registry at namespace load time.

  Usage:
  (defmodule ModuleName
    :commands [:cmd1 :cmd2]
    :wants-private true
    :receive-messages :all  ; or :unclaimed or :commands (default)

    (defn process-channel-message [m]
      ; m is automatically wrapped - use (msg/command m), (msg/reply m text), etc.
      (case (msg/command m)
        :cmd1 (msg/reply m \"Hello!\")
        :cmd2 (msg/reply m \"World!\")))

    ; process-private-message auto-implemented if not provided
    )"
  [module-name & body]
  (let [[options remaining-body] (extract-options body)
        commands (or (:commands options) [])
        wants-private (get options :wants-private true)
        receive-messages (or (:receive-messages options) :commands)
        ; Map to Clojure keywords instead of Java constants
        message-type (case receive-messages
                       :all :all
                       :unclaimed :unclaimed
                       :commands :commands)

        ; Find function definitions
        channel-fn (find-defn-by-name remaining-body 'process-channel-message)
        private-fn (find-defn-by-name remaining-body 'process-private-message)

        ; Generate dispatch function name
        dispatch-fn-name (symbol (str (name module-name) "-dispatch"))
        metadata-name (symbol (str (name module-name) "-metadata"))

        ; Extract function body and parameter
        channel-impl (when channel-fn
                       (let [[_ _ [msg-param] & fn-body] channel-fn]
                         `(fn [~'msg]
                            (let [~msg-param (msg/wrap-message ~'msg)]
                              ~@fn-body))))

        private-impl (when private-fn
                       (let [[_ _ [msg-param] & fn-body] private-fn]
                         `(fn [~'msg]
                            (let [~msg-param (msg/wrap-message ~'msg)]
                              ~@fn-body))))

        ; Auto-delegation - if only one handler defined, use it for both
        final-dispatch (cond
                         (and channel-impl private-impl)
                         `(fn [msg#]
                            (if (.isPrivate msg#)
                              (~private-impl msg#)
                              (~channel-impl msg#)))

                         channel-impl
                         channel-impl

                         private-impl
                         private-impl

                         :else
                         `(fn [~'msg] nil))]

    `(do
       ; Define the dispatch function with exception handling
       (defn ~dispatch-fn-name [msg#]
         (try
           (~final-dispatch msg#)
           (catch Exception e#
             (.printStackTrace e#)
             (when (.hasText msg#)
               (.reply msg# (str ~(name module-name) " caused an exception: "
                                 (.getLocalizedMessage e#)))))))

       ; Create module metadata
       (def ~metadata-name
         (mp/map->ModuleMetadata
           {:module-name ~(name module-name)
            :commands ~(vec commands)
            :message-type ~message-type
            :wants-private ~wants-private
            :dispatch-fn ~dispatch-fn-name}))

       ; Register module immediately when namespace loads
       (registry/register-module! ~metadata-name)

       ; Include any helper functions from body
       ~@(filter #(not (and (seq? %)
                           (= 'defn (first %))
                           (contains? #{'process-channel-message 'process-private-message}
                                     (second %))))
                remaining-body))))