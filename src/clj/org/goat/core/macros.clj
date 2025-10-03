(ns org.goat.core.macros
  "Macros for simplifying Goat module creation"
  (:require [org.goat.core.message :as msg]
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
  "Define a Goat module with automatic Java interop and message wrapping.

  Usage:
  (defmodule ModuleName
    :commands [:cmd1 :cmd2]
    :wants-private true
    :message-type WANT_ALL_MESSAGES

    (defn process-channel-message [m]
      ; m is automatically wrapped - use (get-command m), (reply m text), etc.
      (case (get-command m)
        :cmd1 (reply m \"Hello!\")
        :cmd2 (reply m \"World!\")))

    ; process-private-message auto-implemented if not provided
    )"
  [module-name & body]
  (let [[options remaining-body] (extract-options body)
        commands (or (:commands options) [])
        wants-private (get options :wants-private true)
        message-type (get options :message-type 'Module/WANT_COMMAND_MESSAGES)

        ; Find function definitions
        channel-fn (find-defn-by-name remaining-body 'process-channel-message)
        private-fn (find-defn-by-name remaining-body 'process-private-message)

        ; Generate the namespace qualified class name
        ; Convert dashes to underscores for Java compatibility
        current-ns (str/replace (str (ns-name *ns*)) "-" "_")
        ; If the namespace already ends with the module name, use it directly
        ; Otherwise append the module name
        class-name (if (str/ends-with? current-ns (str "." module-name))
                     (symbol current-ns)
                     (symbol (str current-ns "." module-name)))

        ; Transform function definitions
        channel-method (when channel-fn
                        (transform-message-fn channel-fn "processChannelMessage"))
        private-method (when private-fn
                        (transform-message-fn private-fn "processPrivateMessage"))

        ; Auto-generate missing method (delegate to the other)
        auto-delegation (cond
                         (and channel-fn (not private-fn))
                         (generate-delegation "processPrivateMessage" "processChannelMessage")

                         (and private-fn (not channel-fn))
                         (generate-delegation "processChannelMessage" "processPrivateMessage")

                         :else nil)]

    `(do
       ; Generate the Java class
       (gen-class
         :name ~class-name
         :extends org.goat.core.Module)

       ; Generate getCommands method
       (defn ~'-getCommands [~'_]
         (into-array String ~(mapv name commands)))

       ; Generate messageType method if specified
       ~(when (not= message-type 'Module/WANT_COMMAND_MESSAGES)
          `(defn ~'-messageType [~'_]
             ~message-type))

       ; Generate wantsPrivate field if specified
       ~(when (not wants-private)
          `(def ~'wantsPrivate false))

       ; Include the transformed method definitions
       ~channel-method
       ~private-method
       ~auto-delegation

       ; Include any other definitions from the body
       ~@(filter #(not (and (seq? %)
                           (= 'defn (first %))
                           (contains? #{'process-channel-message 'process-private-message}
                                     (second %))))
                remaining-body))))