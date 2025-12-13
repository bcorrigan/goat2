(ns org.goat.core.dispatcher
  "Core.async based message dispatcher.

  Replaces MessageDispatcher.java with a lightweight core.async solution.
  Instead of N threads (one per module) + N queues, uses a single go-loop
  that processes messages and invokes modules directly via function calls."
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! thread]]
            [org.goat.core.registry :as registry]
            [org.goat.core.message :as msg])
  (:import [org.goat.core Message]
           [java.util.concurrent LinkedBlockingQueue]))

;; Main dispatch channel - messages from Goat.inqueue go here
(defonce ^:private dispatch-chan (chan 100)) ; buffered to handle bursts

;; Control channel for shutdown
(defonce ^:private control-chan (chan))

;; Reference to the dispatcher thread for monitoring
(defonce ^:private dispatcher-thread (atom nil))

(defn- should-dispatch-to-module?
  "Determine if message should be dispatched to this module.
   Checks private message preference (channel membership removed per plan)."
  [^Message msg module]
  (let [{:keys [wants-private]} module
        is-private (.isPrivate msg)]
    ;; Only check wants-private for private messages
    ;; (all modules receive channel messages)
    (or (not is-private) wants-private)))

(defn- get-message-command
  "Extract command keyword from message, or nil if no command"
  [^Message msg]
  (when-let [cmd (.getModCommand msg)]
    (when-not (empty? cmd)
      (keyword cmd))))

(defn- command-matches?
  "Check if message command matches any of module's commands"
  [msg-cmd module-commands]
  (and msg-cmd
       (seq module-commands)
       (contains? (set module-commands) msg-cmd)))

(defn- dispatch-message
  "Dispatch a single message to appropriate modules.

   Implements the same logic as MessageDispatcher.java:
   1. Send to all :all modules
   2. Check if any :all module claims the command
   3. Send to :commands modules if command matches
   4. If not claimed, send to :unclaimed modules"
  [^Message msg]
  ;; Skip messages without text or documents
  (when (or (.hasText msg) (.hasDocument msg))
    (let [modules (registry/get-modules)
          {:keys [all unclaimed commands]} (group-by :message-type modules)
          msg-cmd (get-message-command msg)

          ;; Step 1: Dispatch to :all modules
          _ (doseq [module all]
              (when (should-dispatch-to-module? msg module)
                ((:dispatch-fn module) msg)))

          ;; Step 2: Check if any :all module claimed this command
          claimed? (boolean
                    (some #(command-matches? msg-cmd (:commands %))
                          all))

          ;; Step 3: Dispatch to :commands modules if command matches
          commands-claimed? (atom false)
          _ (doseq [module commands]
              (when (and (command-matches? msg-cmd (:commands module))
                        (should-dispatch-to-module? msg module))
                ((:dispatch-fn module) msg)
                (reset! commands-claimed? true)))

          ;; Step 4: If not claimed, dispatch to :unclaimed modules
          _ (when-not (or claimed? @commands-claimed?)
              (doseq [module unclaimed]
                (when (should-dispatch-to-module? msg module)
                  ((:dispatch-fn module) msg))))]
      nil)))

(defn- bridge-java-queue
  "Bridge between Java LinkedBlockingQueue and core.async channel.
   Runs in a separate thread to handle blocking .take() calls."
  [^LinkedBlockingQueue queue]
  (thread
    (loop []
      (when-let [msg (try
                       (.take queue)
                       (catch InterruptedException _
                         nil))]
        (async/>!! dispatch-chan msg)
        (recur)))))

(defn- dispatcher-loop
  "Main dispatcher loop - processes messages from dispatch-chan.
   Runs in a go-loop for lightweight async processing."
  []
  (go-loop []
    (let [[msg ch] (async/alts! [dispatch-chan control-chan])]
      (cond
        ;; Control message received - stop
        (= ch control-chan)
        (do
          (println "Dispatcher stopping...")
          :stopped)

        ;; nil message means channel closed
        (nil? msg)
        (recur)

        ;; Normal message - dispatch it
        :else
        (do
          (try
            (dispatch-message msg)
            (catch Exception e
              (println "Error dispatching message:" (.getMessage e))
              (.printStackTrace e)))
          (recur))))))

(defn start!
  "Start the dispatcher. Call once at startup.
   Bridges the Java inqueue to core.async and starts processing."
  [^LinkedBlockingQueue inqueue]
  (println "Starting Clojure dispatcher...")

  ;; Start bridge thread (blocks on Java queue)
  (reset! dispatcher-thread (bridge-java-queue inqueue))

  ;; Start dispatcher loop (lightweight go-loop)
  (dispatcher-loop)

  (println "Clojure dispatcher started"))

(defn stop!
  "Stop the dispatcher gracefully.
   Closes channels and terminates processing."
  []
  (println "Stopping Clojure dispatcher...")
  (async/put! control-chan :stop)
  (close! dispatch-chan)
  (close! control-chan)
  (println "Clojure dispatcher stopped"))
