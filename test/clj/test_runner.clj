(ns test-runner
  "Test runner wrapper that forces JVM shutdown after tests complete"
  (:require [cognitect.test-runner.api :as api]))

(defn test
  "Run tests and force JVM shutdown to prevent hanging on AWT threads"
  [opts]
  (let [result (api/test opts)]
    ;; Give a moment for output to flush
    (Thread/sleep 100)
    ;; Force shutdown
    (shutdown-agents)
    (System/exit (if (zero? (:fail result 0)) 0 1))))
