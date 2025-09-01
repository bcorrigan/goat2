(ns org.goat.wordle.feedback-test
  (:require [clojure.test :refer :all]
            [org.goat.wordle.str :as msg]))

(deftest test-get-feedback-message
  (testing "Mistake messages for guesses that reveal no information"
    (let [message (msg/get-feedback-message 0 true)]
      (is (string? message))
      (is (contains? (set msg/mistake-messages) message))))
  
  (testing "Poor messages for low ratings"
    (doseq [rating [1 2 3]]
      (let [message (msg/get-feedback-message rating false)]
        (is (string? message))
        (is (contains? (set msg/poor-messages) message)))))
  
  (testing "Good messages for medium ratings"
    (doseq [rating [4 5 6]]
      (let [message (msg/get-feedback-message rating false)]
        (is (string? message))
        (is (contains? (set msg/good-messages) message)))))
  
  (testing "Excellent messages for high ratings"
    (doseq [rating [7 8 9 10]]
      (let [message (msg/get-feedback-message rating false)]
        (is (string? message))
        (is (contains? (set msg/excellent-messages) message)))))
  
  (testing "Mistake flag overrides rating"
    (let [message (msg/get-feedback-message 10 true)]
      (is (contains? (set msg/mistake-messages) message))))
  
  (testing "Non-mistake with rating 0 gets poor message"
    (let [message (msg/get-feedback-message 0 false)]
      (is (contains? (set msg/poor-messages) message)))))

(deftest test-format-performance-message
  (testing "Skilled win message"
    (let [performance {:avg-rating 8.5 :num-guesses 3 :luck-factor 0.2 :overall-grade :skilled}
          message (msg/format-performance-message performance "Alice" true)]
      (is (string? message))
      (is (.startsWith message "Alice"))
      (is (some #(.contains message %) (get msg/performance-messages :skilled-win)))))
  
  (testing "Lucky win message"
    (let [performance {:avg-rating 4.0 :num-guesses 2 :luck-factor 0.9 :overall-grade :lucky}
          message (msg/format-performance-message performance "Bob" true)]
      (is (.startsWith message "Bob"))
      (is (some #(.contains message %) (get msg/performance-messages :lucky-win)))))
  
  (testing "Skilled loss message"
    (let [performance {:avg-rating 7.5 :num-guesses 6 :luck-factor 0.3 :overall-grade :skilled}
          message (msg/format-performance-message performance "Charlie" false)]
      (is (.startsWith message "Charlie"))
      (is (some #(.contains message %) (get msg/performance-messages :skilled-loss)))))
  
  (testing "Poor performance message"
    (let [performance {:avg-rating 2.0 :num-guesses 6 :luck-factor 0.4 :overall-grade :poor}
          message (msg/format-performance-message performance "Diana" false)]
      (is (.startsWith message "Diana"))
      (is (some #(.contains message %) (get msg/performance-messages :poor-performance))))))

(deftest test-message-collections-non-empty
  (testing "All message collections have content"
    (is (> (count msg/mistake-messages) 0))
    (is (> (count msg/poor-messages) 0))
    (is (> (count msg/good-messages) 0))
    (is (> (count msg/excellent-messages) 0))
    (is (> (count (get msg/performance-messages :skilled-win)) 0))
    (is (> (count (get msg/performance-messages :lucky-win)) 0))
    (is (> (count (get msg/performance-messages :skilled-loss)) 0))
    (is (> (count (get msg/performance-messages :poor-performance)) 0))))

(deftest test-message-quality
  (testing "All messages are strings"
    (doseq [message msg/mistake-messages]
      (is (string? message)))
    (doseq [message msg/poor-messages]
      (is (string? message)))
    (doseq [message msg/good-messages]
      (is (string? message)))
    (doseq [message msg/excellent-messages]
      (is (string? message))))
  
  (testing "No empty messages"
    (doseq [message (concat msg/mistake-messages 
                           msg/poor-messages 
                           msg/good-messages 
                           msg/excellent-messages)]
      (is (> (count (.trim message)) 0)))))