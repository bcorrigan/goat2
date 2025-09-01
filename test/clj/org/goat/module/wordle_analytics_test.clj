(ns org.goat.module.wordle-analytics-test
  (:require [clojure.test :refer :all]
            [org.goat.module.Wordle :as wordle]
            [org.goat.wordle.analytics :as analytics]
            [org.goat.testutils.message :as msg-utils]))

(deftest test-calculate-facts-so-far
  (testing "Facts calculation with no guesses"
    (with-redefs [wordle/get-gameprop (fn [chat-key prop]
                                        (case prop
                                          :guesses []
                                          :answer "HOUSE"))]
      (let [facts (wordle/calculate-facts-so-far :test-chat)]
        (is (map? facts)))))
  
  (testing "Facts calculation with single guess"
    (with-redefs [wordle/get-gameprop (fn [chat-key prop]
                                        (case prop
                                          :guesses ["CRANE"]
                                          :answer "HOUSE"))]
      (let [facts (wordle/calculate-facts-so-far :test-chat)]
        (is (map? facts))
        (is (contains? facts :known))
        (is (contains? facts :bounds))
        (is (contains? facts :known-nots)))))
  
  (testing "Facts calculation with multiple guesses"
    (with-redefs [wordle/get-gameprop (fn [chat-key prop]
                                        (case prop
                                          :guesses ["CRANE" "GHOST"]
                                          :answer "HOUSE"))]
      (let [facts (wordle/calculate-facts-so-far :test-chat)]
        (is (map? facts))
        (is (contains? facts :known))
        (is (contains? facts :bounds))
        (is (contains? facts :known-nots))))))

(deftest test-game-state-initialization
  (testing "New game includes analytics state"
    (let [test-state (atom {:game-states {}})]
      (with-redefs [wordle/state test-state]
        (wordle/new-game! :test-chat "HOUSE" 5 "A dwelling" 1000 "test-user" :easy nil)
        
        (let [game-state (wordle/get-game :test-chat)]
          (is (contains? game-state :facts-history))
          (is (contains? game-state :ratings-history))
          (is (vector? (:facts-history game-state)))
          (is (vector? (:ratings-history game-state))))))))