(ns org.goat.module.wordle-challenge-test
  "Tests for wordle challenge gameplay between two players"
  (:require [org.goat.testutils.message :as msg-utils]
            [org.goat.module.Wordle :as wordle]
            [org.goat.db.users :as users]
            [clojure.test :as t :refer [deftest is testing use-fixtures]])
  (:import (org.goat.core Message)))

(use-fixtures :each (fn [f] 
                      (msg-utils/clear-replies!) 
                      (reset! wordle/state {})
                      (f)))

(defn setup-test-users! []
  (users/user-add "alice" 1001)
  (users/user-add "bob" 1002))

(deftest test-challenge-flow
  "Tests complete challenge flow: setup, gameplay, and final board comparison"
  
  (testing "Full challenge gameplay"
    (setup-test-users!)
    
    (msg-utils/with-clean-replies
      (let [challenge-msg (msg-utils/mock-command-message 
                           "wordle" 
                           "challenge bob"
                           {:chat-id 2000
                            :sender "alice"
                            :is-private false})]
        
        (wordle/-processChannelMessage nil challenge-msg)
        (is (msg-utils/replied-with? "Starting a challenge match"))
        
        (let [alice-key :1001
              bob-key :1002]
          (is (wordle/playing? alice-key))
          (is (wordle/playing? bob-key))
          
          (let [alice-word (wordle/get-gameprop alice-key :answer)
                bob-word (wordle/get-gameprop bob-key :answer)]
            (is (= alice-word bob-word))
            
            (msg-utils/clear-replies!)
            
            ;; Alice plays and wins
            (let [guess1-msg (msg-utils/mock-message {:text "CRANE" :chat-id 1001 :sender "alice" :is-private true})]
              (wordle/-processPrivateMessage nil guess1-msg))
            
            (let [guess2-msg (msg-utils/mock-message {:text "SLOTH" :chat-id 1001 :sender "alice" :is-private true})]
              (wordle/-processPrivateMessage nil guess2-msg))
            
            (let [alice-win-msg (msg-utils/mock-message {:text alice-word :chat-id 1001 :sender "alice" :is-private true})]
              (wordle/-processPrivateMessage nil alice-win-msg))
            
            (msg-utils/clear-replies!)
            
            ;; Bob plays and wins
            (let [guess1-msg (msg-utils/mock-message {:text "ADIEU" :chat-id 1002 :sender "bob" :is-private true})]
              (wordle/-processPrivateMessage nil guess1-msg))
            
            (let [guess2-msg (msg-utils/mock-message {:text "ROAST" :chat-id 1002 :sender "bob" :is-private true})]
              (wordle/-processPrivateMessage nil guess2-msg))
            
            (let [bob-win-msg (msg-utils/mock-message {:text bob-word :chat-id 1002 :sender "bob" :is-private true})]
              (wordle/-processPrivateMessage nil bob-win-msg)
              (is (msg-utils/replied-with? "challenge has concluded")))))))))

(deftest test-challenge-state-inspection
  "Verifies challenge state setup and linkage between games"
  
  (testing "Challenge state setup"
    (setup-test-users!)
    
    (let [challenge-msg (msg-utils/mock-command-message 
                         "wordle" 
                         "challenge bob"
                         {:chat-id 2000
                          :sender "alice" 
                          :is-private false})]
      (wordle/-processChannelMessage nil challenge-msg)
      
      (let [alice-key :1001
            bob-key :1002
            challenge-key (wordle/get-gameprop alice-key :challenge-key)]
        
        (is (= challenge-key (wordle/get-gameprop alice-key :challenge-key)))
        (is (= challenge-key (wordle/get-gameprop bob-key :challenge-key)))
        (is (wordle/get-gameprop alice-key :challenge))
        (is (wordle/get-gameprop bob-key :challenge))))))

(deftest test-image-caching
  "Tests that board images are properly cached during gameplay"
  
  (testing "Image caching after both players finish"
    (setup-test-users!)
    
    (let [challenge-msg (msg-utils/mock-command-message 
                         "wordle" 
                         "challenge bob"
                         {:chat-id 2000
                          :sender "alice"
                          :is-private false})]
      (wordle/-processChannelMessage nil challenge-msg)
      
      (let [alice-key :1001
            bob-key :1002
            challenge-key (wordle/get-gameprop alice-key :challenge-key)
            answer (wordle/get-gameprop alice-key :answer)]
        
        ;; Alice finishes first
        (let [alice-win-msg (msg-utils/mock-message {:text answer :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil alice-win-msg)
          
          ;; Check that Alice's data is cached as first-game (she finished while Bob still playing)
          (let [first-game-data (wordle/get-gameprop challenge-key :first-game)
                cached-img (wordle/get-fgameprop challenge-key :img)
                playing-count (let [atom (wordle/get-gameprop challenge-key :playing)]
                                (if atom @atom "nil"))]
            (println (str "After Alice finishes - Playing count: " playing-count))
            (println (str "First game data: " (some? first-game-data)))
            (println (str "Cached image: " (some? cached-img)))
            (is (some? first-game-data) "First game data should be cached after first player finishes")
            (is (some? cached-img) "Board image should be cached after first player finishes")))
        
        ;; Bob finishes second, completing the challenge
        (let [bob-win-msg (msg-utils/mock-message {:text answer :chat-id 1002 :sender "bob" :is-private true})]
          (wordle/-processPrivateMessage nil bob-win-msg)
          ;; At this point challenge cleanup has occurred and challenge-key is removed
          )))))

(deftest test-remaining-words-history
  "Tests that remaining-words-history is tracked correctly during gameplay"
  
  (testing "Remaining words count is tracked after each guess"
    (setup-test-users!)
    
    (let [start-msg (msg-utils/mock-command-message 
                     "wordle" 
                     ""
                     {:chat-id 1001
                      :sender "alice"
                      :is-private true})]
      (wordle/-processPrivateMessage nil start-msg)
      
      (let [alice-key :1001]
        ;; Verify game started and history is initialized
        (is (wordle/playing? alice-key))
        (is (= [] (wordle/get-gameprop alice-key :remaining-words-history)))
        
        ;; Make first guess
        (let [guess1-msg (msg-utils/mock-message {:text "CRANE" :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil guess1-msg))
        
        ;; Check history has one entry
        (let [history1 (wordle/get-gameprop alice-key :remaining-words-history)]
          (is (= 1 (count history1)) "Should have 1 entry after 1 guess")
          (is (pos? (first history1)) "Remaining words count should be positive"))
        
        ;; Make second guess
        (let [guess2-msg (msg-utils/mock-message {:text "SLOTH" :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil guess2-msg))
        
        ;; Check history has two entries
        (let [history2 (wordle/get-gameprop alice-key :remaining-words-history)]
          (is (= 2 (count history2)) "Should have 2 entries after 2 guesses")
          (is (every? pos? history2) "All counts should be positive")
          (println (str "Remaining words history after 2 guesses: " history2)))
        
        ;; Make third guess (correct answer to end game)
        (let [answer (wordle/get-gameprop alice-key :answer)
              win-msg (msg-utils/mock-message {:text answer :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil win-msg)
          
          ;; Game should be cleared, but we can verify the pattern existed
          (is (not (wordle/playing? alice-key)) "Game should be finished")))))
  
  (testing "Remaining words history is cached in challenge mode"
    (setup-test-users!)
    
    (let [challenge-msg (msg-utils/mock-command-message 
                         "wordle" 
                         "challenge bob"
                         {:chat-id 2000
                          :sender "alice"
                          :is-private false})]
      (wordle/-processChannelMessage nil challenge-msg)
      
      (let [alice-key :1001
            bob-key :1002
            challenge-key (wordle/get-gameprop alice-key :challenge-key)]
        
        ;; Alice makes a guess
        (let [guess-msg (msg-utils/mock-message {:text "CRANE" :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil guess-msg))
        
        ;; Verify Alice has remaining-words-history
        (let [alice-history (wordle/get-gameprop alice-key :remaining-words-history)]
          (is (= 1 (count alice-history)) "Alice should have 1 history entry"))
        
        ;; Alice finishes
        (let [answer (wordle/get-gameprop alice-key :answer)
              win-msg (msg-utils/mock-message {:text answer :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil win-msg))
        
        ;; Verify Alice's remaining-words-history is cached in first-game
        (let [cached-history (wordle/get-fgameprop challenge-key :remaining-words-history)]
          (is (some? cached-history) "Remaining words history should be cached")
          (is (>= (count cached-history) 1) "Cached history should have at least 1 entry")
          (println (str "Cached remaining words history: " cached-history)))
        
        ;; Bob finishes to complete challenge
        (let [answer (wordle/get-gameprop bob-key :answer)
              bob-win-msg (msg-utils/mock-message {:text answer :chat-id 1002 :sender "bob" :is-private true})]
          (wordle/-processPrivateMessage nil bob-win-msg))))))

(deftest test-end-to-end-with-counts
  "End-to-end test verifying remaining words counts are displayed on board images"
  
  (testing "Board images include remaining words counts"
    (setup-test-users!)
    
    (let [start-msg (msg-utils/mock-command-message 
                     "wordle" 
                     ""
                     {:chat-id 1001
                      :sender "alice"
                      :is-private true})]
      (wordle/-processPrivateMessage nil start-msg)
      
      (let [alice-key :1001]
        ;; Make a few guesses
        (let [guess1-msg (msg-utils/mock-message {:text "CRANE" :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil guess1-msg))
        
        (let [guess2-msg (msg-utils/mock-message {:text "SLOTH" :chat-id 1001 :sender "alice" :is-private true})]
          (wordle/-processPrivateMessage nil guess2-msg))
        
        ;; Get the board image - this should now include counts
        (let [board-img (wordle/get-board-img alice-key)
              history (wordle/get-gameprop alice-key :remaining-words-history)]
          (is (some? board-img) "Board image should be generated")
          (is (= 2 (count history)) "Should have 2 count entries")
          
          ;; Verify image width includes space for counts (5 letters * 60px + 10px border + 80px for counts)
          (is (= 390 (.getWidth board-img)) "Image width should include count annotation space")
          (println (str "Board image dimensions: " (.getWidth board-img) "x" (.getHeight board-img)))
          (println (str "Counts displayed: " history)))))))