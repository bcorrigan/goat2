(ns org.goat.module.Countdown-test
  (:require [clojure.test :refer :all]
            [org.goat.module.Countdown :as sut]
            [org.goat.testutils.message :as msg-utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTION TESTS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-extract-numbers
  (testing "Extract numbers from expression"
    (is (= [10 5 3] (#'sut/extract-numbers "10+5*3")))
    (is (= [25 7 100] (#'sut/extract-numbers "25 + 7 * 100")))
    (is (= [1 2 3 4] (#'sut/extract-numbers "(1+2)*(3+4)")))))

(deftest test-numbers-valid
  (testing "Valid number usage"
    (is (true? (#'sut/numbers-valid? [25 7] [25 50 75 7 3 8])))
    (is (true? (#'sut/numbers-valid? [25 7 3 8] [25 50 75 7 3 8])))
    (is (true? (#'sut/numbers-valid? [] [25 50 75 7 3 8]))))

  (testing "Invalid number usage - number not in selection"
    (is (false? (#'sut/numbers-valid? [99] [25 50 75 7 3 8])))
    (is (false? (#'sut/numbers-valid? [25 99] [25 50 75 7 3 8]))))

  (testing "Invalid number usage - number used twice"
    (is (false? (#'sut/numbers-valid? [25 25] [25 50 75 7 3 8])))
    (is (false? (#'sut/numbers-valid? [7 7 7] [25 50 7 7 3 8])))))

(deftest test-expression-valid
  (testing "Valid expressions"
    (is (true? (#'sut/expression-valid? "25+7")))
    (is (true? (#'sut/expression-valid? "(25+7)*3")))
    (is (true? (#'sut/expression-valid? "100/25-3+8")))
    (is (true? (#'sut/expression-valid? "  25 + 7  "))))

  (testing "Invalid expressions - bad operators"
    (is (false? (#'sut/expression-valid? "25^7")))
    (is (false? (#'sut/expression-valid? "25%7")))
    (is (false? (#'sut/expression-valid? "25&7")))
    (is (false? (#'sut/expression-valid? "sqrt(25)")))))

(deftest test-validate-answer
  (testing "Valid answer"
    (let [result (#'sut/validate-answer "25+7" [25 7 3 8 2 10])]
      (is (:valid result))))

  (testing "Invalid answer - wrong number"
    (let [result (#'sut/validate-answer "25+99" [25 7 3 8 2 10])]
      (is (not (:valid result)))
      (is (= "You used a number not in the selection!" (:error result)))))

  (testing "Invalid answer - bad operator"
    (let [result (#'sut/validate-answer "25^7" [25 7 3 8 2 10])]
      (is (not (:valid result)))
      (is (= "Invalid operators in expression" (:error result))))))

(deftest test-evaluate-answer
  (testing "Successful evaluation"
    (let [result (#'sut/evaluate-answer "25+7")]
      (is (:success result))
      (is (= 32 (:value result)))))

  (testing "Complex expression"
    (let [result (#'sut/evaluate-answer "(25+7)*3")]
      (is (:success result))
      (is (= 96 (:value result)))))

  (testing "Division"
    (let [result (#'sut/evaluate-answer "100/25")]
      (is (:success result))
      (is (= 4 (:value result)))))

  (testing "Non-integer result"
    (let [result (#'sut/evaluate-answer "25/7")]
      (is (not (:success result)))
      (is (re-find #"non-int" (:error result))))))

(deftest test-distance-from-target
  (testing "Distance calculation"
    (is (= 10 (#'sut/distance-from-target 100 110)))
    (is (= 10 (#'sut/distance-from-target 110 100)))
    (is (= 0 (#'sut/distance-from-target 100 100)))
    (is (= 500 (#'sut/distance-from-target 100 600)))))

(deftest test-format-numbers
  (testing "Format numbers as space-separated string"
    (is (= "25 50 75 100" (#'sut/format-numbers [25 50 75 100])))
    (is (= "1 2 3 4 5 6" (#'sut/format-numbers [1 2 3 4 5 6])))))

(deftest test-initialize-pools
  (testing "Pool initialization creates correct pools"
    (let [pools (#'sut/initialize-pools)]
      (is (= 4 (count (:big-pool pools))))
      (is (= 20 (count (:small-pool pools))))
      ;; Check all big numbers present (though shuffled)
      (is (= #{25 50 75 100} (set (:big-pool pools))))
      ;; Check all small numbers present (20 total, 2 of each 1-10)
      (is (= {1 2, 2 2, 3 2, 4 2, 5 2, 6 2, 7 2, 8 2, 9 2, 10 2}
             (frequencies (:small-pool pools)))))))

(deftest test-draw-numbers
  (testing "Draw numbers returns 6 numbers"
    (let [pools (#'sut/initialize-pools)
          numbers (#'sut/draw-numbers pools)]
      (is (= 6 (count numbers)))
      (is (vector? numbers))))

  (testing "Numbers are drawn from pools"
    ;; Run multiple times to test both 1-big and 2-big scenarios
    (dotimes [_ 20]
      (let [pools (#'sut/initialize-pools)
            numbers (#'sut/draw-numbers pools)
            big-count (count (filter #{25 50 75 100} numbers))
            small-count (- 6 big-count)]
        (is (or (= big-count 1) (= big-count 2)))
        (is (or (= small-count 5) (= small-count 4)))))))

(deftest test-generate-target
  (testing "Generate target in valid range"
    (dotimes [_ 100]
      (let [target (#'sut/generate-target)]
        (is (>= target 101))
        (is (<= target 999))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE MANAGEMENT TESTS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-game-state-management
  (testing "Game state initialization and cleanup"
    ;; Reset state
    (reset! sut/state {})

    ;; Initially no game active
    (is (false? (#'sut/game-active? :test-chat)))
    (is (false? (#'sut/any-game-active?)))

    ;; Start new game
    (let [result (#'sut/start-new-game! :test-chat 12345)]
      (is (number? (:target-number result)))
      (is (= 6 (count (:source-numbers result)))))

    ;; Game should now be active
    (is (true? (#'sut/game-active? :test-chat)))
    (is (true? (#'sut/any-game-active?)))

    ;; Verify game state
    (let [game (#'sut/get-game-state :test-chat)]
      (is (:game-on game))
      (is (= 12345 (:chat-id game)))
      (is (nil? (:best-answer game))))

    ;; Update best answer
    (#'sut/update-best-answer! :test-chat 100 "alice" "25*4")
    (let [game (#'sut/get-game-state :test-chat)]
      (is (= {:value 100 :username "alice" :expression "25*4"}
             (:best-answer game))))

    ;; Clear game
    (#'sut/clear-game! :test-chat)
    (is (false? (#'sut/game-active? :test-chat)))
    (is (false? (#'sut/any-game-active?)))))

(deftest test-multiple-concurrent-games
  (testing "Multiple games in different chats"
    ;; Reset state
    (reset! sut/state {})

    ;; Start games in two different chats
    (#'sut/start-new-game! :chat1 111)
    (#'sut/start-new-game! :chat2 222)

    ;; Both games active
    (is (true? (#'sut/game-active? :chat1)))
    (is (true? (#'sut/game-active? :chat2)))
    (is (true? (#'sut/any-game-active?)))

    ;; Games have different state
    (is (= 111 (:chat-id (#'sut/get-game-state :chat1))))
    (is (= 222 (:chat-id (#'sut/get-game-state :chat2))))

    ;; Clear one game
    (#'sut/clear-game! :chat1)
    (is (false? (#'sut/game-active? :chat1)))
    (is (true? (#'sut/game-active? :chat2)))
    (is (true? (#'sut/any-game-active?)))

    ;; Clear other game
    (#'sut/clear-game! :chat2)
    (is (false? (#'sut/any-game-active?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGRATION TESTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-start-game-flow
  (testing "Starting a new game"
    ;; Reset state
    (reset! sut/state {})

    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "countdown" ""
                                                 {:sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg)

        ;; Should have sent a reply
        (is (= 1 (msg-utils/reply-count)))

        ;; Reply should contain numbers and target
        (is (msg-utils/replied-with? "New Numbers:"))
        (is (msg-utils/replied-with? "Target:"))

        ;; Game should be active
        (is (true? (#'sut/game-active? (keyword "999"))))))))

(deftest test-cannot-start-game-when-active
  (testing "Cannot start second game in same chat"
    ;; Reset state
    (reset! sut/state {})

    (msg-utils/with-clean-replies
      ;; Start first game
      (let [msg1 (msg-utils/mock-command-message "countdown" ""
                                                  {:sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg1)
        (is (= 1 (msg-utils/reply-count))))

      (msg-utils/clear-replies!)

      ;; Try to start second game
      (let [msg2 (msg-utils/mock-command-message "countdown" ""
                                                  {:sender "bob" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg2)

        ;; Should get error message
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "already playing"))))))

(deftest test-valid-answer-submission
  (testing "Submit valid answer during game"
    ;; Reset state and set up known game
    (reset! sut/state {})
    (swap! sut/state assoc (keyword "999")
           {:game-on true
            :chat-id 999
            :target-number 100
            :source-numbers [25 50 75 7 3 8]
            :best-answer nil
            :best-possible 100  ; Best possible is 100, so 75 won't end the game
            :timer-future nil})

    (msg-utils/with-clean-replies
      ;; Submit answer: 75 (distance 25 from target, won't end game)
      (let [msg (msg-utils/mock-message {:text "75" :sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg)

        ;; Should have replied acknowledging best answer
        (is (>= (msg-utils/reply-count) 1))
        (is (msg-utils/replied-with? "best answer so far"))

        ;; Check if best answer was updated
        (let [best (:best-answer (get @sut/state (keyword "999")))]
          (is (= 75 (:value best)))
          (is (= "alice" (:username best))))))))

(deftest test-invalid-answer-number-not-in-selection
  (testing "Reject answer with invalid number"
    ;; Reset state and set up known game
    (reset! sut/state {})
    (swap! sut/state assoc (keyword "999")
           {:game-on true
            :chat-id 999
            :target-number 100
            :source-numbers [25 50 75 7 3 8]
            :best-answer nil
            :best-possible 100
            :timer-future nil})

    (msg-utils/with-clean-replies
      ;; Submit answer with number not in selection
      (let [msg (msg-utils/mock-message {:text "99+1" :sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg)

        ;; Should have replied with error
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "not in the selection"))))))

(deftest test-invalid-answer-bad-operator
  (testing "Reject answer with invalid operator"
    ;; Reset state and set up known game
    (reset! sut/state {})
    (swap! sut/state assoc (keyword "999")
           {:game-on true
            :chat-id 999
            :target-number 100
            :source-numbers [25 50 75 7 3 8]
            :best-answer nil
            :best-possible 100
            :timer-future nil})

    (msg-utils/with-clean-replies
      ;; Submit answer with invalid operator (using valid numbers but invalid operator)
      (let [msg (msg-utils/mock-message {:text "25^7" :sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg)

        ;; Should have replied with error
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "Invalid operators"))))))

(deftest test-better-answer-updates-best
  (testing "Better answer replaces previous best"
    ;; Reset state and set up known game
    (reset! sut/state {})
    (swap! sut/state assoc (keyword "999")
           {:game-on true
            :chat-id 999
            :target-number 100
            :source-numbers [25 50 75 7 3 8]
            :best-answer nil
            :best-possible 100
            :timer-future nil})

    (msg-utils/with-clean-replies
      ;; First answer: 75 (distance 25)
      (let [msg1 (msg-utils/mock-message {:text "75" :sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg1))

      (msg-utils/clear-replies!)

      ;; Better answer: 75+7 = 82 (distance 18, better than 75 but doesn't end game)
      (let [msg2 (msg-utils/mock-message {:text "75+7" :sender "bob" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg2)

        ;; Should acknowledge bob's better answer
        (is (msg-utils/replied-with? "bob"))
        (is (msg-utils/replied-with? "best answer so far"))

        ;; Best answer should be updated
        (let [best (:best-answer (get @sut/state (keyword "999")))]
          (is (= 82 (:value best)))
          (is (= "bob" (:username best))))))))

(deftest test-messages-ignored-when-no-game
  (testing "Messages ignored when no game active"
    ;; Reset state
    (reset! sut/state {})

    (msg-utils/with-clean-replies
      ;; Send non-command message
      (let [msg (msg-utils/mock-message {:text "25+25" :sender "alice" :chat-id 999})]
        (#'sut/-processChannelMessage nil msg)

        ;; Should not reply (no game active)
        (is (= 0 (msg-utils/reply-count)))))))
