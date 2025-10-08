(ns org.goat.wordle.gfx-stats-test
  "Test for refactored stats drawing using gfx.core"
  (:require [org.goat.wordle.gfx :as sut]
            [org.goat.db.users :as users]
            [clojure.test :refer [deftest is testing]])
  (:import [java.awt.image BufferedImage]))

;; ============================================================================
;; Stats Drawing Tests
;; ============================================================================

(deftest test-draw-stats-gr-executes-without-error
  (testing "draw-stats-gr executes without crashing"
    ;; Mock get-stats to return test data
    (with-redefs [users/get-stats (fn [user]
                                     {:games-won 50
                                      :games-lost 10
                                      :games-won-150 40
                                      :games-lost-150 5
                                      :games-won-20 15
                                      :games-lost-20 2
                                      :guess-rate-150 3.5
                                      :guess-rate-20 3.2
                                      :results-150 [{:won true :guesses 3}
                                                    {:won false :guesses 6}]})
                  users/get-streak (fn [user] 5)
                  users/get-record (fn [user record-type] 0.5)]
      (let [{:keys [canvas g2d]} (org.goat.gfx.core/create-canvas 800 1200)]
        ;; Should not throw
        (sut/draw-stats-gr g2d "testuser")
        (.dispose g2d)
        (is (some? canvas))))))

(deftest test-draw-stats-returns-buffered-image
  (testing "draw-stats returns a BufferedImage with correct dimensions"
    ;; Mock user functions
    (with-redefs [users/get-stats (fn [user]
                                     {:games-won 100
                                      :games-lost 20
                                      :games-won-150 80
                                      :games-lost-150 15
                                      :games-won-20 18
                                      :games-lost-20 3
                                      :guess-rate-150 3.8
                                      :guess-rate-20 3.4
                                      :results-150 []})
                  users/get-streak (fn [user] 10)
                  users/get-record (fn [user record-type] 0.8)]
      (let [img (sut/draw-stats "alice")]
        ;; Should return a BufferedImage
        (is (instance? BufferedImage img))

        ;; Check dimensions match stats constants
        (is (= 800 (.getWidth img)))   ; stats-width
        (is (= 1200 (.getHeight img)))))))  ; stats-height

(deftest test-draw-stats-with-empty-results
  (testing "draw-stats handles empty results-150"
    (with-redefs [users/get-stats (fn [user]
                                     {:games-won 1
                                      :games-lost 0
                                      :games-won-150 1
                                      :games-lost-150 0
                                      :games-won-20 1
                                      :games-lost-20 0
                                      :guess-rate-150 3.0
                                      :guess-rate-20 3.0
                                      :results-150 []})
                  users/get-streak (fn [user] 1)
                  users/get-record (fn [user record-type] 0.0)]
      (let [img (sut/draw-stats "bob")]
        (is (instance? BufferedImage img))
        (is (= 800 (.getWidth img)))
        (is (= 1200 (.getHeight img)))))))

(deftest test-stats-width-and-height-constants
  (testing "Stats dimensions constants are correct"
    (is (= 800 sut/stats-width))
    (is (= 1200 sut/stats-height))))
