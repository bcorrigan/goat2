(ns org.goat.wordle.gfx-pie-test
  "Test for refactored pie chart drawing using gfx.core"
  (:require [org.goat.wordle.gfx :as sut]
            [clojure.test :refer [deftest is testing]])
  (:import [java.awt.image BufferedImage]))

;; ============================================================================
;; Pie Chart Drawing Tests
;; ============================================================================

(deftest test-draw-pie-chart-returns-image
  (testing "draw-pie-chart returns a BufferedImage with correct dimensions"
    (let [img (sut/draw-pie-chart 10 5 2 "Alice" "Bob")]
      ;; Should return a BufferedImage
      (is (instance? BufferedImage img))

      ;; Check dimensions match pie chart constants
      (is (= 600 (.getWidth img)))   ; pie-width
      (is (= 600 (.getHeight img)))))) ; pie-height

(deftest test-draw-pie-chart-all-player1-wins
  (testing "draw-pie-chart handles all wins for player 1"
    (let [img (sut/draw-pie-chart 10 0 0 "Winner" "Loser")]
      (is (instance? BufferedImage img))
      (is (= 600 (.getWidth img)))
      (is (= 600 (.getHeight img))))))

(deftest test-draw-pie-chart-all-draws
  (testing "draw-pie-chart handles all draws"
    (let [img (sut/draw-pie-chart 0 0 10 "Alice" "Bob")]
      (is (instance? BufferedImage img))
      (is (= 600 (.getWidth img)))
      (is (= 600 (.getHeight img))))))

(deftest test-draw-pie-chart-balanced
  (testing "draw-pie-chart handles balanced results"
    (let [img (sut/draw-pie-chart 5 5 5 "Alice" "Bob")]
      (is (instance? BufferedImage img))
      (is (= 600 (.getWidth img)))
      (is (= 600 (.getHeight img))))))

(deftest test-draw-pie-chart-gr-executes-without-error
  (testing "draw-pie-chart-gr executes without crashing"
    (let [{:keys [canvas g2d]} (org.goat.gfx.core/create-canvas 600 600)]
      ;; Should not throw
      (sut/draw-pie-chart-gr g2d 15 8 3 "Player1" "Player2")
      (.dispose g2d)
      (is (some? canvas)))))

(deftest test-pie-width-and-height-constants
  (testing "Pie chart dimensions constants are correct"
    (is (= 600 sut/pie-width))
    (is (= 600 sut/pie-height))))
