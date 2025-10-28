(ns org.goat.wordle.gfx-board-test
  "Test for refactored board drawing using gfx.core"
  (:require [org.goat.wordle.gfx :as sut]
            [clojure.test :refer [deftest is testing]])
  (:import [java.awt.image BufferedImage]))

(deftest test-draw-board-returns-image
  (testing "draw-board returns a BufferedImage with correct dimensions"
    (let [guesses-classified {"GREAT" [:revealed :revealed :revealed :revealed :revealed]
                              "WORLD" [:wrong :semiknown :revealed :wrong :semiknown]}
          size 5
          max-guesses 6
          img (sut/draw-board guesses-classified size max-guesses nil)]

      ;; Should return a BufferedImage
      (is (instance? BufferedImage img))

      ;; Check dimensions
      (let [width (.getWidth img)
            height (.getHeight img)]
        ;; Width should be size * letter-size + border
        ;; letter-size=60, border=10, so 5*60+10 = 310
        (is (= 310 width))
        ;; Height should be max-cols * letter-size + border
        ;; max-cols is min(guesses+1, 6) = min(3, 6) = 3
        ;; So 3*60+10 = 190
        (is (= 190 height))))))

(deftest test-draw-board-empty-guesses
  (testing "draw-board handles empty guesses"
    (let [guesses-classified {}
          size 5
          max-guesses 6
          img (sut/draw-board guesses-classified size max-guesses nil)]

      (is (instance? BufferedImage img))
      (is (> (.getWidth img) 0))
      (is (> (.getHeight img) 0)))))

(deftest test-draw-board-single-guess
  (testing "draw-board handles single guess"
    (let [guesses-classified {"GHOST" [:revealed :revealed :revealed :revealed :revealed]}
          size 5
          max-guesses 6
          img (sut/draw-board guesses-classified size max-guesses nil)]

      (is (instance? BufferedImage img))
      ;; Width: 5*60+10 = 310
      (is (= 310 (.getWidth img)))
      ;; Height: 2*60+10 = 130 (1 guess + 1 = 2 rows)
      (is (= 130 (.getHeight img))))))

(deftest test-board-width-calculation
  (testing "board-width calculation without counts"
    (is (= 310 (sut/board-width 5 false)))
    (is (= 370 (sut/board-width 6 false)))
    (is (= 250 (sut/board-width 4 false)))))

(deftest test-board-height-calculation
  (testing "board-height calculation uses get-no-cols (guesses+1)"
    ;; get-no-cols returns min(guesses+1, 6)
    (is (= 130 (sut/board-height 1)))  ; get-no-cols(1)=2, 2*60+10=130
    (is (= 190 (sut/board-height 2)))  ; get-no-cols(2)=3, 3*60+10=190
    (is (= 370 (sut/board-height 6)))  ; get-no-cols(6)=6, 6*60+10=370
    (is (= 370 (sut/board-height 7))))) ; get-no-cols(7)=6, 6*60+10=370
