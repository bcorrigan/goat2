(ns org.goat.wordle.gfx-test
  "Tests for Wordle graphics rendering with remaining words counts"
  (:require [clojure.test :refer [deftest is testing]]
            [org.goat.wordle.gfx :as gfx]
            [org.goat.wordle.analytics :as analytics])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

(deftest test-board-width-with-counts
  (testing "Board width calculation"
    (let [size 5
          width-without-counts (gfx/board-width size false)
          width-with-counts (gfx/board-width size true)]
      (is (= width-without-counts (+ (* size 60) 10)) "Base width should be size * letter-size + border")
      (is (= width-with-counts (+ width-without-counts 80)) "Width with counts should add 80px"))))

(deftest test-draw-board-without-counts
  (testing "Drawing board without counts (backwards compatibility)"
    (let [guesses-classified {"CRANE" [:semiknown :revealed :revealed :semiknown :revealed]
                              "SLOTH" [:wrong :wrong :wrong :wrong :wrong]}
          size 5
          max-guesses 6
          img (gfx/draw-board guesses-classified size max-guesses nil)]
      (is (instance? BufferedImage img))
      (is (= (.getWidth img) (gfx/board-width size false)))
      (is (> (.getHeight img) 0)))))

(deftest test-draw-board-with-counts
  (testing "Drawing board with remaining words counts"
    (let [guesses-classified {"CRANE" [:semiknown :revealed :revealed :semiknown :revealed]
                              "SLOTH" [:wrong :wrong :wrong :wrong :wrong]}
          size 5
          max-guesses 6
          remaining-words-counts [234 42]
          img (gfx/draw-board guesses-classified size max-guesses remaining-words-counts)]
      (is (instance? BufferedImage img))
      (is (= (.getWidth img) (gfx/board-width size true)) "Width should include space for counts")
      (is (> (.getHeight img) 0))
      
      ;; Optional: Save image for visual inspection
      (when (System/getProperty "save.test.images")
        (let [output-file (File. "test-board-with-counts.png")]
          (ImageIO/write img "png" output-file)
          (println (str "Test image saved to: " (.getAbsolutePath output-file))))))))

(deftest test-draw-board-empty-counts
  (testing "Drawing board with empty counts vector"
    (let [guesses-classified {"CRANE" [:semiknown :revealed :revealed :semiknown :revealed]}
          size 5
          max-guesses 6
          img (gfx/draw-board guesses-classified size max-guesses [])]
      (is (instance? BufferedImage img))
      (is (= (.getWidth img) (gfx/board-width size false)) "Empty counts should not expand width"))))

(deftest test-get-img-sync-with-counts
  (testing "get-img-sync passes counts through to draw-board"
    (let [info {:type :board
                :guesses-classified {"CRANE" [:semiknown :revealed :revealed :semiknown :revealed]
                                     "STALE" [:wrong :wrong :wrong :wrong :wrong]}
                :size 5
                :max-guesses 6
                :remaining-words-counts [156 42]}
          img (gfx/get-img-sync :test-chat info)]
      (is (instance? BufferedImage img))
      (is (= (.getWidth img) (gfx/board-width 5 true)) "Image should have expanded width for counts"))))
