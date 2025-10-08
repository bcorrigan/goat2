(ns org.goat.gfx.core-test
  (:require [org.goat.gfx.core :as sut]
            [clojure.test :refer [deftest is testing]])
  (:import [java.awt.image BufferedImage]
           [java.awt Font Color]))

;; ============================================================================
;; Canvas Creation Tests
;; ============================================================================

(deftest test-create-canvas
  (testing "Creates canvas with correct dimensions"
    (let [{:keys [canvas g2d]} (sut/create-canvas 800 600)]
      (is (instance? BufferedImage canvas))
      (is (= 800 (.getWidth canvas)))
      (is (= 600 (.getHeight canvas)))
      (is (some? g2d))
      (.dispose g2d))))

(deftest test-finalize-canvas
  (testing "Returns BufferedImage after finalization"
    (let [ctx (sut/create-canvas 100 100)
          result (sut/finalize-canvas ctx)]
      (is (instance? BufferedImage result))
      (is (= 100 (.getWidth result))))))

;; ============================================================================
;; Drawing Function Tests
;; ============================================================================

(deftest test-fill-color
  (testing "Sets fill color correctly"
    (let [{:keys [canvas g2d]} (sut/create-canvas 100 100)]
      (sut/fill! g2d 255 0 0)
      (is (= (Color. 255 0 0) (.getColor g2d)))
      (.dispose g2d))))

(deftest test-background
  (testing "Fills background with color"
    (let [{:keys [canvas g2d]} (sut/create-canvas 10 10)]
      (sut/background! g2d 10 10 100 150 200)
      ;; Check a pixel to verify background was filled
      (let [rgb (.getRGB canvas 5 5)
            color (Color. rgb)]
        (is (= 100 (.getRed color)))
        (is (= 150 (.getGreen color)))
        (is (= 200 (.getBlue color))))
      (.dispose g2d))))

(deftest test-rect-drawing
  (testing "Draws rectangle"
    (let [{:keys [canvas g2d]} (sut/create-canvas 100 100)]
      (sut/background! g2d 100 100 0 0 0)
      (sut/fill! g2d 255 255 255)
      (sut/rect! g2d 10 10 20 20)
      ;; Check that pixel inside rect is white
      (let [rgb (.getRGB canvas 15 15)
            color (Color. rgb true)]
        (is (= 255 (.getRed color)))
        (is (= 255 (.getGreen color)))
        (is (= 255 (.getBlue color))))
      (.dispose g2d))))

(deftest test-set-font
  (testing "Sets font correctly"
    (let [{:keys [g2d]} (sut/create-canvas 100 100)]
      (sut/set-font! g2d "Monospaced" Font/BOLD 24)
      (let [font (.getFont g2d)]
        (is (= "Monospaced" (.getFamily font)))
        (is (= Font/BOLD (.getStyle font)))
        (is (= 24 (.getSize font))))
      (.dispose g2d))))

(deftest test-text-bounds
  (testing "Returns text bounds"
    (let [{:keys [g2d]} (sut/create-canvas 100 100)]
      (sut/set-font! g2d "Monospaced" Font/PLAIN 20)
      (let [bounds (sut/text-bounds g2d "Hello")]
        (is (> (:width bounds) 0))
        (is (> (:height bounds) 0))
        (is (> (:ascent bounds) 0)))
      (.dispose g2d))))

(deftest test-text-drawing
  (testing "Draws text without errors"
    (let [{:keys [canvas g2d]} (sut/create-canvas 200 100)]
      (sut/background! g2d 200 100 0 0 0)
      (sut/fill! g2d 255 255 255)
      (sut/set-font! g2d "Monospaced" Font/PLAIN 20)
      ;; Should not throw
      (sut/text! g2d "Test" 100 50 :align-x :center)
      (is (some? canvas))
      (.dispose g2d))))

(deftest test-arc-drawing
  (testing "Draws arc without errors"
    (let [{:keys [canvas g2d]} (sut/create-canvas 200 200)]
      (sut/background! g2d 200 200 0 0 0)
      (sut/fill! g2d 255 0 0)
      ;; Draw a pie slice
      (sut/arc! g2d 50 50 100 100 0 90)
      (is (some? canvas))
      (.dispose g2d))))

;; ============================================================================
;; Convenience Function Tests
;; ============================================================================

(deftest test-with-canvas
  (testing "Convenience wrapper works"
    (let [img (sut/with-canvas 300 200
                (fn [{:keys [g2d]}]
                  (sut/background! g2d 300 200 50 50 50)
                  (sut/fill! g2d 255 255 0)
                  (sut/rect! g2d 10 10 50 50)))]
      (is (instance? BufferedImage img))
      (is (= 300 (.getWidth img)))
      (is (= 200 (.getHeight img)))
      ;; Check background color
      (let [rgb (.getRGB img 250 150)
            color (Color. rgb true)]
        (is (= 50 (.getRed color)))
        (is (= 50 (.getGreen color)))
        (is (= 50 (.getBlue color))))
      ;; Check rect color
      (let [rgb (.getRGB img 20 20)
            color (Color. rgb true)]
        (is (= 255 (.getRed color)))
        (is (= 255 (.getGreen color)))
        (is (= 0 (.getBlue color)))))))

;; ============================================================================
;; Integration Test - Create a Complete Image
;; ============================================================================

(deftest test-complete-image-creation
  (testing "Can create a complete image with multiple elements"
    (let [img (sut/with-canvas 400 300
                (fn [{:keys [g2d]}]
                  ;; Background
                  (sut/background! g2d 400 300 26 26 28)

                  ;; Draw some colored rectangles
                  (sut/fill! g2d 83 140 78)
                  (sut/rect! g2d 50 50 60 60)

                  (sut/fill! g2d 180 158 59)
                  (sut/rect! g2d 120 50 60 60)

                  (sut/fill! g2d 58 58 60)
                  (sut/rect! g2d 190 50 60 60)

                  ;; Draw text
                  (sut/fill! g2d 248 248 248)
                  (sut/set-font! g2d "Sans Serif" Font/BOLD 24)
                  (sut/text! g2d "Test Image" 200 200 :align-x :center)

                  ;; Draw an arc
                  (sut/fill! g2d 255 100 100)
                  (sut/arc! g2d 280 200 80 80 0 270)))]

      (is (instance? BufferedImage img))
      (is (= 400 (.getWidth img)))
      (is (= 300 (.getHeight img))))))
