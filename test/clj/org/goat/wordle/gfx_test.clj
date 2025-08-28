(ns org.goat.wordle.gfx-test
  (:require [org.goat.wordle.gfx :as sut]
            [clojure.test :as t :refer [deftest is testing]]))

(defn mk-buffered-image [color]
  (let [img (java.awt.image.BufferedImage. 100 100 java.awt.image.BufferedImage/TYPE_INT_RGB)]
    (doto (.getGraphics img)
      (.setColor color)
      (.fillRect 0 0 100 100)
      (.dispose))
    img))

(deftest test-append-images
  (let [red-img (mk-buffered-image java.awt.Color/RED)
        blue-img (mk-buffered-image java.awt.Color/BLUE)
        appended-img (sut/append-images red-img blue-img "red" "blue")]
    (is (= 200 (.getWidth appended-img)))
    (is (= 130 (.getHeight appended-img)))))
