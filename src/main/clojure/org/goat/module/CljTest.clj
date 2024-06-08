(ns org.goat.module.CljTest (:gen-class  
          :extends org.goat.core.Module)
          (:require [quil.core :as q :include-macros true]))

(def guesses ["WRONG" "SPETH" "JANET", "HOUSE"])
(def answer "RIGHT")

(defn contains-char? [string c]
  (boolean (some #(= % c) string)))

(defn tosyms [guess]
  (vec (for [i (range (count guess))
        :let [ges_let (get guess i)
              ans_let (get answer i)]]
    (cond
      (= ges_let ans_let) :revealed
      (contains-char? answer ges_let) :semiknown
      :else :wrong))))

(defn draw-letter [c x y style]
  (cond 
    (= :revealed style) (q/fill 83 140 78)
    (= :wrong style) (q/fill 58 58 60)
    (= :semiknown style) (q/fill 180 158 59))
  ;(q/fill "#b49e3b")
  (q/stroke 17 17 18)
  (q/rect x y 50 50)
  (q/fill 248 248 248)
  (q/text-align :center)
  (q/text-font (q/create-font "Source Code Pro" 40))
  ;(q/text-size 40)
  (q/text (str c) (+ 25 x) (+ 40 y)))

(defn draw-unrevealed [x y]
  (q/fill 26 26 28)
  (q/stroke 168 168 168)
  (q/rect x y 50 50))

(defn draw-board [gr]
  (q/with-graphics gr
  (q/background 17 17 18)
  (println (range 10 (+ 10 (* 60 (count guesses))) 60))
  
  (doseq [i (range (count guesses))
    	    j (range (count (get guesses i)))]
  	(let [guess (get guesses i)
    	  letter (nth (seq guess) j)
          sym (get (tosyms guess) j)
       	  y (+ 10 (* 60 i))
       	  x (+ 10 (* 60 j))]
      (draw-letter letter x y sym)))
  
  (doseq [y (range (+ 10 (* 60 (count guesses))) (+ 10 (* 60 6)) 60)
          x (range 10 260 60)]
    (draw-unrevealed x y))))

(defn draw []
  (let [gr (q/create-graphics 310 370 :java2d "/tmp/wordle.png")]
    (q/with-graphics gr
      (draw-board gr)
      (q/save "/tmp/wordle.png")      
      (.dispose gr)
      (q/exit))))
  
(defn -processChannelMessage
  [_ m] 
  (q/defsketch org.goat.module.CljTest
    :host "host"
    :size [310 370]
    :setup draw)
  ;(draw)
  (.reply m "OK bazz, check image was written now."))

(defn -processPrivateMessage
  [this m]
  (-processChannelMessage this m))

(defn -getCommands
  [_]
  (into-array String '("wordle")))

