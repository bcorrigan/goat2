(ns org.goat.wordle.analytics
  (:require [clojure.math :as math]
            [org.goat.db.words :as words]
            [org.goat.util.str :as strutil]
            [clojure.string :as str]))

;; All the hardcore wordle logic goes here!
;; If you want to know:
;;   - Which letters are known in a guess? Which are known but position is not known?
;;   - How many possible answers are there given a sequence of guesses?
;;   - Which is the "best" answer to give if you don'tknow the answer, ie which guess reduces the number of possible answers the most?
;;   - How many "mistakes" and errors in a sequence of guesses?
;;   - how optimal (or suboptimal) are a sequence of guesses?
;; Well my friend, this is the namespace for you! Let's begin with the basic letter classification code that is core to wordle:

(use 'clojure.test)

(defn mask-answer
  [guess answer]
  (vec (for [i (range (count guess))
             :let [ges_let (get guess i)
                   ans_let (get answer i)]]
         (if (= ges_let ans_let) \. ans_let))))

(defn mask-first
  "Mask the first instance of c in answer"
  [answer c]
  (clojure.string/replace-first answer c "."))

(defn classify-letters
  "See compare-guess-to-answer. This is the main implementation. We \"mask\" the answer where letters are correct
  first, then we simply compare letter by letter, if it matches it is :revealed, if it doesn't and is in masked_answer
  then it is :semiknown, otherwise it is :wrong. Then we conj with recursive call for next letter."
  ([guess answer] (classify-letters guess answer (mask-answer guess answer)))
  ([guess answer masked-ans]
   (if (empty? guess)
     '()
     (let [ges_let (first guess)
           ans_let (first answer)
           ans_rest (str/join (rest guess))
           ges_rest (str/join (rest answer))]
       (cond (= ges_let ans_let) (conj (classify-letters ans_rest ges_rest masked-ans)
                                       :revealed)
             (strutil/contains-char? masked-ans ges_let)
             (conj
              (classify-letters ans_rest ges_rest (mask-first masked-ans ges_let))
              :semiknown)
             :else (conj (classify-letters ans_rest ges_rest masked-ans) :wrong))))))

(defn compare-guess-to-answer
  "Compare given guess to answer. For each letter:
    If letter is correct and in right position - :revealed
    If letter is in the answer, but in the wrong position - :semiknown
    If letter is NOT in the answer - :wrong
    A vec of these keywords is returned for each letter in the supplied guess."
  [guess answer]
  (vec (classify-letters guess answer)))

;; This was a tricky fn so lets have a test for once
(deftest test-compare-guess-to-answer
  (is (= [:wrong :semiknown :wrong :semiknown :revealed]
         (compare-guess-to-answer "FEELS" "PALES")))
  (is (= [:wrong :semiknown :wrong :revealed :revealed]
         (compare-guess-to-answer "FLEES" "PALES")))
  (is (= [:wrong :revealed :revealed :revealed :wrong]
         (compare-guess-to-answer "GLEES" "FLEET")))
  (is (= [:semiknown :semiknown :revealed :wrong :wrong]
         (compare-guess-to-answer "EELSY" "AGLEE")))
  (is (= [:semiknown :semiknown :wrong :wrong :wrong]
         (compare-guess-to-answer "EEESY" "AGLEE"))))
