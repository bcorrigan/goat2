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


(defn get-facts
  "Given a map of guesses->classifications, this will calculate and return a list of facts,
  comprising 2 categories:
    - :known - letters known to exist in the word at the given index (ie green in wordle) - a vec of pairs char->position
    - :bounds - bounds for letters where we know some information - a map of chars to :upper and/or :lower bound
    - :known-nots - letters known to NOT exist in the word at the given index. A map of indexes to chars"
  ([classifications guess] (get-facts classifications guess {:known [] :bounds {} :known-nots {}} 0))
  ([classifications guess facts index]
   (if (empty? classifications)
     facts
     (let [classfn (first classifications)
           letter (first guess)
           rest-classes (rest classifications)
           rest-guess (rest guess)
           current-known (:known facts)
           current-bounds (:bounds facts)
           current-known-nots (:known-nots facts)
           new-known-nots (if (or (= classfn :semiknown) (= classfn :wrong))
                            (update current-known-nots index (fn [s] (conj (or s #{}) letter)))
                            current-known-nots)
           process-bounds (fn [bounds lower-key]
                            (let [letter-lower (inc (get-in bounds [letter lower-key] 0))
                                  new-bounds (assoc-in bounds [letter :lower] letter-lower)
                                  current-upper (get-in new-bounds [letter :upper] Integer/MAX_VALUE)]
                              (if (and (not= current-upper Integer/MAX_VALUE)
                                       (< current-upper letter-lower))
                                (assoc-in new-bounds [letter :upper] letter-lower)
                                new-bounds)))]
       (cond
         (= :revealed classfn)
         (let [new-known (conj current-known [letter index])
               new-bounds (process-bounds current-bounds :lower)]
           (recur rest-classes rest-guess
                  {:known new-known
                   :bounds new-bounds
                   :known-nots new-known-nots}
                  (inc index)))
         
         (= :semiknown classfn)
         (let [new-bounds (process-bounds current-bounds :lower)]
           (recur rest-classes rest-guess
                  {:known current-known
                   :bounds new-bounds
                   :known-nots new-known-nots}
                  (inc index)))
         
         (= :wrong classfn)
         (let [current-letter-lower (get-in current-bounds [letter :lower] 0)
               new-upper (if (zero? current-letter-lower)
                           0
                           current-letter-lower)
               new-bounds (assoc-in current-bounds [letter :upper] new-upper)]
           (recur rest-classes rest-guess
                  {:known current-known
                   :bounds new-bounds
                   :known-nots new-known-nots}
                  (inc index))))))))


(deftest test-get-facts-basic
  (let [classfns '(:wrong :wrong :wrong :wrong :revealed)
        facts (get-facts classfns "ABCDE")]
    (println "facts:" facts)
    (is (= {\A {:upper 0},
            \B {:upper 0},
            \C {:upper 0},
            \D {:upper 0},
            \E {:lower 1}}
           (:bounds facts))
        "A-D should have an upper bound of 0, E should have an unknown upper bound and a lower bound of 1")
    (is (= [[\E 4]]
           (:known facts))
        "E is known at position 4 and nothing else")
    (is (= {0 #{\A}, 1 #{\B}, 2 #{\C}, 3 #{\D}}
           (:known-nots facts))
        "A is known to not be at pos 0, B is known to not be at pos 1 etc.")))

(deftest test-get-facts-semiknown
  (let [classfns '(:wrong :wrong :semiknown :semiknown :revealed)
        facts (get-facts classfns "EBCDE")]

    (is (= {\E {:upper 1 :lower 1},
            \B {:upper 0},
            \C {:lower 1},
            \D {:lower 1}}
           (:bounds facts))
        "E should have an upper and lower bound of 1. B should have upper bound 0. C and D should have lower bound of 1.")
    (is (= [[\E 4]]
           (:known facts))
        "E is known at position 4 and nothing else")
    (is (= {0 #{\E}, 1 #{\B}, 2 #{\C}, 3 #{\D} }
           (:known-nots facts))
        "E is known to not be at pos 0, B is known to not be at pos 1, and C&D as semiknown are known to not be at 2 and 3")))

