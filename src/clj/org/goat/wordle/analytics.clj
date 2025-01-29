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

(defn get-facts-old
  "Given a map of guesses->classifications, this will calculate and return a list of facts,
  comprising 2 categories:
    - :known - letters known to exist in the word at the given index (ie green in wordle) - a vec of pairs char->position
    - :bounded - bounds for letters where we know some information - a map of chars to :upper and/or :lower bound
    - :known-nots - letters known to NOT exist in the word at the given index. A map of indexes to chars

  By representing in this way, we can \"add\" multiple facts together in a simple way and then quickly check all known facts
  at once.

  Some example responses:

  Answer: ABBAS Guess: LOOOS
  Response: {:known [('S' 4)]
             :bounded {'L' {:upper 0} 'O' {:upper 0} 'S' {:lower 1} }
             :known-nots { 0 #{'L'} 1 #{'O'} 2 #{'O'} 3 #{'O'} }

  Answer: ABBAS Guess: ABABA
  Response: {:known [('A' 0) ('B' 1)] :bounded {'A' {:lower 2 :upper 2} 'B' {:lower 2}  }  }

  As ALL letters have an :upper bound of 5 (and :lower of 0) at start of game, we don't calculate this obvious fact. Only if :lower >0 do we represent it, and only if :upper < 5 - otherwise it is the default value. The default :upper is (5 - sum of :lower bounds + this letter's lower bound) and default lower is always 0. So for example ABABA above default :upper is (5 - (2 + 2)) = 1. So we know that 'Z' may appear between 0 and 1 times - while 'B' is 2 or 3 times.

  However, we don't pass the answer as a param - rather we pass the classifications (from compare-guess-to-answer)
  and the guess. That way we're separating our comparison of guess to answer to figure out each letter status - from the
  logic for composing facts."
  ([classifications guess] (get-facts classifications guess {}))
  ([classifications guess bounds] (get-facts classifications guess bounds 0))
  ([classifications guess bounds index]
  ;; um tbd
   (let [classfn (first classifications)
         letter (first guess)]
     (cond (= :revealed classfn)
           (do
             ;; add '(guess-char index) pair to :known, add to :bounded and recur
             ;;if :bounded->:upper = :known count for the letter, remove from :known-nots too

             )
           (= :semiknown classfn)
           (do
             ;; add to :bounded, add to :known-nots and recur
             )
           (= :wrong classfn)
           (do
             ;; add to add to bounded, and recur
             )
           )
     )
   ))


(defn get-facts
  "Given a map of guesses->classifications, this will calculate and return a list of facts,
  comprising 3 categories:
    - :known - letters known to exist in the word at the given index (ie green in wordle) - a vec of pairs char->position
    - :bounded - bounds for letters where we know some information - a map of chars to :upper and/or :lower bound
    - :known-nots - letters known to NOT exist in the word at the given index. A map of indexes to chars"
  ([classifications guess]
   (get-facts classifications guess {:known [] :bounded {} :known-nots {}}))
  ([classifications guess bounds]
   (get-facts classifications guess bounds 0))
  ([classifications guess bounds index]
   (if (empty? classifications)
     bounds
     (let [[classification letter] (first classifications)]
       (case classification
         :revealed
         (let [new-known (conj (:known bounds) [letter index])
               bounded-letter (get (:bounded bounds) letter {:lower 0 :upper 5})
               new-lower (inc (:lower bounded-letter))
               new-upper (if (>= (:upper bounded-letter) 5)
                          (:upper bounded-letter)
                          (dec (:upper bounded-letter)))]
           (recur
            (rest classifications)
            (rest guess)
            (-> bounds
                (assoc :known new-known)
                (assoc-in [:bounded letter :lower] new-lower)
                (assoc-in [:bounded letter :upper] new-upper)
                (update-in :known-nots
                           (fn [m]
                             (reduce (fn [m idx]
                                       (update m idx disj letter))
                                     m
                                     (range 5)))))))

         :semiknown
         (let [new-bounds (assoc-in (:bounded bounds) [letter] {:lower (get-in (:bounded bounds) [letter :lower] 0)})
               new-bounds (assoc-in new-bounds [letter :upper] (get-in (:bounded bounds) [letter :upper] 5))
               new-known-nots (update-in (:known-nots bounds) [index] (fnil conj #{}) letter)]
           (recur
            (rest classifications)
            (rest guess)
            (-> bounds
                (update :bounded merge new-bounds)
                (assoc :known-nots new-known-nots))))

         :wrong
         (let [new-known-nots (reduce
                               (fn [m idx]
                                 (update m idx (fnil conj #{}) letter))
                               (:known-nots bounds)
                               (range 5))]
           (recur
            (rest classifications)
            (rest guess)
            (-> bounds
                (update :bounded merge (:bounded bounds))
                (assoc :known-nots new-known-nots))))))))


(defn update-bounds
  "Helper function to update bounds for a letter"
  [bounds letter classification]
  (println bounds " " letter " " classification )
  (let [current (get bounds letter {:lower 0 :upper 5})
        new-lower (if (= classification :wrong)
                    (:lower current)
                    (inc (:lower current)))
        new-upper (if (= classification :wrong)
                    (dec (:upper current))
                    (:upper current))]
    (cond-> {}
      (> new-lower 0) (assoc :lower new-lower :upper (:upper current))
      (< new-upper 5) (assoc :upper new-upper :lower (:lower current) ))))

(defn get-facts
  "Given a map of guesses->classifications, this will calculate and return a list of facts,
  comprising 3 categories:
    - :known - letters known to exist in the word at the given index (ie green in wordle) - a vec of pairs char->position
    - :bounded - bounds for letters where we know some information - a map of chars to :upper and/or :lower bound
    - :known-nots - letters known to NOT exist in the word at the given index. A map of indexes to chars"
  ([classifications guess] (get-facts classifications guess {:known [] :bounded {} :known-nots {}}))
  ([classifications guess bounds] (get-facts classifications guess bounds 0))
  ([classifications guess bounds index]
   (if (empty? classifications)
     bounds
     (let [classification (first classifications)
           letter (first guess)
           new-letter-bounds (update-bounds (:bounded bounds) letter classification)
           known-count (count (filter #(= (second %) letter) (:known bounds)))
           upper-bound (get-in new-letter-bounds [:upper] 5)]
       (case classification
         :revealed
         (recur (rest classifications)
                (rest guess)
                (-> bounds
                    (update :known conj [letter index])
                    (update-in [:bounded letter] merge new-letter-bounds)
                    (cond-> (= known-count (dec upper-bound))
                      (update :known-nots #(reduce (fn [m i] (update m i disj letter)) % (range 5)))))
                (inc index))

         :semiknown
         (recur (rest classifications)
                (rest guess)
                (-> bounds
                    (update-in [:bounded letter] merge new-letter-bounds)
                    (update-in [:known-nots index] (fnil conj #{}) letter))
                (inc index))

         :wrong
         (recur (rest classifications)
                (rest guess)
                (-> bounds
                    (update-in [:bounded letter] merge new-letter-bounds)
                    (update :known-nots #(reduce (fn [m i] (update m i (fnil conj #{}) letter)) % (range 5))))
                (inc index)))))))
