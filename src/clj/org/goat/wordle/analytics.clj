(ns org.goat.wordle.analytics
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [org.goat.util.str :as strutil]
            [org.goat.db.words :as words]))

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
  "Given classifications and guess, returns facts with:
   - :known = map of position → confirmed letter
   - :bounds = map of letters → {:lower X :upper Y}
   - :known-nots = map of position → set of excluded letters *at explicit positions*"
  ([classifications guess]
   (get-facts classifications guess {:known {} :bounds {} :known-nots {} :current-guess-counts {}} 0))
  ([classifications guess facts index]
   (if (empty? classifications)
     ;; After processing all classifications, update :bounds with :current-guess-counts
     (let [current-guess-counts (:current-guess-counts facts)
           ;; Update lower bounds and ensure upper >= lower
           new-bounds (reduce (fn [bounds [letter count]]
                                (let [current-lower (get-in bounds [letter :lower] 0)
                                      new-lower (max current-lower count)
                                      current-upper (get-in bounds [letter :upper] Integer/MAX_VALUE)
                                      new-upper (if (> new-lower current-upper)
                                                  new-lower
                                                  current-upper)
                                      new-bounds (assoc-in bounds [letter :lower] new-lower)]
                                  (if (< new-upper 20)
                                    (assoc-in new-bounds [letter :upper] new-upper)
                                    new-bounds)))
                              (:bounds facts)
                              current-guess-counts)]
       (-> facts
           (assoc :bounds new-bounds)
           (dissoc :current-guess-counts)))
     (let [classfn (first classifications)
           letter (first guess)
           rest-classes (rest classifications)
           rest-guess (rest guess)
           current-known (:known facts)
           current-bounds (:bounds facts)
           current-known-nots (:known-nots facts)
           current-guess-counts (:current-guess-counts facts)
           ;; Update current-guess-counts if classfn is :revealed or :semiknown
           new-guess-counts (if (or (= classfn :revealed) (= classfn :semiknown))
                              (update current-guess-counts letter (fnil inc 0))
                              current-guess-counts)
           new-known-nots (if (or (= classfn :semiknown) (= classfn :wrong))
                            (update current-known-nots index (fn [s] (conj (or s #{}) letter)))
                            current-known-nots)
           ;; For :wrong, process upper bound
           process-upper (fn [bounds]
                           (let [current-letter-lower (get-in bounds [letter :lower] 0)
                                 new-upper (if (zero? current-letter-lower)
                                             0
                                             current-letter-lower)
                                 current-upper (get-in bounds [letter :upper] Integer/MAX_VALUE)
                                 updated-upper (min current-upper new-upper)]
                             (assoc-in bounds [letter :upper] updated-upper)))]
       (cond
         (= :revealed classfn)
         (let [new-known (assoc current-known index letter)]
           (recur rest-classes rest-guess
                  (-> facts
                      (assoc :known new-known)
                      (assoc :known-nots new-known-nots)
                      (assoc :current-guess-counts new-guess-counts)
                      (assoc :bounds current-bounds))
                  (inc index)))
         
         (= :semiknown classfn)
         (recur rest-classes rest-guess
                (-> facts
                    (assoc :known-nots new-known-nots)
                    (assoc :current-guess-counts new-guess-counts)
                    (assoc :bounds current-bounds))
                (inc index))
         
         (= :wrong classfn)
         (let [new-bounds (process-upper current-bounds)]
           (recur rest-classes rest-guess
                  (-> facts
                      (assoc :known-nots new-known-nots)
                      (assoc :current-guess-counts new-guess-counts)
                      (assoc :bounds new-bounds))
                  (inc index))))))))


(deftest test-get-facts-basic
  (let [classfns '(:wrong :wrong :wrong :wrong :revealed)
        facts (get-facts classfns "ABCDE")]
    (is (= {\A {:upper 0},
            \B {:upper 0},
            \C {:upper 0},
            \D {:upper 0},
            \E {:lower 1}}
           (:bounds facts))
        "A-D should have an upper bound of 0, E should have an unknown upper bound and a lower bound of 1")
    (is (= {4 \E}
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
    (is (= {4 \E}
           (:known facts))
        "E is known at position 4 and nothing else")
    (is (= {0 #{\E}, 1 #{\B}, 2 #{\C}, 3 #{\D} }
           (:known-nots facts))
        "E is known to not be at pos 0, B is known to not be at pos 1, and C&D as semiknown are known to not be at 2 and 3")))

(deftest test-get-more-facts
  (let [classfns1 '(:wrong :wrong :semiknown :semiknown :revealed)
        facts1 (get-facts classfns1 "EBCDE")
        classfns2 '(:wrong :revealed :semiknown :wrong :revealed)
        facts2 (get-facts classfns2 "XCDXE" facts1 0)]
    (is (= {4 \E, 1 \C} ; Position 1 maps to C, position 4 to E
           (:known facts2))
        "C added at pos 1, E remains at pos 4")
    ;; fails because both \C and \D are incorrectly bounded as {:lower 1 :upper 2147483647} suggesting an overflow!
    ;; other letters are correct
    (is (= {\E { :upper 1 :lower 1 },
            \B { :upper 0 },
            \C { :lower 1 },
            \D { :lower 1 },
            \X { :upper 0 }}
           (:bounds facts2))
        "C and D are still lower 1, X is resolved to upper 0, E remains are upper 1 lower 1")
    (is (= {0 #{\E \X}, 1 #{\B}, 2 #{\C \D}, 3 #{\D \X}}
           (:known-nots facts2))
        ":known-nots should be as listed")
    ))

(defn word-matches-known?
  "test word matches given known letter positions"
  [known-letters word]
  (if (= known-letters '())
    true
    (let [i (ffirst known-letters)
          known-letter (second (first known-letters))
          word-letter (get word i)]
      (and (= known-letter
              word-letter)
           (recur (rest known-letters) word)))))

(deftest test-word-matches-known?
  (is (and
       (word-matches-known? {0 \A 4 \A} "AXXXA")
       (word-matches-known? {0 \A 2 \A 3 \X} "AXAXA")
       (word-matches-known? {1 \X 4 \A} "AXXXA")
       (word-matches-known? {3 \Q 4 \M 0 \X 2 \Z 1 \Y} "XYZQM")
       (word-matches-known? {} "AXXXA"))
      "These should all match correctly.")
  (is (not
       (or
        (word-matches-known? {0 \A 3 \A} "AXXXA")
        (word-matches-known? {0 \A 2 \A 4 \X} "AXAXA")
        (word-matches-known? {0 \X 4 \A} "AXXXA")
        (word-matches-known? {3 \Q 4 \Q 0 \X 2 \Z 1 \Y} "XYZQM")
        (word-matches-known? {4 \X} "AXXXA")))
      "None of these should match"))

(defn word-matches-known-nots?
  "Test word does not have letter in these positions"
  [known-nots word]
  (if (= known-nots '())
    false
    (let [i (ffirst known-nots)
          known-letters (second (first known-nots))
          word-letter (get word i)]
      (or (not (contains? known-letters
              word-letter))
           (recur (rest known-nots) word)))))

(deftest test-word-matches-known-nots?
  (is (and
       (word-matches-known-nots? {0 #{\Z \B} 4 #{\B \C}} "AXXXA")
       (word-matches-known-nots? {1 #{\A \C \D \E} 4 #{\A \B \C \D}} "ABCDE")
       (word-matches-known-nots? {0 #{\X} 4 #{\B \C} 1 #{\Z} 2 #{\Q} 3 #{\M} } "AXXXA")
       (word-matches-known-nots? {4 #{\X}} "AXXXA"))
      "These should all match correctly.")
  (is (not
       (or
        (word-matches-known-nots? {4 #{\A}} "AXXXA")
        (word-matches-known-nots? {0 #{\A} 1 #{\X} 2 #{\X} 3 #{\X} 4 #{\A} } "AXXXA")
        (word-matches-known-nots? {0 #{\A \B} 1 #{\B \C \D} 2 #{\B \C} 3 #{\M \D} 4 #{\E \F \B} } "ABCDE")))
      "None of these should match"))

(defn word-matches-bounds?
  "Test if given word matches given bounds - the bounds being letter frequencies"
  ([bounds word]
   (word-matches-bounds? bounds (frequencies word) (count word)))
  ([bounds word-frequencies max-bound]
  (if (= bounds '())
    true
    (let [letter (ffirst bounds)
          letter-bound (second (first bounds))
          upper-bound (get letter-bound :upper max-bound)
          lower-bound (get letter-bound :lower 0)
          letter-count (get word-frequencies letter 0)]
      (and (<= letter-count
              upper-bound)
           (>= letter-count
              lower-bound)
           (recur (rest bounds) word-frequencies max-bound ))))))

(deftest test-word-matches-bounds?
  (let [classfns (classify-letters "AABBX" "AXAZB") ;;rvd smi smi wrg smi
        facts (get-facts classfns "AABBX")
        bounds (:bounds facts)]
    (is (and
         (word-matches-bounds? bounds "AXAZB")
         (word-matches-bounds? bounds "AABQX")
         (word-matches-bounds? bounds "AXABA")
         (word-matches-bounds? bounds "ABNAX")
         (word-matches-bounds? {} "POOPS")))
    (is (not (or
         (word-matches-bounds? bounds "AABBX")
         (word-matches-bounds? bounds "AXBBX")
         (word-matches-bounds? bounds "AXBBA")
         (word-matches-bounds? bounds "AXQQA")
         (word-matches-bounds? bounds "AZBBA"))))))


(defn word-matches-facts?
  "Test the given word matches all the given facts.
   Facts look like e.g.
  {:known {4 E, 1 C},
   :bounds {E {:upper 1, :lower 1}, B {:upper 0}, C {:lower 1}, D {:lower 1}, X {:upper 0}},
   :known-nots {0 #{E X}, 1 #{B}, 2 #{C D}, 3 #{D X}}}
  Broadly - letters known to exist at a given position, letters known NOT to exist at a given position, and bounds on letters."
  [facts word]
  (and (word-matches-known? (:known facts) word)
       (word-matches-known-nots? (:known-nots facts) word)
       (word-matches-bounds? (:bounds facts) word )))


;;OK, lets discover the perfect starting word.
;; Worked example
;; If possible "dicitonary" is AA, AC, CC or CD:
;; Guess "AA" ->   25% chance right answer
;;                 25% chance AC is revealed as right
;;                 50% chance we reveal it is CC or CD potentially requiring another guess
;; Guess "AC" ->   25% chance right answer
;;                 75% chance one of AC, CC, or CD is revealed as right
;; Therefore, AC is a superior guess to AA if we don't know what answer is.
;; How to calcuate this via get-facts etc above?
;; for each (AA, AC, CC, CD) set one as possible answer:
;;    for each (AA, AC, CC, CD) set one as the guess:
;;             Get the revealed "fact"
;;             For each (AA, AC, CC, CD) test if it matches fact to get a count of matches
;;                 add this up
;;                 resulting map of word->count - one with lowest count wins

;;(def mydict (map :word (words/get-word :normal 5 :all)))

;; Precomputed data structures
(defonce position-index (atom {}))
(defonce word-freqs (atom {}))
(defonce dict-set (atom #{}))

(defn build-indexes!
  "Build indexes for the dictionary to optimize filtering."
  [dict]
  (reset! dict-set (set dict))
  (reset! position-index
          (reduce (fn [index word]
                    (reduce (fn [idx pos]
                              (let [c (get word pos)]
                                (update idx [pos c] (fnil conj #{}) word)))
                            index
                            (range (count word))))
                  {}
                  dict))
  (reset! word-freqs
          (zipmap dict (map frequencies dict))))

;;populate the indexes
(build-indexes! (map :word (words/get-word :all 5 :all)))

(defn allowed-words-for-facts
  "Compute the set of words allowed by the given facts using precomputed indexes."
  [facts]
  (let [{:keys [known known-nots bounds]} facts
        ;; Apply known positions
        known-words (if (empty? known)
                      @dict-set
                      (apply set/intersection
                             (map (fn [[pos c]] (get @position-index [pos c] #{}))
                                  known)))
        ;; Apply known-nots
        without-known-nots (reduce (fn [allowed [pos excluded-chars]]
                                     (let [excluded (apply set/union
                                                           (map #(get @position-index [pos %] #{})
                                                                excluded-chars))]
                                       (set/difference allowed excluded)))
                                   known-words
                                   known-nots)
        ;; Apply bounds
        bounds-words (filter (fn [word]
                               (let [freq (@word-freqs word)]
                                 (every? (fn [[c {:keys [lower upper]}]]
                                           (let [cnt (get freq c 0)]
                                             (and (>= cnt (or lower 0))
                                                  (<= cnt (or upper Integer/MAX_VALUE)))))
                                         bounds)))
                             without-known-nots)]
    (set bounds-words)))

;; Takes 30 seconds on an 8 core ryzen for a dict size of 5800 words
;; But should be good enough for analysis after first guess when possible dict size is massively reduced
(defn optimal-guesses
  "Return map of possible guesses->match counts, optimised with indexes."
  [dict]
   ;;(build-indexes! dict)
   (let [dict-vec (vec dict)
         ;; Precompute to avoid recalculating
         process-guess (fn [guess]
                         (let [pattern-groups (->> dict-vec
                                                   (pmap #(vector (compare-guess-to-answer guess %) %))
                                                   (group-by first))
                               total (reduce-kv (fn [acc pattern answers]
                                                  (let [facts (get-facts pattern guess)
                                                        allowed (allowed-words-for-facts facts)
                                                        cnt (* (count answers) (count allowed))]
                                                    (+ acc cnt)))
                                                0
                                                pattern-groups)]
                           {guess total}))]
     (->> dict-vec
          (pmap process-guess)
          (into {}))))

(deftest test-optimal-guesses
  (let [dict '("AA" "AC" "CC" "CD" "CX" "XC" "QQ" "AB")]
    (build-indexes! dict)
    (let [optimals (optimal-guesses dict) ;;AC is best QQ is worst
          best (ffirst (sort-by val < optimals))
          worst (ffirst (sort-by val > optimals))]
      (is (= best "AC") "AC should be the BEST choice")
      (is (= worst "QQ") "QQ should be the WORST choice"))))


;; (def mydict (map :word (words/get-word :all 5 :all)))
