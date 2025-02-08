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

;; Precomputed data structures
(defonce position-index (atom {}))
(defonce word-freqs (atom {}))
(def ^:dynamic *dict-set* (set (map :word (words/get-word :all 5 :all))))

(defn build-indexes!
  "Build indexes for the dictionary to optimize filtering."
  []
  (reset! position-index
          (reduce (fn [index word]
                    (reduce (fn [idx pos]
                              (let [c (get word pos)]
                                (update idx [pos c] (fnil conj #{}) word)))
                            index
                            (range (count word))))
                  {}
                  *dict-set*))
  (reset! word-freqs
          (zipmap *dict-set* (map frequencies *dict-set*))))

(build-indexes!)

(defn allowed-words-for-facts
  "Compute the set of words allowed by the given facts using precomputed indexes."
  [facts]
  (let [{:keys [known known-nots bounds]} facts
        ;; Apply known positions
        known-words (if (empty? known)
                      *dict-set*
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

(defn add-to-facts
  "Given a guess and facts, add additional facts"
  [facts guess answer]
  (get-facts (classify-letters guess answer) guess facts 0))

(defn valid-guess-words
  "Return guess words that meaningfully reduce possible answers - these are words that reveal new information somehow.
  Any word NOT in this set can be considered a bad mistake
  Surprisingly, even when you know a lot of information, *most* words that remain still reveal information.
  For example, support the answer is MOPER and you have guessed HOPER. LOPER DOPER ROPER and MOPER are the 4 possible correct
  answers. Yet, out of 5800 words in the dict, 3169 of them reveal new information for this set. e.g. guessing TOADY or WRYER or LAITY all reveal enough to exclude down to 3 possible words. And any word with an M in it will exclude down to 1 possible word!"
  [facts answer]
  (let [original-allowed (allowed-words-for-facts facts)
        original-count (count original-allowed)]
    (if (zero? original-count)
      []
      (->> *dict-set*
           (pmap (fn [guess-word]
                   (let [new-facts (add-to-facts facts guess-word answer)
                         new-allowed (allowed-words-for-facts new-facts)]
                     (when (and (not= original-allowed new-allowed)
                                (< (count new-allowed) original-count))
                       guess-word))))
           (doall)  ; Realize the pmap
           (remove nil?)))))

;; Thank you 3blue1brown!
(defn evaluate-guess-quality
  "Calculate the quality score for a guess based on how it partitions possible answers.
   Higher scores are better (indicate more information gain)."
  [possible-answers guess]
  (let [total (count possible-answers)
        ;; Group answers by the classification pattern they would produce
        pattern-groups (group-by (fn [answer] 
                                   (compare-guess-to-answer guess answer)) 
                                 possible-answers)
        
        ;; Calculate entropy for this partitioning
        entropy (reduce (fn [acc [pattern answers]]
                          (let [p (/ (count answers) total)
                                information (- (* p (Math/log p)))]
                            (+ acc information)))
                        0
                        pattern-groups)]
    
    ;; Return tuple of [guess score] where higher score = better
    [guess entropy]))


(defn rank-guesses
  "Rank potential guesses by their information gain against possible answers"
  [possible-answers guesses]
  (->> guesses
       (pmap (partial evaluate-guess-quality possible-answers))
       (sort-by second)))

;;(rank-guesses (allowed-words-for-facts new-facts) (valid-guess-words new-facts "MOPER"))

;;this is borked
(defn rate-guess
  "Rate a guess against possible answers with fun categories.
   Returns map with :rating and :commentary."
  [guess possible-answers]
  (let [answers-count (count possible-answers)
        ;; Calculate quality metrics
        [entropy] (evaluate-guess-quality possible-answers guess)
        sorted-guesses (sort-by second (map #(evaluate-guess-quality possible-answers %) possible-answers))
        percentile (->> sorted-guesses
                        (map-indexed (fn [idx [word score]] [word idx]))
                        (filter #(= guess (first %)))
                        ffirst
                        (#(/ (double %) (count sorted-guesses)))
                        (* 100))]
        
        ;; Classification logic
    (cond
      (= 1 answers-count) (if (= guess (first possible-answers))
                            :excellent
                            :mistake)
      (contains? possible-answers guess) :good  ; Always rate correct answers well
      (= entropy (Math/log answers-count)) :mistake  ; No information gain
      (<= percentile 5) :excellent
      (<= percentile 20) :good
      (<= percentile 50) :average
      :else :poor)))

;; (def mydict (map :word (words/get-word :all 5 :all)))
