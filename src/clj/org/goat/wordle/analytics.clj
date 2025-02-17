(ns org.goat.wordle.analytics
  (:require [clojure.set :as set]
            [clojure.string :as str]
			[clojure.math :as math]
            [org.goat.util.str :as strutil]
            [org.goat.db.words :as words])
  (:import [java.util Arrays]))

;; All the hardcore wordle logic goes here!
;; If you want to know:
;;   - Which letters are known in a guess? Which are known but position is not known?
;;   - How many possible answers are there given a sequence of guesses?
;;   - Which is the "best" answer to give if you don'tknow the answer, ie which guess reduces the number of possible answers the most?
;;   - How many "mistakes" and errors in a sequence of guesses?
;;   - how optimal (or suboptimal) are a sequence of guesses?
;; Well my friend, this is the namespace for you! Let's begin with the basic letter classification code that is core to wordle:


(defn classify-letters-int
  "Returns pattern as integer (2 bits per letter)"
  [^String guess ^String answer]
  (let [len (.length guess)
        ^char/1 answer-chars (.toCharArray answer)
        ^java.util.HashMap freq (java.util.HashMap.)
        ^int/1 pattern (int-array len)]
    ;; Initialize pattern array with -1
    (Arrays/fill pattern -1)
    ;; Initialize frequency map
    (dotimes [i len]
      (let [c (aget answer-chars i)]
        (.put freq c (inc (.getOrDefault freq c 0)))))
    ;; First pass: mark exact matches (00)
    (dotimes [i len]
      (let [g (.charAt guess i)
            a (aget answer-chars i)]
        (when (= g a)
          (aset pattern i 0)
          (.put freq a (dec (.get freq a))))))
    ;; Second pass: mark semiknown (01) and wrong (10)
    (dotimes [i len]
      (when (== (aget pattern i) -1)
        (let [g (.charAt guess i)
              remaining (.getOrDefault freq g 0)]
          (if (pos? remaining)
            (do
              (aset pattern i 1)
              (.put freq g (dec remaining)))
            (aset pattern i 2)))))
    ;; Encode to integer
    (loop [i 0 code 0]
      (if (< i len)
        (recur (inc i) (bit-or (bit-shift-left code 2) (aget pattern i)))
        code))))

(defn decode-pattern [code ^long word-length]
  (loop [i (dec word-length)
         pattern []
         remaining code]
    (if (>= i 0)
      (let [mask (bit-shift-right remaining (* 2 i))
            classification (case mask
                             0 :revealed
                             1 :semiknown
                             2 :wrong)]
        (recur (dec i) (conj pattern classification) 
               (bit-and remaining (dec (bit-shift-left 1 (* 2 i))))))
      pattern)))

(defn classify-letters
  [guess answer]
  (decode-pattern (classify-letters-int guess answer) (count guess)))

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
(def ^clojure.lang.PersistentHashSet dict-set (set (map :word (words/get-word :all 5 :all))))

(defn build-indexes
  "Build indexes for the dictionary to optimize filtering."
  [dict-set]
  (let [position-index (reduce (fn [index word]
                                 (reduce (fn [idx pos]
                                           (let [c (get word pos)]
                                             (update idx [pos c] (fnil conj #{}) word)))
                                         index
                                         (range (count word))))
                               {}
                               dict-set)
        word-freqs (zipmap dict-set (map frequencies dict-set))]
    {:position-index position-index
     :word-freqs word-freqs}))

(def indexes (build-indexes dict-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; allowed-words-for-facts is a huge bottleneck for our use case.
;; When we "rate guesses" we have to call it a *lot* and each time it has to apply
;; the facts to every word in the dictionary! so if few facts are known, it
;; has a huge impact on performance and profiler shows almost all time spend in that function.
;;
;; hence, the following helper functions are highly type hinted etc. This makes things a bit
;; ugly looking but gets us a good 15% extra performance. We also utilise indexes / precomputation
;; as much as possible to accellerate.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn apply_knowns [^clojure.lang.MapEntry e] 
  (let [^int pos (key e)  
        ^Character c (val e)
		^clojure.lang.PersistentHashMap position-index (:position-index indexes)
		] 
    (get position-index [(int pos) c] #{})))

(defn apply_known_nots [^clojure.lang.PersistentHashSet allowed
	 ^clojure.lang.MapEntry e] 
  (let [^int pos (key e)  
		^clojure.lang.PersistentHashMap position-index (:position-index indexes)
		^clojure.lang.PersistentHashSet excluded-chars (val e)
		^clojure.lang.PersistentHashSet excluded (apply set/union
													 (map (fn [^Character c] 
															(get position-index [(int pos) c] #{}))
														  excluded-chars))]
	(set/difference allowed excluded)))

(defn apply_bounds [^clojure.lang.PersistentHashMap word-freqs
					^clojure.lang.PersistentHashMap bounds
					^String word] 
  (let [^clojure.lang.PersistentHashMap freq (word-freqs word)] 
    (every? (fn [^clojure.lang.MapEntry e] 
              (let [^Character c (key e) 
                    {:keys [^int lower
							^int upper]} (val e)
                    cnt (int (get freq c 0)) 
                    lower-bound (int (or lower 0)) 
                    upper-bound (int (or upper Integer/MAX_VALUE))] 
                (and (>= cnt lower-bound)
                     (<= cnt upper-bound))))
            bounds)))

(defn allowed-words-for-facts
  "Compute the set of words allowed by the given facts using precomputed indexes."
  [^clojure.lang.IPersistentMap facts] 
  (let [{:keys [^clojure.lang.PersistentArrayMap known
				^clojure.lang.PersistentArrayMap known-nots
				^clojure.lang.PersistentHashMap bounds]} facts
        {:keys [^clojure.lang.PersistentHashMap word-freqs]} indexes
        ;; Apply known positions
        ^clojure.lang.PersistentHashSet known-words (if (empty? known)
													  dict-set
													  (apply set/intersection
															 (map apply_knowns known)))

        ;; Apply known-nots
        ^clojure.lang.PersistentHashSet without-known-nots (reduce apply_known_nots
																   known-words
																   known-nots)
		;; Apply bounds
		^clojure.lang.PersistentHashSet bounds-words (filter (partial apply_bounds word-freqs bounds)
                               without-known-nots)]
    bounds-words))

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
      (->> dict-set
           (pmap (fn [guess-word]
                   (let [new-facts (add-to-facts facts guess-word answer)
                         new-allowed (allowed-words-for-facts new-facts)]
                     (when (< (count new-allowed) original-count)
                       guess-word))))
           (doall)  ; Realize the pmap
           (remove nil?)))))

;; Precompute log cache during namespace initialization
(def ^:private max-log-cache-size 6000)  ; Cover typical maximum possible answer counts
(def ^:private log-cache 
  (let [arr (double-array max-log-cache-size)]
    (dotimes [i max-log-cache-size]
      (aset arr i (Math/log (inc i))))  ; Store log(1) to log(20000)
    arr))

(defn- get-log [n]
  (if (and (>= n 1) (< n max-log-cache-size))
    (aget log-cache (dec n))
    (Math/log n)))

(defn evaluate-guess-quality
  "Optimized version with precomputed logs and array access"
  [^java.util.HashMap possible-answers ^String guess]
  (let [total (count possible-answers)
        arr-answers (object-array possible-answers)  ; Faster than vec for iteration
        ^java.util.HashMap pattern-counts (java.util.HashMap.)
        log-total (get-log total)]
    (dotimes [i (alength arr-answers)]
      (let [^String answer (aget arr-answers i)
            ^java.util.HashMap pattern-code (classify-letters-int guess answer)]
        (.put pattern-counts pattern-code (inc (.getOrDefault pattern-counts pattern-code 0)))))
    (let [entropy (atom 0.0)]
      (doseq [[_ cnt] pattern-counts]
        (let [p (/ cnt total)
              term (- (* p (- (get-log cnt) log-total)))]
          (swap! entropy + term)))
      [guess @entropy])))

(defn rank-guesses
  "Rank potential guesses by their information gain against possible answers"
  [possible-answers guesses]
  (->> guesses
       (pmap (partial evaluate-guess-quality possible-answers))
       (sort-by second)))

(defn count-from-threshold [freq-map threshold]
  (reduce (fn [sum [value count]]
            (if (>= value threshold)
              (+ sum count)
              sum))
          0
          freq-map))

;;(def new-facts (get-facts (classify-letters "MOPER" "DOPER") "MOPER"))
;;(def guesses-ranked (rank-guesses (allowed-words-for-facts new-facts) (valid-guess-words new-facts "DOPER")))
;;(count (distinct (map second guesses-ranked)))

(defn rate-guess
  "Return an integer representing the quality of the users guess, given their known information, from 0-5. 5 is a *perfect* guess, that is, reveals the maximum possible information. 0 means no new informaiton was revealed."
  [guess answer facts]
  (let [possible-answers (allowed-words-for-facts facts) ;;usually a few of these
		possible-guesses (valid-guess-words facts answer) ;;MANY of these
		guesses-ranked (rank-guesses possible-answers possible-guesses) ;;list of tuple of guess-score
		freqs (frequencies (map second guesses-ranked))
		guess-score (second (first (filter #(= (first %) guess) guesses-ranked)))]
	(if (nil? guess-score)
	  0
	  (let [score-count (count-from-threshold freqs guess-score)
			total-count (count possible-guesses)
			percentile (* 100 (/ score-count total-count))
			rating (int (math/floor (/ (- 100 percentile) 10.0)))
			max-score (apply max (map first freqs))] 
		(if (= guess-score max-score)
		  10 ;; we always give 10 if the guess is as good as possible
		  (if (= rating 0)
			1 ;;if the score would be 0.. bump it up to 1.
			rating))))))

;; (def mydict (map :word (words/get-word :all 5 :all)))
