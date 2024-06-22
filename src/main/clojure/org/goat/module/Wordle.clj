(ns org.goat.module.Wordle
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:require [quil.core :as q :include-macros true]
            [org.goat.db.words :as words]
            [org.goat.db.users :as users]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.edn :as edn]))
(use 'clojure.test)

(def max-guesses 6)
(def letter-size 60)
(def letter-border 10)
(def A-Z (set (map char (concat (range 65 91)))))

(def state
  "key is :game-states->chatid - returning
  {:guesses [\"WRONG HOUSE\"] :answer \"RIGHT\"
  :size 5 :answerdef \"definition\" :hits 1000 }"
  (atom {}))

(defn playing?
  "true if we're playing a game on the given chatid"
  [chat-key]
  (not (nil? (get-in @state [:game-states chat-key]))))

(defn guesses-made
  "How many guesses have been made for the given chatid game?"
  [chat-key]
  (count (get-in @state [:game-states chat-key :guesses])))

(defn add-to-col!
  "Append the given value to the given collection.
  col is a symbol either :guesses, :excluded-letters, :included-letters"
  [chat-key col val]
  (let [current-vals (get-in @state [:game-states chat-key col])]
    (swap! state assoc-in [:game-states chat-key col] (conj current-vals val))))

(defn add-inc-letter!
  "Append the given letter to the list of letters the user has
  seen in the word"
  [chat-key letter]
  (add-to-col! chat-key :included-letters letter))

(defn add-exc-letter!
  "Append the given letter to the list of definitely excluded letters."
  [chat-key letter]
  (add-to-col! chat-key :excluded-letters letter))

(defn get-gameprop
  "Get given property for given chatid game"
  [chat-key property]
  (get-in @state [:game-states chat-key property]))

(defn set-gameprop
  "Set given value for given chatid game"
  [chat-key property value]
  (swap! state assoc-in [:game-states chat-key property] value))

(defn new-game!
  "Add state for a new game of wordle."
  [chat-key answer size answerdef hits user difficulty]
  (let [starttime (System/currentTimeMillis)]
    (swap! state assoc-in
      [:game-states chat-key]
      {:guesses [],
       :excluded-letters #{},
       :included-letters #{},
       :answer answer,
       :size size,
       :type :single, ;;can be :single (if one player) or :multi
       :starttime starttime,
       :user user,
       :difficulty difficulty,
       :answerdef answerdef,
       :hits hits})))

(defn won?
  "Has the user just won the game?"
  [chat-key]
  (= (last (get-gameprop chat-key :guesses)) (get-gameprop chat-key :answer)))

(defn contains-char?
  "Does the string contain the char c?"
  [string c]
  (boolean (some #(= % c) string)))

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

(defn tosymsl-
  ([guess answer] (tosymsl- guess answer (mask-answer guess answer)))
  ([guess answer masked-ans]
   (if (empty? guess)
     '()
     (let [ges_let (first guess)
           ans_let (first answer)
           ans_rest (clojure.string/join (rest guess))
           ges_rest (clojure.string/join (rest answer))]
       (cond (= ges_let ans_let) (conj (tosymsl- ans_rest ges_rest masked-ans)
                                       :revealed)
             (contains-char? masked-ans ges_let)
               (conj
                 (tosymsl- ans_rest ges_rest (mask-first masked-ans ges_let))
                 :semiknown)
             :else (conj (tosymsl- ans_rest ges_rest masked-ans) :wrong))))))

(defn tosyms
  "Compare given guess to answer. For each letter:
    If letter is correct and in right position - :revealed
    If letter is in the answer, but in the wrong position - :semiknown
    If letter is NOT in the answer - :wrong
    A vec of these keywords is returned for each letter in the supplied guess."
  [guess answer]
  (vec (tosymsl- guess answer)))

(defn no-progress?
  "If the last 3 guesses have not resulted in finding new information
   (other than excluded letters) - returns true"
  [chat-key]
  (apply =
    (mapv #(tosyms % (get-gameprop chat-key :answer))
      (take-last 3 (get-gameprop chat-key :guesses)))))

;;TODO make our own string util pkg....
(defn chop
  [s piece-count]
  (let [step (/ (count s) piece-count)]
    (->> (range (inc piece-count)) ;; <-- Enumerated split positions
         (map #(-> %
                   (* step)
                   double
                   Math/round)) ;; <-- Where to split
         (partition 2 1) ;; <-- Form slice lower/upper bounds
         (map (fn [[l u]] (subs s l u)))))) ;; <-- Slice input string

(defn add-guess!
  "Append the given guess to the state map.
   This will also update the lists of seen letters"
  [chat-key guess user]
  (let [syms (tosyms guess (get-gameprop chat-key :answer))
        le-syms (map vector syms guess)]
    (mapv #(do (cond (= :wrong (first %)) (add-exc-letter! chat-key (second %))
                     (= :revealed (first %)) (add-inc-letter! chat-key
                                                              (second %))))
      le-syms)
    (add-to-col! chat-key :guesses guess)
    (if (not (= (get-gameprop chat-key :user) user))
      ;; multiple users have played this game
      (set-gameprop chat-key :type :group))))

(defn letter-help
  "Provides a visual aid in the form of the remaining unguessed letters"
  [chat-key]
  (let [less-excluded (set/difference A-Z
                                      (get-gameprop chat-key :excluded-letters))
        included (get-gameprop chat-key :included-letters)
        letter-aid (set/difference less-excluded included)]
    (str (clojure.string/join
           "\n"
           (chop (clojure.string/join " " (mapv str letter-aid)) 2))
         " \n [ "
         (clojure.string/join " " included)
         " ]")))


(deftest test-tosyms
  (is (= [:wrong :semiknown :wrong :semiknown :revealed]
         (tosyms "FEELS" "PALES")))
  (is (= [:wrong :semiknown :wrong :revealed :revealed]
         (tosyms "FLEES" "PALES")))
  (is (= [:wrong :revealed :revealed :revealed :wrong]
         (tosyms "GLEES" "FLEET")))
  (is (= [:semiknown :semiknown :revealed :wrong :wrong]
         (tosyms "EELSY" "AGLEE")))
  (is (= [:semiknown :semiknown :wrong :wrong :wrong]
         (tosyms "EEESY" "AGLEE"))))

(defn draw-letter
  "Draw the given letter on the board at position x,y.
    The style is :revealed, :wrong or :semiknown."
  [c x y style]
  (cond (= :revealed style) (q/fill 83 140 78)
        (= :wrong style) (q/fill 58 58 60)
        (= :semiknown style) (q/fill 180 158 59))
  ;(q/fill "#b49e3b")
  (q/stroke 17 17 18)
  (q/rect x y (- letter-size letter-border) (- letter-size letter-border))
  (q/fill 248 248 248)
  (q/text-align :center)
  (q/text-font (q/create-font "Source Code Pro"
                              (- letter-size (* 2 letter-border))))
  ;(q/text-size 40)
  (q/text (str c)
          (+ (/ (- letter-size letter-border) 2) x)
          (+ (- letter-size (* 2 letter-border)) y)))

(defn draw-unrevealed
  "Draw an unrevealed letterbox at x,y."
  [x y]
  (q/fill 26 26 28)
  (q/stroke 168 168 168)
  (q/rect x y (- letter-size letter-border) (- letter-size letter-border)))

(defn draw-board
  "Draws the board letter by letter according to the chat's guesses array"
  [gr chat-key]
  (let [guesses (get-gameprop chat-key :guesses)
        answer (get-gameprop chat-key :answer)]
    (q/with-graphics
      gr
      (q/background 17 17 18)
      (doseq [i (range (count guesses))
              j (range (count (get guesses i)))]
        (let [guess (get guesses i)
              letter (nth (seq guess) j)
              sym (get (tosyms guess answer) j)
              y (+ letter-border (* letter-size i))
              x (+ letter-border (* letter-size j))]
          (draw-letter letter x y sym)))
      (doseq [y (range (+ letter-border (* letter-size (count guesses)))
                       (+ letter-border (* letter-size max-guesses))
                       letter-size)
              x (range letter-border
                       (+ (* (get-gameprop chat-key :size) letter-size)
                          (* 2 letter-border))
                       letter-size)]
        (draw-unrevealed x y)))))

(defn get-no-cols
  "Calculate the number of columns to draw.
  This is the number of guesses +1, or 6, whichever is smallest"
  [chat-key]
  (let [guesses (guesses-made chat-key)]
    (if (> guesses 5)
      6
      (+ 1 guesses))))

(defn draw
  "Initites all the drawing and puts the image into /tmp"
  [chat-key]
  (let [gr (q/create-graphics (+ (* (get-gameprop chat-key :size) letter-size)
                                 letter-border)
                              (+ (* (get-no-cols chat-key) letter-size) letter-border)
                              :java2d)]
    (q/with-graphics gr
                     (draw-board gr chat-key)
                     (q/save (format "/tmp/wordle.%s.png"
                                     (str (symbol chat-key))))
                     (.dispose gr)
                     (q/exit))))

(defn get-img
  "Setup sketch, call drawing fn, get the png, return File object"
  [chat-key]
  (q/defsketch org.goat.module.Wordle
               :host "host"
               :setup (partial draw chat-key))
  ;;seems we need to give some time for sync to disk to happen or else we
  ;;get errors
  (Thread/sleep 200)
  (io/file (format "/tmp/wordle.%s.png" (str (symbol chat-key)))))

(defn get-size
  "If size is present, set it, otherwise just return 5."
  [s]
  (let [number (re-find #"\d+" s)]
    (if (nil? number) 5 (edn/read-string number))))

(defn get-difficulty
  "If hard difficulty is present, set it, otherwise just return :normal"
  [s]

  (let [elspeth (re-find #"elspeth" s)
        difficulty (re-find #"hard" s)]
    (if (nil? elspeth)
      (if (nil? difficulty) :easy :hard)
      :veasy)))

(defn audit-game
  "Get all related game data and audit it into DB"
  [chat-key]
  (let [m (get-in @state [:game-states chat-key])
        endtime (System/currentTimeMillis)]
    (users/record-wordle-game chat-key
                              (conj m
                                    [:endtime endtime] [:won (won? chat-key)]))))

(defn clear-game!
  "For a given chatid, remove all the game state entirely"
  [chat-key]
  (audit-game chat-key)
  (swap! state assoc-in
         [:game-states]
         (dissoc (get-in @state [:game-states]) chat-key)))

(defn get-user
  "Get the sender of the message. If the message contains 'elspeth' then
  set user to elspeth. If the game in progress is an elspeth game, then
  it always continues as an elspeth game."
  [m chat-key]
  (let [has-elspeth (clojure.string/includes? (clojure.string/lower-case (.getText m)) "elspeth")
        sender (.getSender m)]
    (if (playing? chat-key)
      (if (= "Elspeth" (get-gameprop chat-key :user))
        "Elspeth"
        sender)
      (if has-elspeth
        "Elspeth"
        sender))))

(defn -processChannelMessage
  [_ m]
  (let [chat-key (keyword (str (.getChatId m)))
        guess (clojure.string/upper-case (.getText m))
        command (clojure.string/lower-case (.getModCommand m))
        trailing (clojure.string/lower-case (.getText m))
        user (get-user m chat-key)]
    (if (= "wordle" command)
      (if (not (playing? chat-key))
        (let [size (get-size trailing)
              difficulty (get-difficulty trailing)
              worddata (words/get-word difficulty size)
              word (get worddata :word)
              definition (get worddata :definition)
              hits (get worddata :hits)]
          (if (or (> size 10) (< size 2))
            (.reply
              m
              "Don't be an eejit. I won't do more than 10 or less than 2.")
            (do
              (new-game! chat-key word size definition hits user difficulty)
              (if (= "Elspeth" user)
                (.reply m "I hope you enjoy the game Elspeth!"))
                (if (= difficulty :hard)
                  (.reply m (str "Ohh, feeling cocky, are we, " user "?"))
                  (.replyWithImage m (get-img chat-key))))))
        (.reply m "We're already playing a game, smart one."))
      (if (playing? chat-key)
        (do
          (println "guess:" guess ":answer:" (get-gameprop chat-key :answer))
          (if (= (get-gameprop chat-key :size)
                 (count (re-matches #"[a-zA-Z]*" guess)))
            (if (words/real-word? (clojure.string/upper-case guess))
              (do
                (add-guess! chat-key guess user)
                (.replyWithImage m (get-img chat-key))
                (if (and (= (guesses-made chat-key) (- max-guesses 1))
                         (not (won? chat-key)))
                  (.reply m "Uh oh!"))
                (if (and (> (guesses-made chat-key) 3)
                         (< (guesses-made chat-key) max-guesses)
                         (no-progress? chat-key)
                         (not (won? chat-key)))
                  (.reply
                    m
                    "You seem to be digging yourself into a hole! Do better."))
                (if (and (> (guesses-made chat-key) 2)
                         (not (won? chat-key))
                         (< (guesses-made chat-key) max-guesses))
                  (.reply m (letter-help chat-key)))
                ;; TODO store win/lose stats, streaks etc..
                (if (won? chat-key)
                  (do
                    (cond
                      (= max-guesses (guesses-made chat-key))
                        (.reply m "Phew! You won - barely.")
                      (= 5 (guesses-made chat-key))
                        (.reply
                          m
                          "You won, but I'm sure you feel a little bit dissapointed.")
                      (= 4 (guesses-made chat-key))
                        (.reply m
                                "Well done! You won, and you won competently.")
                      (= 3 (guesses-made chat-key))
                        (.reply m "WOW!! An excellent performance!! You won!")
                      (= 2 (guesses-made chat-key))
                        (.reply
                          m
                          "Gasp! How could you possibly win like that? Are you gifted?")
                      (= 1 (guesses-made chat-key))
                        (.reply
                          m
                          "You won. I prostrate myself before you, for clearly I am in the presence of a wordle deity. Let this day live forever in our memories."))
                    (.reply m
                            (str "Definition: "
                                 (get-gameprop chat-key :answerdef)))
                    (clear-game! chat-key))
                  (if (= max-guesses (guesses-made chat-key))
                    (do (.reply
                          m
                          (str "Oh no! You lost the game! \n The answer was: "
                               (get-gameprop chat-key :answer)))
                        (.reply m
                                (str "The too-difficult-for-you word means: "
                                     (get-gameprop chat-key :answerdef)))
                        (clear-game! chat-key))))))))))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '("wordle")))

(defn -messageType [_] 0)
