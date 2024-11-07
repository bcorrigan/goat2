(ns org.goat.module.Wordle
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:require [quil.core :as q :include-macros true]
            [org.goat.db.words :as words]
            [org.goat.db.users :as users]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.string :as str])
  (:import  [java.awt.image BufferedImage]
            [java.awt Color Font RenderingHints]
            [org.goat.core Module]
            [org.goat.core BotStats]))

(use 'clojure.test)

(def max-guesses 6)
(def letter-size 60)
(def letter-border 10)
(def A-Z (set (map char (concat (range 65 91)))))
(def bot (BotStats/getInstance))

(def state
  "key is :game-states->chatid - returning
  {:guesses [\"WRONG HOUSE\"] :answer \"RIGHT\"
  :size 5 :answerdef \"definition\" :hits 1000 }"
  (atom {}))

(def img-state
  "A temporary image cache - quil runs in a different thread to the main thread, this is essentially a simple means to communicate an image from the quil applet thread back to the main thread, which waits for this to be populated."
  (atom {}))

(defn get-img
  "Get stats image for a given chat"
  [chat-key img-key]
  (get-in @img-state [img-key chat-key :img]))

(defn set-img
  "Set stats image for a given chat"
  [chat-key img-key img]
  (swap! img-state assoc-in [img-key chat-key :img] img))

(defn remove-img!
  "Remove image associated with given chat"
  [chat-key img-key]
  (swap! img-state assoc-in
         [img-key]
         (dissoc (get-in @img-state [img-key]) chat-key)))

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

(defn get-game
  "Get all the state for given game"
  [chat-key]
  (get-in @state [:game-states chat-key]))

(defn get-fgameprop
  "Get property from given property in cached first game in challenge."
  [challenge-key property]
  (get-in @state [:game-states challenge-key :first-game property]))

(defn set-gameprop
  "Set given value for given chatid game"
  [chat-key property value]
  (swap! state assoc-in [:game-states chat-key property] value))

(defn playing?
  "true if we're playing a game on the given chatid and it isn't in process of concluding"
  [chat-key]
  (not (nil? (get-in @state [:game-states chat-key]))))

(defn new-game!
  "Add state for a new game of wordle."
  [chat-key answer size answerdef hits user difficulty challenge-key]
  (let [starttime (System/currentTimeMillis)
        challenge (not (nil? challenge-key))]
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
            :challenge challenge,
            :challenge-key challenge-key,
            :drawing (atom false)
            :hits hits})))

(defn new-challenge!
  "Add state for a new challenge"
  [challenge-key chat-key1 chat-key2 group-chat-key user1 user2 m]
  (swap! state assoc-in
         [:game-states challenge-key]
         {:chat-key1 chat-key1,
          :chat-key2 chat-key2,
          :group-chat-key group-chat-key,
          :user1 user1,
          :user2 user2,
          :playing (atom 2),
          :m m
          ;; :first-game (all the details of the other game)
          }))

(defn other-user
  "Get the other user in a challenge."
  [chat-key]
  (let [challenge-key (get-gameprop chat-key :challenge-key)
        user (get-gameprop chat-key :user)
        user1 (get-gameprop challenge-key :user1)]
    (println (str "user:" user "user1:" user1))
    (if (= user user1)
      (get-gameprop challenge-key :user2)
      user1)))

(defn other-chat-key
  "Get the other chat-key in a challenge."
  [chat-key]
  (let [challenge-key (get-gameprop chat-key :challenge-key)
        chat-key1 (get-gameprop challenge-key :chat-key1)]
    (if (= chat-key chat-key1)
      (get-gameprop challenge-key :chat-key2)
      chat-key1)))


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
           ans_rest (clojure.string/join (rest guess))
           ges_rest (clojure.string/join (rest answer))]
       (cond (= ges_let ans_let) (conj (classify-letters ans_rest ges_rest masked-ans)
                                       :revealed)
             (contains-char? masked-ans ges_let)
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

(defn no-progress?
  "If the last 3 guesses have not resulted in finding new information
   (other than excluded letters) - returns true"
  [chat-key]
  (apply =
         (mapv #(compare-guess-to-answer % (get-gameprop chat-key :answer))
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

(defn pgraphics-to-bufferedimage
  [pg]
  (let [w   (.width pg)
        h   (.height pg)
        bi  (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g2d (.createGraphics bi)]
    (.drawImage g2d (.getImage pg) 0 0 nil)
    (.dispose g2d)
    bi))

(defn add-guess!
  "Append the given guess to the state map.
   This will also update the lists of seen letters"
  [chat-key guess user]
  (let [syms (compare-guess-to-answer guess (get-gameprop chat-key :answer))
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
              sym (get (compare-guess-to-answer guess answer) j)
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

(defn board-width
  "get the width of the board for the given chat-key"
  [chat-key]
  (+ (* (get-gameprop chat-key :size) letter-size)
     letter-border))

(defn board-height
  "get the width of the board for the given chat-key"
  [chat-key]
  (+ (* (get-no-cols chat-key) letter-size) letter-border))

(def stats-width "Width of the statistics graphic" 800)
(def stats-height "height of the statistics graphic"1200)

(defn draw
  "Initites all the drawing and puts the image into /tmp"
  [chat-key]
  (println (str "height:" (board-height chat-key)))
  (let [gr (q/create-graphics (board-width chat-key)
                              (board-height chat-key)
                              :p2d)]
    (q/with-graphics gr
      (draw-board gr chat-key)
      (set-gameprop chat-key :img (pgraphics-to-bufferedimage (q/current-graphics)))
      (.dispose gr)
      (q/exit))))

(defn get-wonbox-col
  "Get the colour of the box needed for stats image"
  [won guesses]
  (if won
    (cond (= 6 guesses)
          '(168 64 29)    ;; dark orange
          (= 5 guesses)
          '(235 149 10)    ;; orange
          (= 4 guesses)
          '(235 227 0)    ;;yellow
          (= 3 guesses)
          '(150 203 0)    ;; green-yellow
          (= 2 guesses)
          '(0 255 0)      ;; green
          (= 1 guesses)
          '(255 255 255)) ;;white
    '(60 0 0) ;; red
    ))

(def small-font-pt 30)
(def big-font-pt 50)
(def stats-square-px 40)
(def text-height-px 70)
(def text-indent (* 6 stats-square-px))
(defn text-row-px
  "Calculate offset for given row"
  [row]
  (+ 10 (* text-height-px row)))

(defn draw-stats-gr
  "Draw onto the given graphics our stats"
  [gr chat-key user]
  (let [stats (users/get-stats user)
        games (get stats :results-150)]
    (q/with-graphics gr
      (q/background 17 17 18) ;;black
      (doseq [i (range (count games))]
        (let [col (mod i 5)
              row (int (math/floor (/ i 5)))
              game (nth games i)
              won (get game :won)
              guesses (get game :guesses)
              boxcol (get-wonbox-col won guesses)]
          (q/fill (first boxcol) (second boxcol) (nth boxcol 2))
          (q/stroke (first boxcol) (second boxcol) (nth boxcol 2))
          (q/rect (* col stats-square-px)
                  (* row stats-square-px)
                  (- stats-square-px 4)
                  (- stats-square-px 4))))
      ;; Text
      (let [games-played (+ (or (get stats :games-won) 0) (or (get stats :games-lost) 0))
            games-played-150 (+ (or (get stats :games-won-150) 0) (or (get stats :games-lost-150) 0))
            win-ratio (* 100 (double (/ (or (get stats :games-won-150) 0) games-played-150)))
            win-ratio-fmt (format "%.3f" win-ratio)
            guess-rate-150 (format "%.3f" (or (get stats :guess-rate-150) 0))
            guess-rate-20 (format "%.3f" (or (get stats :guess-rate-20) 0))
            games-played-20 (+ (or (get stats :games-won-20) 0) (or (get stats :games-lost-20) 0))
            win-ratio-20 (* 100 (double (/ (or (get stats :games-won-20) 0) games-played-20)))
            win-ratio-20-fmt (format "%.3f" win-ratio-20)
            big-font  (q/create-font "Noto Sans" big-font-pt)
            small-font (q/create-font "Noto Sans" small-font-pt)]
        (q/text-font big-font)
        (q/fill 248 248 248) ;; white
        (q/text (str user "'s records:") text-indent (text-row-px 1))
        (q/text (str "Games played: " games-played) text-indent (text-row-px 2))
        (q/text (str "Streak: " (users/get-streak user)) text-indent (text-row-px 3))
        (q/text-font small-font)
        (q/fill 148 148 148) ;; grey
        (q/text (str "        ---LAST 150---") text-indent (text-row-px 4))
        (q/fill 248 248 248) ;; white
        (q/text-font big-font)
        (q/text (str "Win ratio: " win-ratio-fmt "%") text-indent (text-row-px 5))
        (q/text (str "Guess rate: " guess-rate-150) text-indent (text-row-px 6))
        (q/fill 148 148 148) ;; grey
        (q/text-font small-font)
        (q/text (str "        ---LAST 20---") text-indent (text-row-px 7))
        (q/fill 248 248 248) ;; white
        (q/text-font big-font)
        (if (>= win-ratio-20 win-ratio)
          (q/fill 123 177 114)  ;; green
          (q/fill 255 167 255)) ;; pink
        (q/text (str "Win ratio: " win-ratio-20-fmt "%") text-indent (text-row-px 8))
        (if (<= (or (get stats :guess-rate-20) 0) (or (get stats :guess-rate-150) 0))
          (q/fill 123 177 114)  ;; green
          (q/fill 255 167 255)) ;; pink
        (q/text (str "Guess rate: " guess-rate-20) text-indent (text-row-px 9))
        (q/fill 148 148 148) ;; grey
        (q/text-font small-font)
        (q/text (str "        ---PERSONAL BESTS---") text-indent (text-row-px 10))
        (q/fill 248 248 248) ;; white
        (q/text-font big-font)
        (q/text (str "Streak: " (or (users/get-record user :streak) 0)) text-indent (text-row-px 11))
        (q/text (str "/150 Win ratio: " (format "%.3f" (* 100 (or (users/get-record user :won-rate-150) 0.0)))) text-indent (text-row-px 12))
        (q/text (str "/20 Guess ratio: " (format "%.3f" (or (users/get-record user :guess-rate-20) 0.0))) text-indent (text-row-px 13))))))

(defn draw-pie-chart-gr
  "Draw a pie chart in wordle coloring representing head2head combat stats.
  p1 and p2 are the names of p1 and p2, other params are number of wins and draws."
  [gr player1-wins player2-wins draws p1 p2]
  (q/with-graphics gr
    (let [width            600
          height           600
          center-x         (/ width 2)
          center-y         (- (/ height 2) 80)
          radius           400
          total            (+ player1-wins player2-wins draws)
          to-radians       (fn [degrees] (/ (* degrees Math/PI) 180))
          p1-color         [83 140 78]
          p2-color         [180 158 59]
          draw-color       [58 58 60]
          draw-slice       (fn [start-angle end-angle color]
                       (q/fill color)
                       (q/arc center-x center-y radius radius
                              (to-radians start-angle)
                              (to-radians end-angle)
                              :pie))
          draw-legend-item (fn [text color x y]
                             (q/fill color)
                             (q/rect x y 60 30)
                             (q/fill 248 248 248)
                             (q/text-align :left :center)
                             (q/text text (+ x 70) (+ y 15) ))]

                                        ; Set up the drawing
      (q/background 26 26 28)
      (q/smooth)
      (q/no-stroke)

                                        ; Draw the slices
      (draw-slice 0
                  (* 360 (/ player1-wins total))
                  p1-color)  ; Red for player 1
      (draw-slice (* 360 (/ player1-wins total))
                  (* 360 (/ (+ player1-wins player2-wins) total))
                  p2-color)  ; Blue for player 2
      (draw-slice (* 360 (/ (+ player1-wins player2-wins) total))
                  360
                  draw-color)  ; Green for draws

                                        ; Add labels
      (q/text-size 20)
      (draw-legend-item (str p1 ": " player1-wins) p1-color (- center-x 280) 450)
      (draw-legend-item (str p2 ": " player2-wins) p2-color (- center-x 280) 500)
      (draw-legend-item (str "Draws: " draws ) draw-color (- center-x 280) 550))))

(defn draw-stats
  "Draw the *stats* window"
  [user chat-key]
  (let [gr (q/create-graphics 800 1200 :p2d)]
    (q/with-graphics gr
      (draw-stats-gr gr chat-key user)
      (set-img chat-key :stats-img  (pgraphics-to-bufferedimage (q/current-graphics)))
      (q/exit))))

(defn draw-pie-chart
  "Draw the pie chart representing who's best"
  [player1-wins player2-wins draws p1 p2 chat-key]
  (let [gr (q/create-graphics 600 600 :p2d)]
    (q/with-graphics gr
      (draw-pie-chart-gr gr player1-wins player2-wins draws p1 p2)
      ;; need some get-pie-img - can this not be abstracted?
      (set-img chat-key :pie-img (pgraphics-to-bufferedimage (q/current-graphics)))
      (q/exit))))

(defn get-drawn-img
  "Setup sketch for given underlying game draw fn"
  [chat-key drawfn width height]
  (set-gameprop chat-key :img nil)
  (q/defsketch org.goat.module.Wordle
    :host "host"
    :renderer :p2d
    :size [width height]
    :setup (partial drawfn chat-key))
  ;; defsketch spawns an applet under the hood which has its own thread
  ;; so we wait till it completes by checking if img is ready
  (while (nil? (get-gameprop chat-key :img))
    (Thread/sleep 50))
  (get-gameprop chat-key :img))

(defn get-quil-img
  "Setup sketch for given underlying draw fn"
  [chat-key drawfn width height img-key]
  (remove-img! chat-key img-key)
  (q/defsketch org.goat.module.Wordle
    :host "host"
    :renderer :p2d
    :size [width height]
    :setup (partial drawfn chat-key))

  (while (nil? (get-img chat-key img-key))
    (Thread/sleep 50))
  (get-img chat-key img-key))


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

(defn get-challenge
  "Parse out who is being challenged."
  [s]
  (if (str/includes? s "challenge")
    (let [mr (re-matcher  #"\W*\w+\W+(\w+)\W*" s)]
      (.matches mr)
      (if (.hasMatch  mr)
        (.group mr 1)))))

(defn audit-game
  "Get all related game data and audit it into DB.
    Returns any new PBs"
  [chat-key]
  (let [m (get-in @state [:game-states chat-key])
        endtime (System/currentTimeMillis)]
    (users/record-wordle-game chat-key
                              (conj m
                                    [:endtime endtime] [:won (won? chat-key)]))))

(defn append-images
  "Append two images side by side as one new image."
  [img1 img2 player1-name player2-name]
  (let [width1          (.getWidth img1)
        width2          (.getWidth img2)
        height1         (.getHeight img1)
        height2         (.getHeight img2)
        text-height     30 ;; height of the text area
        text-color      (Color. 248 248 248)
        combined-width  (+ width1 width2)
        combined-height (+ (max height1 height2) text-height)
        bgcolor         (Color. 26 26 28)
        combined-img    (BufferedImage. combined-width combined-height BufferedImage/TYPE_INT_ARGB)
        g2d             (.createGraphics combined-img)]
    ;; Set rendering hints for smoother text
    (.setRenderingHint g2d RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    ;;fill background
    (.setColor g2d bgcolor)
    (.fillRect g2d 0 0 combined-width combined-height)
    ;;draw player names
    (.setColor g2d text-color)
    (.setFont g2d (Font. "Source Code Pro" Font/BOLD 20))
    (.drawString g2d player1-name 10 25)
    (.drawString g2d player2-name (+ width1 10) 25)
    ;;append images
    (.drawImage g2d img1 0 text-height nil)
    (.drawImage g2d img2 width1 text-height nil)
    (.dispose g2d)
    combined-img))

(defn reply-both
  "Reply to two chats"
  [m1 m2 msg]
  (.reply m1 msg)
  (.reply m2 msg))

(defn clear-game!
  "For a given chatid, remove all the game state entirely.
  Returns any PBs that have been set in the concluded game.
  Audits the game in the stats table, records new records, and
  if a challenge game we conclude the challege and record challenge stats."
  [chat-key m]

  (if (get-gameprop chat-key :challenge)
    (let [challenge-key (get-gameprop chat-key :challenge-key)
          other-user    (other-user chat-key)
          this-user     (get-gameprop chat-key :user)
          other-chatid  (users/user-chat other-user)
          other-msg     (new org.goat.core.Message other-chatid "" true "goat")
          playing       (get-gameprop challenge-key :playing)]
      ;; If both players press enter simultaneously, they could end up entering this code at the same moment?
      (locking playing
        (if (= @playing 2)
          (do
            (.reply m (str "I'm afraid that " other-user " is not as speedy as you, but I will keep you posted in the main chat."))
            (.reply other-msg (str this-user " just finished, " other-user ", better get your skates on!"))
            (set-gameprop challenge-key :first-game (get-game chat-key))
            (reset! playing 1))
          (if (= @playing 1)
            ;;wrap up the challenge now
            (let [first-img      (get-fgameprop challenge-key :img)
                  second-img     (get-gameprop chat-key :img)
                  p1             (get-fgameprop challenge-key :user)
                  p2             (get-gameprop chat-key :user)
                  combined-image (append-images second-img first-img p2 p1)
                  reply          (partial reply-both m other-msg)]
              (reset! playing 0)
              (reply "The challenge has concluded!")
              (.replyWithImage m combined-image)
              (.replyWithImage other-msg combined-image)
              (let [p1-guesses (count (get-fgameprop challenge-key :guesses))
                    p2-guesses (count (get-gameprop chat-key :guesses))
                    p1-won     (get-fgameprop challenge-key :won)
                    p2-won     (get-gameprop chat-key :won)]
                (cond
                  (and p1-won p2-won
                       (= p1-guesses p2-guesses))
                  (reply "The scorekeeper hereby declares this match a score draw.")
                  (and p1-won (not p2-won))
                  (reply (str "Well done " p1 ", you won! Commiserations " p2 "."))
                  (and (not p1-won) p2-won)
                  (reply (str "Good job " p2 ", you were victorious! Hard luck to you, " p1 "."))
                  (and p1-won p2-won
                       (< p1-guesses p2-guesses))
                  (reply (str "A valiant attempt by " p2 ", but " p1 " was just too strong and got there faster. Well done " p1 "!"))
                  (and p1-won p2-won
                       (< p2-guesses p1-guesses))
                  (reply (str "Nae luck " p1 ". " p2 " was just that little bit better. Nice work, " p2 "!"))
                  (and (not p1-won) (not p2-won))
                  (reply "Wow, that must have been really hard, or, you are both really bad at wordle. You both lost!"))
                (users/audit-challenge-game p1 p2 p1-guesses p2-guesses p1-won p2-won))

              (swap! state assoc-in
                     [:game-states]
                     (dissoc (get-in @state [:game-states]) challenge-key))))))))
  (println (str "USER:" (get-gameprop chat-key :user)))
  (let [pbs (audit-game chat-key)]
    (swap! state assoc-in
           [:game-states]
           (dissoc (get-in @state [:game-states]) chat-key))
    pbs))

  (defn get-user
    "Get the sender of the message. If the message contains 'elspeth' then
  set user to elspeth. If the game in progress is an elspeth game, then
  it always continues as an elspeth game."
    [m chat-key]
    (let [has-elspeth (clojure.string/includes? (clojure.string/lower-case (.getText m)) "elspeth")
          sender      (.getSender m)]
      (if (playing? chat-key)
        (if (= "Elspeth" (get-gameprop chat-key :user))
          "Elspeth"
          sender)
        (if has-elspeth
          "Elspeth"
          sender))))

(defn combine-keys
  "Concatenate supplied chat-key symbols, to obtain a combined identifier."
  [chat-key1 chat-key2]
  (let [skeys (sort [chat-key1 chat-key2])
        key1 (first skeys)
        key2 (second skeys)]
  (symbol
   (str ":"
        (symbol (str key1))
        (symbol (str key2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RESPONSE STRINGS ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-streak-msg
  "Get a congratulatory (or critical) message depending on user's streak"
  [streak user]
  (cond (= 0 streak)
        (str "Your streak is a big fat 0 I'm afraid, " user ". Don't feel bad, everybody is good at something.")
        (>= streak 50)
        (str user ", you are the god of wordle with a streak of " streak)
        (>= streak 40)
        (str user ", you have an excellent streak of " streak ". Careful now!")
        (>= streak 20)
        (str "Well done " user " you have a pretty decent streak of " streak ".")
        (>= streak 10)
        (str "You have a respectable streak of " streak ", " user ". Keep up the good work.")
        (>= streak 5)
        (str "You have a streak of " streak ". If you keep being careful, maybe you can build on this.")
        (>= streak 1)
        (str "Baby steps, " user ", you have a streak of " streak ". Maybe this time you can go further.")))

(def invalid-guess-responses
  ["Are you just mashing the keyboard now?"
   "I don't think that's a word in the English language. Or indeed any language."
   "Your creativity is... interesting."
   "Ah, I see you've invented a new word. How avant-garde!"
   "That's a lovely keysmash you've got there."
   "I'm starting to think you're not taking this seriously."
   "Is that your cat walking on the keyboard?"
   "Fascinating. Tell me more about this 'word' of yours."
   "I'm not sure that's in the dictionary. Or any dictionary."
   "Your spelling is... unconventional."
   "Are you trying to summon an elder god with that word?"
   "I admire your commitment to nonsense."
   "That's certainly... a choice."
   "I think you might need a refresher on the alphabet."
   "Interesting strategy. Bold. Wrong, but bold."
   "Is that what happens when you sneeze while typing?"
   "I'm not sure if that's a word or a secret code."
   "Are you trying to break the game or just the English language?"
   "That's a very creative interpretation of 'word'."
   "I think your autocorrect just had a nervous breakdown."
   "Did you just headbutt the keyboard?"
   "Are you trying to communicate in alien?"
   "I'm beginning to think you're playing a different game entirely."
   "That's not a word. That's not even close to a word."
   "I've seen more coherent alphabet soup."
   "Are you entering passwords from your other accounts now?"
   "I don't think that word means what you think it means. In fact, I don't think it means anything at all."
   "Your spelling is so bad, it's almost good. Almost."
   "Did you just throw Scrabble tiles at the screen and type what stuck?"
   "I'm not saying it's gibberish, but... actually, yes, I am saying it's gibberish."
   "That's a lovely collection of letters you've assembled there. Shame it doesn't mean anything."
   "I think you've discovered a new form of encryption."
   "Are you trying to set a record for most consecutive invalid guesses?"
   "I'm starting to think you're doing this on purpose."
   "That's not a word in this dimension. Maybe try another one?"
   "Are you mildly dyslexic? Or just severely dyslexic."])

(def win-responses
  {1 ["You won on the first guess? Are you a mind reader or just insanely lucky?"
      "I bow before your wordsmithing prowess. That was simply incredible!"
      "Did you hack the game, or are you just that good? Phenomenal!"
      "I'm not saying you're a word wizard, but... actually, yes, I am saying exactly that."
      "One guess? ONE? I'm speechless. Utterly gobsmacked. Bravo!"
      "Are you sure you're not cheating? Because that was suspiciously perfect."
      "I think we just witnessed a Wordle miracle. Astounding!"
      "You've peaked. It's all downhill from here. What a guess!"
      "Although I can do 100 million calculations per second, I'm going to need a moment to process how ridiculously good that was."
      "That wasn't a guess. That was divine intervention. Simply otherworldly!"
      "I feel like I just witnessed the Wordle equivalent of a hole-in-one. Incredible!"
      "You've officially broken the game. We can all go home now."
      "I didn't think it was possible to win on the first try. I stand corrected."
      "Are you some kind of Wordle savant? That was unbelievable!"
      "I'm pretty sure that violates the laws of probability. Absolutely amazing!"
      "You've ascended to Wordle godhood. We're not worthy!"
      "That wasn't a guess. That was a work of art. Masterful!"
      "I'm going to tell my grandkids about this someday. Legendary!"
      "You've peaked. It's all downhill from here. What a guess!"
      "I'm not saying you're psychic, but... are you psychic?"]

   2 ["Two guesses? That's not just good, that's scary good!"
      "I'm impressed. Very impressed. You're a Wordle natural!"
      "Wow, you cracked it in two! You must be a word wizard!"
      "That was like watching a master at work. Brilliant job!"
      "You're making this look way too easy. Fantastic guessing!"
      "I'm in awe of your Wordle skills. That was exceptional!"
      "You must have a dictionary for a brain. Incredible work!"
      "That was a virtuoso performance. Take a bow!"
      "You're not just good at this, you're freakishly good. Well done!"
      "I think we have a Wordle prodigy on our hands. Bravo!"
      "Your word game is strong. Very, very strong. Impressive!"
      "That was like watching a chess grandmaster, but with words. Superb!"
      "You've got some serious Wordle mojo. Excellent job!"
      "I'm beginning to suspect you might be a linguistic genius. Great work!"
      "That was a masterclass in Wordle strategy. Well played!"
      "You're not human. You're some kind of Wordle-solving machine. Incredible!"
      "I'm running out of superlatives here. That was simply fantastic!"
      "You must have a sixth sense for words. Remarkable guessing!"
      "That wasn't just good, that was 'frame it and put it on the wall' good!"
      "I think you might have found your calling. Wordle champion extraordinaire!"]

   3 ["Three guesses? Now that's what I call efficiency!"
      "Well done! You solved it with style and speed."
      "Great job! You've got a real talent for this game."
      "Impressive work! You cracked it in no time."
      "Nicely done! Your Wordle skills are on point."
      "Excellent guessing! You made that look easy."
      "Bravo! You've got a knack for this, don't you?"
      "That was smooth! You've clearly got this game figured out."
      "Well played! Your word deduction skills are admirable."
      "Fantastic job! You're quite the Wordle whiz."
      "Stellar performance! You've got a good head for words."
      "Great solving! You've got the Wordle touch."
      "Impressive stuff! You're a natural at this game."
      "Nicely solved! Your Wordle game is strong."
      "Well executed! You've got this down to a science."
      "Excellent work! You're making this look too easy."
      "Kudos! Your word skills are seriously impressive."
      "Great guessing! You've got a real gift for this."
      "Well done! You're a Wordle force to be reckoned with."
      "Bravo! Your Wordle prowess is something to behold."]

   4 ["Four guesses? Not bad, not bad at all!"
      "Good job! You figured it out with room to spare."
      "Nice work! You're getting pretty good at this."
      "Well done! That was a solid performance."
      "Not too shabby! You've got a decent handle on this game."
      "Pretty good! You're definitely improving."
      "Nicely solved! You're above average, you know."
      "Good stuff! You're holding your own against the Wordle."
      "Well played! You're certainly no Wordle novice."
      "Not bad at all! You're getting the hang of this."
      "Decent job! You're showing some real potential."
      "Good going! You're making steady progress."
      "Nice one! You're developing a knack for this."
      "Solid work! You're definitely on the right track."
      "Well executed! You're becoming quite the Wordle solver."
      "Good show! You're certainly no slouch at this game."
      "Nice solving! You're definitely above the curve."
      "Well done! You're holding your own quite nicely."
      "Good job! You're showing some real Wordle acumen."
      "Not too bad! You're certainly no Wordle pushover."]

   5 ["Five guesses? Well, you got there in the end!"
      "Phew! That was a close one, but you made it."
      "You won, but I bet that last guess had you sweating."
      "Victory snatched from the jaws of defeat! Well, almost."
      "Not your best performance, but a win's a win, right?"
      "You like to keep it suspenseful, don't you?"
      "Living dangerously, I see. But you pulled it off!"
      "Cut it a bit fine there, didn't you? But you made it!"
      "I was getting worried for a moment, but you came through."
      "That was tense! But you got there in the end."
      "Not the most elegant win, but it counts!"
      "You had me on the edge of my seat, but you did it!"
      "A bit of a nail-biter, that one. But you prevailed!"
      "Squeaked by on that one, didn't you? Still, good job!"
      "That was like watching a high-wire act. Nerve-wracking but successful!"
      "You like to make things interesting, don't you? But you pulled it off!"
      "I thought you were done for, but you proved me wrong. Nice save!"
      "That was a close shave! But you managed to clinch it."
      "You certainly know how to build suspense! Good job... eventually."
      "Whew! That was a close one. But you made it across the finish line!"]

   6 ["Six guesses? Well, you won... technically."
      "Cutting it a bit fine there, weren't you? But hey, a win's a win."
      "Phew! You just scraped by on that one. Literally."
      "Well, you won. I guess. If you want to call that winning."
      "That was like watching a sloth solve a Rubik's cube. But you got there!"
      "I was about to send out a search party. But you made it... barely."
      "Were you trying to use up all six guesses on purpose? Because you succeeded."
      "That was less 'winning' and more 'not losing'. But I'll take it."
      "I've seen glaciers move faster. But you got there in the end!"
      "Did you enjoy your leisurely stroll to the answer?"
      "I was starting to wonder if you'd ever guess it. But you surprised me!"
      "That was a real nail-biter. And by nail-biter, I mean I chewed off all my nails waiting for you to get it."
      "You really like to keep me in suspense, don't you?"
      "I hope you're not planning a career as a professional wordle player."
      "Well, you used all the guesses. Every. Single. One. But you won ugly I suppose."
      "That was less of a sprint and more of a marathon. But you crossed the finish line even though you walked round the course, so well done?"
      "I was about to call it a day, but you finally got there!"
      "You sure know how to stretch out a game of Wordle to its absolute limit."
      "I'm not saying you're slow, but... actually, yes, that's exactly what I'm saying."
      "Congratulations on your last-second, by-the-skin-of-your-teeth victory!"]})

(def lose-responses
  ["I'm not saying you're bad at Wordle, but dictionary publishers just breathed a sigh of relief."
   "Have you considered taking up a different hobby? Perhaps competitive paint drying?"
   "Your Wordle skills are so bad, even autocorrect is facepalming right now."
   "I've seen better guesses from a random word generator. And it was broken."
   "You might want to consider a career that doesn't involve... well, words. Or thinking."
   "That was so bad, I think you actually made the English language cry."
   "Congratulations! You've just set a new record for 'Most Creative Way to Lose at Wordle'."
   "I'm starting to think you're playing 'How NOT to guess the word'."
   "Your performance was so legendary, it'll be used in Wordle schools as a cautionary tale."
   "I've seen more coherent letter combinations in alphabet soup."
   "You do realize the goal is to guess the word, not avoid it at all costs, right?"
   "I think you've discovered a new game called anti-Wordle. Impressive, in a way."
   "Your Wordle skills are like your winning streak - non-existent."
   "I'm not saying you should give up, but... maybe consider donating your keyboard to science?"
   "That was so bad, I think you owe the dictionary an apology."
   "I've seen better word skills from a cat walking across a keyboard."
   "Your Wordle game is so weak, it just applied for life support."
   "Congratulations on turning a simple word game into an epic tragedy."
   "I think you just redefined 'rock bottom' for Wordle players everywhere."
   "Your Wordle skills are like a unicorn: mythical and non-existent."])

(def lose-reveal-responses
  ["Well, that was... something. The word you spectacularly failed to guess was: %s"
   "Ouch. I felt that from here. The word that defeated you was: %s"
   "And the award for 'Most Creative Way to Lose at Wordle' goes to you! The actual word was: %s"
   "I'm running out of ways to say 'you lost'. Oh wait, here's one: YOU LOST. The word was: %s"
   "Congratulations on your consistent ability to avoid the correct answer! It was: %s"
   "In a stunning turn of events, you've managed to lose again. The elusive word was: %s"
   "Well, you tried. Not very well, but you tried. The correct word was: %s"
   "I'm not mad, I'm just disappointed. The word you were looking for was: %s"
   "And the crowd goes... silent. Better luck next time! The word was: %s"
   "That was... an attempt. An unsuccessful one, but an attempt. The word was: %s"
   "I'd slow clap, but I don't want to hurt your feelings. The correct answer was: %s"
   "Well, that happened. The word that just happened to you was: %s"
   "I'm sensing a pattern here, and it's not a winning one. The word was: %s"
   "On the bright side, you've eliminated one more word you don't know. It was: %s"
   "Plot twist: You lost. Again. The protagonist of this tragedy was: %s"
   "Impressive. Most impressive. Oh wait, I meant depressive. The word was: %s"
   "Your Wordle journey ends here, not with a bang but a whimper. The word was: %s"
   "In the game of Wordle, you either win or... well, this. The correct word was: %s"
   "I'd say 'better luck next time', but let's be real. Anyway, the word was: %s"
   "Behold, the word that bested our brave but bewildered player: %s"])

(def definition-reveal-responses
  ["For those whose vocabulary is as limited as their Wordle skills, %s means: %s"
   "Allow me to expand your clearly insufficient lexicon. %s is defined as: %s"
   "Brace yourself for some learning, oh Wordle-challenged one. %s signifies: %s"
   "Let's enlighten that word-deprived brain of yours. %s is a fancy term for: %s"
   "Here's a nugget of wisdom for your word-starved mind. %s translates to: %s"
   "Prepare to be educated, oh vocabularily challenged one. %s is synonymous with: %s"
   "For the benefit of your clearly limited word bank, %s can be understood as: %s"
   "Let me introduce you to a word that's clearly not in your repertoire. %s means: %s"
   "Time for a vocabulary lesson, since you clearly need it. %s is defined as: %s"
   "Expanding your word knowledge one crushing defeat at a time. %s signifies: %s"
   "Here's some enlightenment for your word-impoverished existence. %s stands for: %s"
   "Behold, a word beyond your grasp! %s is a sophisticated way of saying: %s"
   "Let's add to your clearly deficient word pool. %s is another way to express: %s"
   "For future reference (and fewer embarrassing losses), %s is defined as: %s"
   "Here's a word to add to your apparently minuscule vocabulary. %s means: %s"
   "Prepare to learn, oh lexically challenged one. %s is a term used for: %s"
   "Let's bridge the vast gap in your word knowledge. %s is synonymous with: %s"
   "Expanding your horizons one word at a time. Today's lesson: %s means %s"
   "Here's a word that was clearly out of your league. %s is defined as: %s"
   "Time to learn a word that's evidently not in your limited arsenal. %s signifies: %s"])

(def no-progress-responses
  ["You're about as close to solving this as a penguin is to flying. Time to switch tactics!"
   "Your strategy is so effective, you might solve this by the heat death of the universe. Try something new!"
   "I've seen glaciers move faster than your progress. Maybe try a different approach?"
   "You're circling the answer like a shark, if the shark were blind and had no sense of direction. Mix it up!"
   "Your guesses are so off, they're in a different postal code. Time for a new game plan!"
   "You're not just digging yourself into a hole, you're tunneling to Australia. Change course!"
   "If your goal is to use all possible wrong letters, you're succeeding! Otherwise, maybe rethink this?"
   "Your Wordle skills are evolving! Backwards. Time to shuffle that strategy deck!"
   "You're so far off track, you might discover a new word. But that's not the goal here. New approach time!"
   "I've seen more progress in a traffic jam. Time to take a detour from your current strategy!"
   "Your guesses are so wild, they should be in a zoo. How about trying something a bit more... logical?"
   "You're repeating yourself more than a broken record. Scratch that disc and try a new tune!"
   "If persistence alone won Wordle, you'd be champion. Sadly, it doesn't. Time for Plan B... or is it Plan Z by now?"
   "Your strategy is about as effective as a chocolate teapot. Time to brew up some new ideas!"
   "You're stuck in a loop tighter than a pretzel. Time to unknot your thinking!"
   "Your progress is so non-existent, it's practically theoretical. Let's make it practical, shall we?"
   "You're playing Wordle like it's a game of 'How many ways can I avoid the right answer?' New tactic, perhaps?"
   "If your goal was to eliminate all wrong answers, you're crushing it! If not, maybe try a new approach?"
   "Your guesses are so consistently off, I'm starting to think it's a talent. A useless talent, but still. Mix it up!"
   "You're circling the drain faster than my hopes for your victory. Time to pull the plug on this strategy!"])

(defn last-chance-message []
  (rand-nth ["Uh oh! Last chance."
             "Final guess! Careful!"
             "Last shot! Think carefully."
             "One more try!"
             "Make it count!"
             "No pressure, but..."
             "Final attempt! Focus!"
             "Last guess! Good luck!"
             "This is it!"
             "Now or never!"
             "Moment of truth!"
             "Final opportunity! Concentrate!"
             "Last guess incoming!"
             "Crunch time!"
             "Do or die!"
             "Final round! Ready?"
             "Last chance saloon!"
             "Make it or break it!"
             "The final countdown!"
             "One last shot!"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME LOGIC FNS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-new-game [m chat-key user size difficulty]
  (let [worddata (words/get-word difficulty size)
        word (get worddata :word)
        definition (get worddata :definition)
        hits (get worddata :hits)]
    (new-game! chat-key word size definition hits user difficulty nil)
    (when (= "Elspeth" user)
      (.reply m "I hope you enjoy the game Elspeth!"))
    (when (= difficulty :hard)
      (.reply m (str "Ohh, feeling cocky, are we, " user "?")))
    (.replyWithImage m (get-drawn-img chat-key draw
                                      (board-width chat-key)
                                      (board-height chat-key)))))

(defn handle-guess [m chat-key guess user]
  (add-guess! chat-key guess user)
  (.replyWithImage m (get-drawn-img chat-key draw (board-width chat-key) (board-height chat-key)))

  (when (and (not (won? chat-key))
             (< (guesses-made chat-key) max-guesses))
    (.reply m (letter-help chat-key))
    (when (= (guesses-made chat-key) (- max-guesses 1))
      (.reply m (last-chance-message)))

    (when (and (> (guesses-made chat-key) 2)
         (no-progress? chat-key))
        (.reply m (rand-nth no-progress-responses)))))

(defn handle-win [m chat-key]
  (set-gameprop chat-key :won true)
  (let [guesses-count (guesses-made chat-key)
        congrats-msg (rand-nth (get win-responses guesses-count))]
    (.reply m congrats-msg))

  (.reply m (str "Definition: " (get-gameprop chat-key :answerdef)))

  (let [pbs (clear-game! chat-key m)
        user (get-gameprop chat-key :user)
        streak (get pbs :streak)
        won-rate-150 (get pbs :won-rate-150)
        guess-rate-20 (get pbs :guess-rate-20)]
    (when (and (not (nil? streak)) (= 0 (mod streak 5)))
      (.reply m (format "Well done %s!! Your PB streak is now %s." user streak)))
    (when (not (nil? won-rate-150))
      (.reply m (format "NEW PB!!! Well done %s!! Your PB win rate is now %s." user won-rate-150)))
    (when (not (nil? guess-rate-20))
      (.reply m (format "NEW PB!!! Well done %s!! Your PB guess rate is now %s." user guess-rate-20)))))

(defn handle-loss [m chat-key]
  (set-gameprop chat-key :won false)
  (.reply m (rand-nth lose-responses))
  (.reply m (format (rand-nth lose-reveal-responses)
                    (get-gameprop chat-key :answer)))
  (.reply m (format (rand-nth definition-reveal-responses)
                   (get-gameprop chat-key :answer)
                   (get-gameprop chat-key :answerdef)))
  (clear-game! chat-key m))


(defn handle-game-end [m chat-key]
  (if (won? chat-key)
    (handle-win m chat-key)
    (handle-loss m chat-key)))

(defn handle-gameplay [m chat-key guess user]
  (if (and (= (get-gameprop chat-key :size)
              (count (re-matches #"[a-zA-Z]*" guess)))
           (words/real-word? (clojure.string/upper-case guess)))
    (do
      (handle-guess m chat-key guess user)
      (when (or (won? chat-key) (= max-guesses (guesses-made chat-key)))
        (handle-game-end m chat-key)))
    (.reply m (rand-nth invalid-guess-responses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND HANDLING FNS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-stats-command [m chat-key user]
  (.replyWithImage m (get-quil-img chat-key (partial draw-stats user) stats-width stats-height :stats-img))
  (remove-img! chat-key :stats-img))

(defn handle-statsvs-command [m chat-key user]
  (let [opponent (clojure.string/trim (.getModText m))]
    (if (users/user-known? opponent)
      (let [days          7
            user-wins     (users/count-recent-wins user opponent days)
            opponent-wins (users/count-recent-wins opponent user days)
            draws         (users/count-recent-draws user opponent days)]
        (.replyWithImage m (get-quil-img chat-key (partial draw-pie-chart user-wins opponent-wins draws user opponent) 601 601 :pie-img))
        (remove-img! chat-key :pie-img))
      (.reply m "Er, who is that?"))))

(defn handle-streak-command [m user]
  (let [streak     (users/get-streak user)
        streak-msg (get-streak-msg streak user)]
    (.reply m streak-msg)))

(defn handle-challenge [m chat-key user challenge-user size difficulty]
  (if (users/user-known? user)
    (if (= (clojure.string/lower-case challenge-user) (clojure.string/lower-case user))
      (.reply m "You can't challenge yourself, silly.")
      (let [user1-chatid   (users/user-chat challenge-user)
            user2-chatid   (users/user-chat user)
            user1-chat-key (keyword (str user1-chatid))
            user2-chat-key (keyword (str user2-chatid))
            user1-msg      (new org.goat.core.Message user1-chatid "" true "goat")
            user2-msg      (new org.goat.core.Message user2-chatid "" true "goat")
            challenge-key  (combine-keys user1-chat-key user2-chat-key)]
        (println (str "challenge-key:" challenge-key))
        (if (not (or (playing? user1-chat-key) (playing? user2-chat-key)))
          (do
            ;; Init game for each user and message for each separately
            (new-challenge! challenge-key user1-chat-key user2-chat-key (.getChatId m) user challenge-user m)
            (.reply m "Starting a challenge match!! Let's go!")
            (let [worddata (words/get-word difficulty size)
                  word (get worddata :word)
                  definition (get worddata :definition)
                  hits (get worddata :hits)]
              (new-game! user1-chat-key word size definition hits challenge-user difficulty challenge-key)
              (.replyWithImage user1-msg (get-drawn-img user1-chat-key draw
                                                        (board-width user1-chat-key)
                                                        (board-height user1-chat-key)))
              (new-game! user2-chat-key word size definition hits user difficulty challenge-key)
              (.replyWithImage user2-msg (get-drawn-img user2-chat-key draw
                                                        (board-width user2-chat-key)
                                                        (board-height user2-chat-key)))))
          (.reply m "There's already a challenge match being played! Can't have too much challenge."))))
    (.reply m (str "I can't message you privately, " user
                   ", please message me privately the word \"setchat\" and try again."))))

(defn handle-wordle-command [m chat-key user trailing]
  (if (not (playing? chat-key))
    (let [size           (get-size trailing)
          challenge-user (users/user-known? (get-challenge (.getModText m)))
          difficulty     (get-difficulty trailing)]
      (cond
        (or (> size 10) (< size 2))
        (.reply m "Don't be an eejit. I won't do more than 10 or less than 2.")

        challenge-user
        (handle-challenge m chat-key user challenge-user size difficulty)

        :else
        (start-new-game m chat-key user size difficulty)))
    (.reply m "We're already playing a game, smart one.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE ENTRY POINT ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -processChannelMessage [_ m]
  (let [chat-key (keyword (str (.getChatId m)))
        guess    (clojure.string/upper-case (.getText m))
        command  (clojure.string/lower-case (.getModCommand m))
        trailing (clojure.string/lower-case (.getText m))
        user     (get-user m chat-key)]
    (case command
      "stats"   (handle-stats-command m chat-key user)
      "statsvs" (handle-statsvs-command m chat-key user)
      "streak"  (handle-streak-command m user)
      "wordle"  (handle-wordle-command m chat-key user trailing)
      (when (playing? chat-key)
        (handle-gameplay m chat-key guess user)))))

;; (defn -processChannelMessage
;;   [_ m]
;;   (println (str "state:" @state ))
;;   (let [chat-key (keyword (str (.getChatId m)))
;;         guess (clojure.string/upper-case (.getText m))
;;         command (clojure.string/lower-case (.getModCommand m))
;;         trailing (clojure.string/lower-case (.getText m))
;;         user (get-user m chat-key)]
;;     (if (= "stats" command)
;;       (do
;;         (.replyWithImage m (get-quil-img chat-key (partial draw-stats user) stats-width stats-height :stats-img))
;;         (remove-img! chat-key :stats-img))
;;       (if (= "statsvs" command)
;;         (let [opponent (clojure.string/trim (.getModText m))]
;;           (if (users/user-known? opponent)
;;             (let [ days 7
;;                    user-wins (users/count-recent-wins user opponent days)
;;                    opponent-wins (users/count-recent-wins opponent user days)
;;                    draws (users/count-recent-draws user opponent days) ]
;;               (.replyWithImage m (get-quil-img chat-key (partial draw-pie-chart user-wins opponent-wins draws user opponent) 601 601 :pie-img))
;;               (remove-img! chat-key :pie-img))
;;             (.reply m "Er, who is that?")))
;;       (if (= "streak" command)
;;         (let [streak (users/get-streak user)
;;               streak-msg (get-streak-msg streak user)]
;;           (.reply m streak-msg))
;;         (if (= "wordle" command)
;;           (if (not (playing? chat-key))
;;             (let [size (get-size trailing)
;;                   challenge-user (users/user-known? (get-challenge (.getModText m)))
;;                   difficulty (get-difficulty trailing)
;;                   worddata (words/get-word difficulty size)
;;                   word (get worddata :word)
;;                   definition (get worddata :definition)
;;                   hits (get worddata :hits)]
;;               (println (str "challenge user:" challenge-user) )
;;               (if (or (> size 10) (< size 2))
;;                 (.reply
;;                  m
;;                  "Don't be an eejit. I won't do more than 10 or less than 2.")
;;                 (if challenge-user
;;                   (if challenge-user
;;                     (if (users/user-known? user)
;;                       (if (= (clojure.string/lower-case challenge-user) (clojure.string/lower-case user))
;;                         (.reply m "You can't challenge yourself, silly.")
;;                         (let [user1-chatid   (users/user-chat challenge-user)
;;                               user2-chatid   (users/user-chat user)
;;                               user1-chat-key (keyword (str user1-chatid))
;;                               user2-chat-key (keyword (str user2-chatid))
;;                               user1-msg      (new org.goat.core.Message user1-chatid "" true "goat")
;;                               user2-msg      (new org.goat.core.Message user2-chatid "" true "goat")
;;                               challenge-key  (combine-keys user1-chat-key user2-chat-key)]
;;                           (println (str "challenge-key:" challenge-key))
;;                           (if (not (or (playing? user1-chat-key) (playing? user2-chat-key)))
;;                             (do
;;                               ;; Init game for each user and message for each seperately
;;                               (new-challenge! challenge-key user1-chat-key user2-chat-key (.getChatId m) user challenge-user m)
;;                               (.reply m "Starting a challenge match!! Let's go!")
;;                               (new-game! user1-chat-key word size definition hits challenge-user difficulty challenge-key)
;;                               (.replyWithImage user1-msg (get-drawn-img user1-chat-key draw
;;                                                                   (board-width user1-chat-key)
;;                                                                   (board-height user1-chat-key)))
;;                               (new-game! user2-chat-key word size definition hits user difficulty challenge-key)
;;                               (.replyWithImage user2-msg (get-drawn-img user2-chat-key draw
;;                                                                   (board-width user2-chat-key)
;;                                                                   (board-height user2-chat-key))))
;;                             (.reply m "There's already a challenge match being played! Can't have too much challenge."))))
;;                       (.reply m (str "I can't message you privately, " user
;;                                      ", please message me privately the word \"setchat\" and try again.")))
;;                     (.reply m "Er, who???")) ;; user not known
;;                   (do  ;;if not a challenge, it is a normal game
;;                     (new-game! chat-key word size definition hits user difficulty nil)
;;                     (if (= "Elspeth" user)
;;                       (.reply m "I hope you enjoy the game Elspeth!"))
;;                     (if (= difficulty :hard)
;;                       (.reply m (str "Ohh, feeling cocky, are we, " user "?")))
;;                     (.replyWithImage m (get-drawn-img chat-key draw
;;                                                 (board-width chat-key)
;;                                                 (board-height chat-key)))))))
;;             (.reply m "We're already playing a game, smart one."))
;;           (if (playing? chat-key)
;;             (do
;;               (println "guess:" guess ":answer:" (get-gameprop chat-key :answer))
;;               (if (= (get-gameprop chat-key :size)
;;                      (count (re-matches #"[a-zA-Z]*" guess)))
;;                 (if (words/real-word? (clojure.string/upper-case guess))
;;                   (do
;;                     (add-guess! chat-key guess user)
;;                     (.replyWithImage m (get-drawn-img chat-key draw (board-width chat-key) (board-height chat-key)))
;;                     (if (and (= (guesses-made chat-key) (- max-guesses 1))
;;                              (not (won? chat-key)))
;;                       (.reply m "Uh oh!"))
;;                     (if (and (> (guesses-made chat-key) 3)
;;                              (< (guesses-made chat-key) max-guesses)
;;                              (no-progress? chat-key)
;;                              (not (won? chat-key)))
;;                       (.reply
;;                        m
;;                        "You seem to be digging yourself into a hole! Do better."))
;;                     (if (and (> (guesses-made chat-key) 1)
;;                              (not (won? chat-key))
;;                              (< (guesses-made chat-key) max-guesses))
;;                       (.reply m (letter-help chat-key)))
;;                     (if (won? chat-key)
;;                       (do
;;                         (set-gameprop chat-key :won true)
;;                         (cond
;;                           (= max-guesses (guesses-made chat-key))
;;                           (.reply m "Phew! You won - barely.")
;;                           (= 5 (guesses-made chat-key))
;;                           (.reply
;;                            m
;;                            "You won, but I'm sure you feel a little bit dissapointed.")
;;                           (= 4 (guesses-made chat-key))
;;                           (.reply m
;;                                   "Well done! You won, and you won competently.")
;;                           (= 3 (guesses-made chat-key))
;;                           (.reply m "WOW!! An excellent performance!! You won!")
;;                           (= 2 (guesses-made chat-key))
;;                           (.reply
;;                            m
;;                            "Gasp! How could you possibly win like that? Are you gifted?")
;;                           (= 1 (guesses-made chat-key))
;;                           (.reply
;;                            m
;;                            "You won. I prostrate myself before you, for clearly I am in the presence of a wordle deity. Let this day live forever in our memories."))
;;                         (.reply m
;;                                 (str "Definition: "
;;                                      (get-gameprop chat-key :answerdef)))
;;                         ;;FIXME this doesn't seem to work
;;                         (let [pbs (clear-game! chat-key m)
;;                               streak (get pbs :streak)
;;                               won-rate-150 (get pbs :won-rate-150)
;;                               guess-rate-20 (get pbs :guess-rate-20)]
;;                           (println (str "pbs:" pbs))
;;                           (if (not (nil? streak))
;;                             (if (= 0 (mod streak 5))
;;                               (.reply m (format "Well done %s!! Your PB streak is now %s." user streak))))
;;                           (if (not (nil? won-rate-150))
;;                             (.reply m (format "NEW PB!!! Well done %s!! Your PB win rate is now %s." user won-rate-150)))
;;                           (if (not (nil? guess-rate-20))
;;                             (.reply m (format "NEW PB!!! Well done %s!! Your PB guess rate is now %s." user guess-rate-20)))))
;;                       (if (= max-guesses (guesses-made chat-key))
;;                         (do
;;                           (set-gameprop chat-key :won false)
;;                           (.reply
;;                              m
;;                              (str "Oh no! You lost the game! \n The answer was: "
;;                                   (get-gameprop chat-key :answer)))
;;                             (.reply m
;;                                     (str "The too-difficult-for-you word means: "
;;                                          (get-gameprop chat-key :answerdef)))
;;                             (clear-game! chat-key m)))))))))))))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '("wordle" "streak", "stats")))

(defn -messageType [_]
  Module/WANT_ALL_MESSAGES)
