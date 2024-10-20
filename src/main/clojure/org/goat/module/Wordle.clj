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
            [java.awt Color]
            [org.goat.core Module]
            [org.goat.core BotStats]))

(use 'clojure.test)

(def max-guesses 6)
(def letter-size 60)
(def letter-border 10)
(def A-Z (set (map char (concat (range 65 91)))))
(def bot (BotStats/getInstance))

;(defn file-name
;  "Get the wordle file name."
;  [chat-key]
;  (format "/run/user/1000/wordle.%s.png"
;          (str (symbol chat-key))))

(def state
  "key is :game-states->chatid - returning
  {:guesses [\"WRONG HOUSE\"] :answer \"RIGHT\"
  :size 5 :answerdef \"definition\" :hits 1000 }"
  (atom {}))

(def stats-state
  (atom {}))

(defn get-stats-img
  "Get stats image for a given chat"
  [chat-key]
  (get-in @stats-state [:stat-states chat-key :img]))

(defn set-stats-img
  "Set stats image for a given chat"
  [chat-key img]
  (swap! stats-state assoc-in [:stat-states chat-key :img] img))

(defn remove-stats-img!
  "Remove image associated with given chat"
  [chat-key]
  (swap! stats-state assoc-in
         [:stat-states]
         (dissoc (get-in @stats-state [:stat-states]) chat-key)))

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

(defn draw-stats
  "Draw the *stats* window"
  [user chat-key]
  (let [gr (q/create-graphics 800 1200 :p2d)]
    (q/with-graphics gr
      (draw-stats-gr gr chat-key user)
      (set-stats-img chat-key (pgraphics-to-bufferedimage (q/current-graphics)))
      (q/exit))))

(defn get-img
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

(defn get-stats-quil-img
  "Setup sketch for given underlying stats draw fn"
  [chat-key drawfn width height]
  (remove-stats-img! chat-key)
  (q/defsketch org.goat.module.Wordle
    :host "host"
    :renderer :p2d
    :size [width height]
    :setup (partial drawfn chat-key))

  (while (nil? (get-stats-img chat-key))
    (Thread/sleep 50))
  (get-stats-img chat-key))


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
  [img1 img2]
  (let [width1          (.getWidth img1)
        width2          (.getWidth img2)
        height1         (.getHeight img1)
        height2         (.getHeight img2)
        combined-width  (+ width1 width2)
        combined-height (max height1 height2)
        bgcolor         (Color. 26 26 28)
        combined-img    (BufferedImage. combined-width combined-height BufferedImage/TYPE_INT_ARGB)
        g2d             (.createGraphics combined-img)]
    (.setBackground  g2d bgcolor)
    (.drawImage g2d img1 0 0 nil)
    (.drawImage g2d img2 width1 0 nil)
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
                  combined-image (append-images second-img first-img)
                  reply          (partial reply-both m other-msg)]
              (reset! playing 0)
              (reply "The challenge has concluded!")
              (.replyWithImage m combined-image)
              (.replyWithImage other-msg combined-image)
              (let [p1         (get-fgameprop challenge-key :user)
                    p2         (get-gameprop chat-key :user)
                    p1-guesses (count (get-fgameprop challenge-key :guesses))
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

(defn -processChannelMessage
  [_ m]
  (println (str "state:" @state ))
  (let [chat-key (keyword (str (.getChatId m)))
        guess (clojure.string/upper-case (.getText m))
        command (clojure.string/lower-case (.getModCommand m))
        trailing (clojure.string/lower-case (.getText m))
        user (get-user m chat-key)]
    (if (= "stats" command)
      (do
        (.replyWithImage m (get-stats-quil-img chat-key (partial draw-stats user) stats-width stats-height ))
        (remove-stats-img! chat-key))
      (if (= "streak" command)
        (let [streak (users/get-streak user)
              streak-msg (get-streak-msg streak user)]
          (.reply m streak-msg))
        (if (= "wordle" command)
          (if (not (playing? chat-key))
            (let [size (get-size trailing)
                  challenge-user (users/user-known? (get-challenge (.getModText m)))
                  difficulty (get-difficulty trailing)
                  worddata (words/get-word difficulty size)
                  word (get worddata :word)
                  definition (get worddata :definition)
                  hits (get worddata :hits)]
              (println (str "challenge user:" challenge-user) )
              (if (or (> size 10) (< size 2))
                (.reply
                 m
                 "Don't be an eejit. I won't do more than 10 or less than 2.")
                (if challenge-user
                  (if challenge-user
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
                              ;; Init game for each user and message for each seperately
                              (new-challenge! challenge-key user1-chat-key user2-chat-key (.getChatId m) user challenge-user m)
                              (.reply m "Starting a challenge match!! Let's go!")
                              (new-game! user1-chat-key word size definition hits challenge-user difficulty challenge-key)
                              (.replyWithImage user1-msg (get-img user1-chat-key draw
                                                                  (board-width user1-chat-key)
                                                                  (board-height user1-chat-key)))
                              (new-game! user2-chat-key word size definition hits user difficulty challenge-key)
                              (.replyWithImage user2-msg (get-img user2-chat-key draw
                                                                  (board-width user2-chat-key)
                                                                  (board-height user2-chat-key))))
                            (.reply m "There's already a challenge match being played! Can't have too much challenge."))))
                      (.reply m (str "I can't message you privately, " user
                                     ", please message me privately the word \"setchat\" and try again.")))
                    (.reply m "Er, who???")) ;; user not known
                  (do  ;;if not a challenge, it is a normal game
                    (new-game! chat-key word size definition hits user difficulty nil)
                    (if (= "Elspeth" user)
                      (.reply m "I hope you enjoy the game Elspeth!"))
                    (if (= difficulty :hard)
                      (.reply m (str "Ohh, feeling cocky, are we, " user "?")))
                    (.replyWithImage m (get-img chat-key draw
                                                (board-width chat-key)
                                                (board-height chat-key)))))))
            (.reply m "We're already playing a game, smart one."))
          (if (playing? chat-key)
            (do
              (println "guess:" guess ":answer:" (get-gameprop chat-key :answer))
              (if (= (get-gameprop chat-key :size)
                     (count (re-matches #"[a-zA-Z]*" guess)))
                (if (words/real-word? (clojure.string/upper-case guess))
                  (do
                    (add-guess! chat-key guess user)
                    (.replyWithImage m (get-img chat-key draw (board-width chat-key) (board-height chat-key)))
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
                    (if (won? chat-key)
                      (do
                        (set-gameprop chat-key :won true)
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
                        ;;FIXME this doesn't seem to work
                        (let [pbs (clear-game! chat-key m)
                              streak (get pbs :streak)
                              won-rate-150 (get pbs :won-rate-150)
                              guess-rate-20 (get pbs :guess-rate-20)]
                          (println (str "pbs:" pbs))
                          (if (not (nil? streak))
                            (if (= 0 (mod streak 5))
                              (.reply m (format "Well done %s!! Your PB streak is now %s." user streak))))
                          (if (not (nil? won-rate-150))
                            (.reply m (format "NEW PB!!! Well done %s!! Your PB win rate is now %s." user won-rate-150)))
                          (if (not (nil? guess-rate-20))
                            (.reply m (format "NEW PB!!! Well done %s!! Your PB guess rate is now %s." user guess-rate-20)))))
                      (if (= max-guesses (guesses-made chat-key))
                        (do
                          (set-gameprop chat-key :won false)
                          (.reply
                             m
                             (str "Oh no! You lost the game! \n The answer was: "
                                  (get-gameprop chat-key :answer)))
                            (.reply m
                                    (str "The too-difficult-for-you word means: "
                                         (get-gameprop chat-key :answerdef)))
                            (clear-game! chat-key m))))))))))))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '("wordle" "streak", "stats")))

(defn -messageType [_]
  Module/WANT_ALL_MESSAGES)
