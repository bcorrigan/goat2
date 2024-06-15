(ns org.goat.module.CljTest
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:require [quil.core :as q :include-macros true]
            [org.goat.words.words :as words]
            [clojure.java.io :as io]))
(use 'clojure.test)

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

(defn add-guess!
  "Append the given guess to the state map."
  [chat-key guess]
  (let [current-guesses (get-in @state [:game-states chat-key :guesses])]
    (swap! state assoc-in
      [:game-states chat-key :guesses]
      (conj current-guesses guess))))

(defn get-gameprop
  "Get given property for given chatid game"
  [chat-key property]
  (get-in @state [:game-states chat-key property]))

(defn clear-game!
  "For a given chatid, remove all the game state entirely"
  [chat-key]
  (swap! state assoc-in
    [:game-states]
    (dissoc (get-in @state [:game-states]) chat-key)))

(defn new-game!
  "Add state for a new game of wordle."
  [chat-key answer size answerdef hits]
  (swap! state assoc-in
    [:game-states chat-key]
    {:guesses [],
     :answer answer,
     :size size,
     :answerdef answerdef,
     :hits hits}))

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

(defn tosyms
  "Compare given guess to answer. For each letter:
    If letter is correct and in right position - :revealed
    If letter is in the answer, but in the wrong position - :semiknown
    If letter is NOT in the answer - :wrong
    A vec of these keywords is returned for each letter in the supplied guess."
  [guess answer]
  (vec (tosymsl- guess answer)))

(defn tosymsl-
  ([guess answer] (tosyms2 guess answer (mask-answer guess answer)))
  ([guess answer masked-ans]
   (if (empty? guess)
     '()
     (let [ges_let (first guess)
           ans_let (first answer)
           ans_rest (clojure.string/join (rest guess))
           ges_rest (clojure.string/join (rest answer))]
       (cond (= ges_let ans_let) (conj (tosyms2 ans_rest ges_rest masked-ans)
                                       :revealed)
             (contains-char? masked-ans ges_let)
               (conj (tosyms2 ans_rest ges_rest (mask-first masked-ans ges_let))
                     :semiknown)
             :else (conj (tosyms2 ans_rest ges_rest masked-ans) :wrong))))))

(defn mask-first
  "Mask the first instance of c in answer"
  [answer c]
  (clojure.string/replace-first answer c "."))

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
  (q/rect x y 50 50)
  (q/fill 248 248 248)
  (q/text-align :center)
  (q/text-font (q/create-font "Source Code Pro" 40))
  ;(q/text-size 40)
  (q/text (str c) (+ 25 x) (+ 40 y)))

(defn draw-unrevealed
  "Draw an unrevealed letterbox at x,y."
  [x y]
  (q/fill 26 26 28)
  (q/stroke 168 168 168)
  (q/rect x y 50 50))

(defn draw-board
  "Draws the board letter by letter according to the chat's guesses array"
  [gr chat-key]
  (let [guesses (get-gameprop chat-key :guesses)
        answer (get-gameprop chat-key :answer)]
    (q/with-graphics
      gr
      (q/background 17 17 18)
      (println (range 10 (+ 10 (* 60 (count guesses))) 60))
      (doseq [i (range (count guesses))
              j (range (count (get guesses i)))]
        (let [guess (get guesses i)
              letter (nth (seq guess) j)
              sym (get (tosyms guess answer) j)
              y (+ 10 (* 60 i))
              x (+ 10 (* 60 j))]
          (draw-letter letter x y sym)))
      (doseq [y (range (+ 10 (* 60 (count guesses))) (+ 10 (* 60 6)) 60)
              x (range 10 260 60)]
        (draw-unrevealed x y)))))

(defn draw
  "Initites all the drawing and puts the image into /tmp"
  [chat-key]
  (let [gr (q/create-graphics 310 370 :java2d)]
    (q/with-graphics gr
                     (draw-board gr chat-key)
                     (q/save (format "/tmp/wordle.%s.png"
                                     (str (symbol chat-key))))
                     (.dispose gr)
                     (q/exit))))

(defn get-img
  "Setup sketch, call drawing fn, get the png, return File object"
  [chat-key]
  (q/defsketch org.goat.module.CljTest
               :host "host"
               :size [310 370]
               :setup (partial draw chat-key))
  ;;seems we need to give some time for sync to disk to happen or else we
  ;;get errors
  (Thread/sleep 200)
  (io/file (format "/tmp/wordle.%s.png" (str (symbol chat-key)))))

(defn -processChannelMessage
  [_ m]
  (println "0000000")
  (let [chat-key (keyword (str (.getChatId m)))
        guess (clojure.string/upper-case (.getText m))]
    (if (= "wordle" (.getModCommand m))
      (if (not (playing? chat-key))
        (let [worddata (words/get-word :easy)
              word (get worddata :word)
              definition (get worddata :definition)
              hits (get worddata :hits)]
          (new-game! chat-key word 5 definition hits)
          (.replyWithImage m (get-img chat-key)))
        (.reply m "We're already playing a game, smart one."))
      (if (playing? chat-key)
        (do (println "guess:" guess ":answer:" (get-gameprop chat-key :answer))
            (println "real-word?" (words/real-word? guess))
            (if (= (get-gameprop chat-key :size)
                   (count (re-matches #"[a-zA-Z]*" guess)))
              (if (words/real-word? (clojure.string/upper-case guess))
                (do (println "Got here 2")
                    (add-guess! chat-key guess)
                    (.replyWithImage m (get-img chat-key))
                    ; TODO store win/lose stats, streaks etc..
                    (if (won? chat-key)
                      (do (.reply m "Well done! You won!!!!")
                          (.reply m
                                  (str "Definition: "
                                       (get-gameprop chat-key :answerdef)))
                          (clear-game! chat-key))
                      (if (= 6 (guesses-made chat-key))
                        (do (.reply m "Oh no! You lost the game! Sorry.")
                            (.reply m
                                    (str
                                      "The too-difficult-for-you word means: "
                                      (get-gameprop chat-key :answerdef)))
                            (clear-game! chat-key))))))))))))

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '("wordle")))

(defn -messageType [_] 0)

(defn test [word])
