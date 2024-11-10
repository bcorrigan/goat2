(ns org.goat.module.Wordle
  (:gen-class :extends org.goat.core.Module
              :exposes {WANT_ALL_MESSAGES {:get WANT_ALL_MESSAGES}})
  (:require [org.goat.db.words :as words]
            [org.goat.db.users :as users]
            [org.goat.wordle.gfx :as gfx]
            [org.goat.wordle.str :as msg]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import  [org.goat.core Module]
            [org.goat.core BotStats]))

(use 'clojure.test)

(def max-guesses 6)
(def A-Z (set (map char (concat (range 65 91)))))
(def bot (BotStats/getInstance))

(def state
  "key is :game-states->chatid - returning
  {:guesses [\"WRONG HOUSE\"] :answer \"RIGHT\"
  :size 5 :answerdef \"definition\" :hits 1000 }"
  (atom {}))

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
    (when (not (= (get-gameprop chat-key :user) user))
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

(defn parse-size
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
  (when (str/includes? s "challenge")
    (let [mr (re-matcher  #"\W*\w+\W+(\w+)\W*" s)]
      (.matches mr)
      (when (.hasMatch  mr)
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
  (when (get-gameprop chat-key :challenge)
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
          (when (= @playing 1)
            ;;wrap up the challenge now
            (let [first-img      (get-fgameprop challenge-key :img)
                  second-img     (get-gameprop chat-key :img)
                  p1             (get-fgameprop challenge-key :user)
                  p2             (get-gameprop chat-key :user)
                  combined-image (gfx/append-images second-img first-img p2 p1)
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

(defn get-board-img
  "Marshalls reqd data to call our drawing function and calls it, returning the bufferedimage"
  [chat-key]
  (let [guesses (get-gameprop chat-key :guesses)
        size (get-gameprop chat-key :size)
        answer (get-gameprop chat-key :answer)
        guesses-classified (zipmap guesses
                                   (map #(compare-guess-to-answer % answer)
                                        guesses))]
    (gfx/get-img-sync chat-key {:type    :board
                                :size    size
                                :guesses-classified guesses-classified
                                :max-guesses max-guesses})))

(defn start-new-game [m chat-key user size difficulty]
  (let [worddata (words/get-word difficulty size)
        word (:word worddata)
        definition (:definition worddata)
        hits (:hits worddata)]
    (new-game! chat-key word size definition hits user difficulty nil)
    (when (= "Elspeth" user)
      (.reply m "I hope you enjoy the game Elspeth!"))
    (when (= difficulty :hard)
      (.reply m (str "Ohh, feeling cocky, are we, " user "?")))
    (.replyWithImage m (get-board-img chat-key))))

(defn handle-guess [m chat-key guess user]
  (add-guess! chat-key guess user)
  (.replyWithImage m (get-board-img chat-key))

  (when (and (not (won? chat-key))
             (< (guesses-made chat-key) max-guesses))
    (.reply m (letter-help chat-key))
    (when (= (guesses-made chat-key) (- max-guesses 1))
      (.reply m (msg/last-chance-message)))

    (when (and (> (guesses-made chat-key) 2)
         (no-progress? chat-key))
        (.reply m (rand-nth msg/no-progress-responses)))))

(defn handle-win [m chat-key]
  (set-gameprop chat-key :won true)
  (let [guesses-count (guesses-made chat-key)
        congrats-msg (rand-nth (get msg/win-responses guesses-count))]
    (.reply m congrats-msg))

  (.reply m (str "Definition: " (get-gameprop chat-key :answerdef)))

  (let [pbs (clear-game! chat-key m)
        user (get-gameprop chat-key :user)
        streak (:streak pbs)
        won-rate-150 (:won-rate-150 pbs)
        guess-rate-20 (:guess-rate-20 pbs)]
    (when (and (not (nil? streak)) (= 0 (mod streak 5)))
      (.reply m (format "Well done %s!! Your PB streak is now %s." user streak)))
    (when (not (nil? won-rate-150))
      (.reply m (format "NEW PB!!! Well done %s!! Your PB win rate is now %s." user won-rate-150)))
    (when (not (nil? guess-rate-20))
      (.reply m (format "NEW PB!!! Well done %s!! Your PB guess rate is now %s." user guess-rate-20)))))

(defn handle-loss [m chat-key]
  (set-gameprop chat-key :won false)
  (.reply m (rand-nth msg/lose-responses))
  (.reply m (format (rand-nth msg/lose-reveal-responses)
                    (get-gameprop chat-key :answer)))
  (.reply m (format (rand-nth msg/definition-reveal-responses)
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
    (.reply m (rand-nth msg/invalid-guess-responses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND HANDLING FNS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-stats-command [m chat-key user]
  (.replyWithImage m (gfx/get-img-sync chat-key {:user user :type :stats})))

(defn handle-statsvs-command [m chat-key user]
  (let [opponent (clojure.string/trim (.getModText m))]
    (if (users/user-known? opponent)
      (let [days          7
            user-wins     (users/count-recent-wins user opponent days)
            opponent-wins (users/count-recent-wins opponent user days)
            draws         (users/count-recent-draws user opponent days)]
        (.replyWithImage m (gfx/get-img-sync chat-key {:user-wins user-wins :opponent-wins opponent-wins :draws draws :user user :opponent opponent :type :pie})))
      (.reply m "Er, who is that?"))))

(defn handle-streak-command [m user]
  (let [streak     (users/get-streak user)
        streak-msg (msg/get-streak-msg streak user)]
    (.reply m streak-msg)))

(defn handle-challenge [m user challenge-user size difficulty]
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
        (if (not (or (playing? user1-chat-key) (playing? user2-chat-key)))
          (do
            ;; Init game for each user and message for each separately
            (new-challenge! challenge-key user1-chat-key user2-chat-key (.getChatId m) user challenge-user m)
            (.reply m "Starting a challenge match!! Let's go!")
            (let [worddata (words/get-word difficulty size)
                  word (:word worddata)
                  definition (:definition worddata)
                  hits (:hits worddata)]
              (new-game! user1-chat-key word size definition hits challenge-user difficulty challenge-key)
              (.replyWithImage user1-msg (get-board-img user1-chat-key))
              (new-game! user2-chat-key word size definition hits user difficulty challenge-key)
              (.replyWithImage user2-msg (get-board-img user2-chat-key))))
          (.reply m "There's already a challenge match being played! Can't have too much challenge."))))
    (.reply m (str "I can't message you privately, " user
                   ", please message me privately the word \"setchat\" and try again."))))

(defn handle-wordle-command [m chat-key user trailing]
  (if (not (playing? chat-key))
    (let [size           (parse-size trailing)
          challenge-user (users/user-known? (get-challenge (.getModText m)))
          difficulty     (get-difficulty trailing)]
      (cond
        (or (> size 10) (< size 2))
        (.reply m "Don't be an eejit. I won't do more than 10 or less than 2.")

        challenge-user
        (handle-challenge m user challenge-user size difficulty)

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

(defn -processPrivateMessage [this m] (-processChannelMessage this m))

(defn -getCommands [_] (into-array String '("wordle" "streak", "stats", "statsvs")))

(defn -messageType [_]
  Module/WANT_ALL_MESSAGES)
