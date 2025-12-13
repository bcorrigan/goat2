(ns org.goat.module.Wordle
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]
   [org.goat.core.macros :refer [defmodule]]
   [org.goat.core.message :as message]
   [org.goat.core.message-parse :as msg-parse]
   [org.goat.db.users :as users]
   [org.goat.db.words :as words]
   [org.goat.util.str :as strutil]
   [org.goat.wordle.analytics :as analytics]
   [org.goat.wordle.gfx :as gfx]
   [org.goat.wordle.str :as msg])
  (:import
   (org.goat.core BotStats)))

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
  (some? (get-in (deref state) [:game-states chat-key])))

(defn new-game!
  "Add state for a new game of wordle."
  [chat-key answer size answerdef hits user difficulty challenge-key]
  (let [starttime (System/currentTimeMillis)
        challenge (some? challenge-key)]
    (swap! state assoc-in
           [:game-states chat-key]
           {:guesses []
            :excluded-letters #{}
            :included-letters #{}
            :answer answer
            :size size
            :type :single ;; can be :single (if one player) or :multi
            :starttime starttime
            :user user
            :difficulty difficulty
            :answerdef answerdef
            :challenge challenge
            :challenge-key challenge-key
            :drawing (atom false)
            :hits hits
            :facts-history []  ;; Track accumulated facts after each guess
            :ratings-history []  ;; Track guess ratings from analytics
            :remaining-words-history []  ;; Track count of remaining possible words after each guess
            })))

(defn new-challenge!
  "Add state for a new challenge"
  [challenge-key chat-key1 chat-key2 group-chat-key user1 user2 m]
  (swap! state assoc-in
         [:game-states challenge-key]
         {:chat-key1 chat-key1
          :chat-key2 chat-key2
          :group-chat-key group-chat-key
          :user1 user1
          :user2 user2
          :playing (atom 2)
          :m m}))
          ;; :first-game (all the details of the other game)

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

(defn no-progress?
  "If the last 3 guesses have not resulted in finding new information
   (other than excluded letters) - returns true"
  [chat-key]
  (apply =
         (mapv #(analytics/compare-guess-to-answer % (get-gameprop chat-key :answer))
               (take-last 3 (get-gameprop chat-key :guesses)))))

(defn calculate-facts-so-far
  "Calculate accumulated facts from all guesses made so far"
  [chat-key]
  (let [guesses (get-gameprop chat-key :guesses)
        answer (get-gameprop chat-key :answer)]
    (reduce (fn [facts guess]
              (analytics/add-to-facts facts guess answer))
            {}
            guesses)))

(defn add-guess!
  "Append the given guess to the state map.
   This will also update the lists of seen letters"
  [chat-key guess user]
  (let [syms (analytics/compare-guess-to-answer guess (get-gameprop chat-key :answer))
        le-syms (map vector syms guess)]
    (mapv #(do (cond (= :wrong (first %)) (add-exc-letter! chat-key (second %))
                     (= :revealed (first %)) (add-inc-letter! chat-key
                                                              (second %))))
          le-syms)
    (add-to-col! chat-key :guesses guess)
    (when (not= (get-gameprop chat-key :user) user)
      ;; multiple users have played this game
      (set-gameprop chat-key :type :group))))

(defn letter-help
  "Provides a visual aid in the form of the remaining unguessed letters"
  [chat-key]
  (let [less-excluded (set/difference A-Z
                                      (get-gameprop chat-key :excluded-letters))
        included (get-gameprop chat-key :included-letters)
        letter-aid (set/difference less-excluded included)]
    (str (str/join
           "\n"
           (strutil/chop (str/join " " (mapv str letter-aid)) 2))
         " \n [ "
         (str/join " " included)
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
  [m1 m2 text]
  (message/reply m1 text)
  (message/reply m2 text))

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
          other-msg     (msg-parse/create-message :chat-id other-chatid :sender "goat" :private? true :text "")
          playing       (get-gameprop challenge-key :playing)]
      ;; If both players press enter simultaneously, they could end up entering this code at the same moment?
      (locking playing
        (if (= @playing 2)
          (do
            (message/reply m (str "I'm afraid that " other-user " is not as speedy as you, but I will keep you posted in the main chat."))
            (message/reply other-msg (str this-user " just finished, " other-user ", better get your skates on!"))
            (set-gameprop challenge-key :first-game (get-game chat-key))
            (reset! playing 1))
          (when (= @playing 1)
            ;; wrap up the challenge now
            (let [first-img      (get-fgameprop challenge-key :img)
                  second-img     (get-gameprop chat-key :img)
                  p1             (get-fgameprop challenge-key :user)
                  p2             (get-gameprop chat-key :user)
                  combined-image (gfx/append-images second-img first-img p2 p1)
                  reply          (partial reply-both m other-msg)]
              (reset! playing 0)
              (reply "The challenge has concluded!")
              (message/reply-image m combined-image)
              (message/reply-image other-msg combined-image)
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
  (let [has-elspeth (str/includes? (str/lower-case (message/text m)) "elspeth")
        sender      (message/sender m)]
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
        remaining-words-counts (get-gameprop chat-key :remaining-words-history)
        ;; Use vector of [guess, classification] pairs to preserve duplicate guesses
        guesses-classified (mapv (fn [guess]
                                   [guess (analytics/compare-guess-to-answer guess answer)])
                                 guesses)]
    (gfx/get-img-sync chat-key {:type    :board
                                :size    size
                                :guesses-classified guesses-classified
                                :max-guesses max-guesses
                                :remaining-words-counts remaining-words-counts})))

(defn start-new-game [m chat-key user size difficulty]
  (let [worddata (words/get-word difficulty size)
        word (:word worddata)
        definition (:definition worddata)
        hits (:hits worddata)]
    (new-game! chat-key word size definition hits user difficulty nil)
    (when (= "Elspeth" user)
      (message/reply m "I hope you enjoy the game Elspeth!"))
    (when (= difficulty :hard)
      (message/reply m (str "Ohh, feeling cocky, are we, " user "?")))
    (message/reply-image m (get-board-img chat-key))))

(defn handle-guess [m chat-key guess user]
  ;; CRITICAL: Calculate analytics BEFORE adding the guess to game state
  ;; We need to evaluate the guess against the facts from previous guesses only
  (let [current-facts (calculate-facts-so-far chat-key)
        answer (get-gameprop chat-key :answer)
        work-required (analytics/work-level current-facts)
        
        ;; Calculate analytics for the current guess before state changes
        ;; Using proper information theory without cheating!
        analytics-feedback (when (= work-required :easy)
                            (try
                              (let [possible-answers (analytics/allowed-words-for-facts current-facts)
                                    guess-entropy (if (empty? possible-answers)
                                                   0.0
                                                   (second (analytics/evaluate-guess-quality possible-answers guess)))
                                    ;; Use the new non-cheating rate-guess function
                                    rating (analytics/rate-guess guess current-facts)
                                    ;; A guess is a "mistake" if:
                                    ;; 1. When there's only 1 possible word and you guess a different word
                                    ;; 2. When there are multiple words and your guess has very low entropy
                                    is-mistake? (or
                                                  ;; Mistake: 1 word left but you guessed something else
                                                  (and (= (count possible-answers) 1)
                                                       (not (contains? (set possible-answers) guess)))
                                                  ;; Mistake: multiple words but low information gain
                                                  (and (> (count possible-answers) 1)
                                                       (< guess-entropy 0.01)))]
                                {:rating rating
                                 :is-mistake? is-mistake?
                                 :entropy guess-entropy
                                 :possible-answers-count (count possible-answers)
                                 :feedback-msg (msg/get-feedback-message rating is-mistake?)})
                              (catch Exception e
                                ;; If analytics fail, just log and continue
                                (println (str "Analytics error: " (.getMessage e)))
                                nil)))]

    ;; Now add the guess to state (this changes the facts!)
    (add-guess! chat-key guess user)
    
    ;; Calculate remaining words count AFTER adding the guess
    ;; This represents how many words are still possible given all guesses so far
    (let [updated-facts (calculate-facts-so-far chat-key)
          remaining-count (analytics/allowed-words-for-facts updated-facts :count)]
      (add-to-col! chat-key :remaining-words-history remaining-count))
    
    (message/reply-image m (get-board-img chat-key))

    ;; Provide analytics feedback if we calculated it successfully
    (when analytics-feedback
      ;; Store rating for potential post-game analysis
      (add-to-col! chat-key :ratings-history (:rating analytics-feedback))
      
      ;; Provide feedback for notable guesses (mistakes or excellent plays)
      ;; Only praise exceptional plays (rating >= 9) when not in endgame (>4 words left)
      ;; to avoid praising lucky guesses
      (when (or (:is-mistake? analytics-feedback)
                (and (>= (:rating analytics-feedback) 9)
                     (> (:possible-answers-count analytics-feedback) 4))
                (<= (:rating analytics-feedback) 3))
        (message/reply m (:feedback-msg analytics-feedback))))

    ;; Continue with existing game logic
    (when (and (not (won? chat-key))
               (< (guesses-made chat-key) max-guesses))
      (message/reply m (letter-help chat-key))

      ;; Warn if next guess will be the last (after 5 guesses made, before guess 6)
      (when (= (guesses-made chat-key) (dec max-guesses))
        (message/reply m (msg/last-chance-message)))

      (when (and (> (guesses-made chat-key) 2)
                 (no-progress? chat-key))
        (message/reply m (rand-nth msg/no-progress-responses))))))

(defn handle-win [m chat-key]
  (set-gameprop chat-key :won true)
  (let [guesses-count (guesses-made chat-key)
        congrats-msg (rand-nth (get msg/win-responses guesses-count))]
    (message/reply m congrats-msg))

  ;; Check for lucky guess (many possible words before winning guess)
  (let [remaining-history (get-gameprop chat-key :remaining-words-history)
        ;; Get the count of possible words BEFORE the winning guess
        possible-before-win (when (seq remaining-history)
                             (last remaining-history))]
    (when (and possible-before-win (>= possible-before-win 5))
      (message/reply m (format "LUCKY GUESS!!! That was a %d:1 shot and you made it!!!" possible-before-win))))

  (message/reply m (str "Definition: " (get-gameprop chat-key :answerdef)))

  (let [pbs (clear-game! chat-key m)
        user (get-gameprop chat-key :user)
        streak (:streak pbs)
        won-rate-150 (:won-rate-150 pbs)
        guess-rate-20 (:guess-rate-20 pbs)]
    (when (and (some? streak) (zero? (mod streak 5)))
      (message/reply m (format "Well done %s!! Your PB streak is now %s." user streak)))
    (when (some? won-rate-150)
      (message/reply m (format "NEW PB!!! Well done %s!! Your PB win rate is now %s." user won-rate-150)))
    (when (some? guess-rate-20)
      (message/reply m (format "NEW PB!!! Well done %s!! Your PB guess rate is now %s." user guess-rate-20)))))

(defn handle-loss [m chat-key]
  (set-gameprop chat-key :won false)
  (message/reply m (rand-nth msg/lose-responses))
  (message/reply m (format (rand-nth msg/lose-reveal-responses)
                    (get-gameprop chat-key :answer)))
  (message/reply m (format (rand-nth msg/definition-reveal-responses)
                    (get-gameprop chat-key :answer)
                    (get-gameprop chat-key :answerdef)))
  (clear-game! chat-key m))

(defn handle-game-end [m chat-key]
  ;; Cache the final board image before clearing the game
  ;; This is needed for challenge games where the image will be retrieved later
  (set-gameprop chat-key :img (get-board-img chat-key))
  
  (if (won? chat-key)
    (handle-win m chat-key)
    (handle-loss m chat-key)))

(defn handle-gameplay [m chat-key guess user]
  (if (and (= (get-gameprop chat-key :size)
              (count (re-matches #"[a-zA-Z]*" guess)))
           (words/real-word? (str/upper-case guess)))
    (do
      (handle-guess m chat-key guess user)
      (when (or (won? chat-key) (= max-guesses (guesses-made chat-key)))
        (handle-game-end m chat-key)))
    (message/reply m (rand-nth msg/invalid-guess-responses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND HANDLING FNS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-stats-command [m chat-key user]
  (message/reply-image m (gfx/get-img-sync chat-key {:user user :type :stats})))

(defn handle-statsvs-command [m chat-key user]
  (let [opponent (str/trim (message/mod-text m))]
    (if (users/user-known? opponent)
      (let [days          7
            user-wins     (users/count-recent-wins user opponent days)
            opponent-wins (users/count-recent-wins opponent user days)
            draws         (users/count-recent-draws user opponent days)]
        (message/reply-image m (gfx/get-img-sync chat-key {:user-wins user-wins :opponent-wins opponent-wins :draws draws :user user :opponent opponent :type :pie})))
      (message/reply m "Er, who is that?"))))

(defn handle-league-command [m chat-key user]
  (let [mod-text (str/trim (message/mod-text m))
        parts (str/split mod-text #"\s+")
        opponent (first parts)
        time-period (when (> (count parts) 1) (str/lower-case (second parts)))
        all-time? (= time-period "all")]
    (if (users/user-known? opponent)
      (let [days (if all-time? :all 7)
            league-data (users/get-league-table user opponent days)
            user-points (:p1-points league-data)
            opponent-points (:p2-points league-data)
            games-played (:games-played league-data)
            time-desc (if all-time? "All Time" "Last 7 days")
            reply-msg (format "League Table (%s) - %d game%s played:\n%s: %d points\n%s: %d points"
                              time-desc
                              games-played
                              (if (= games-played 1) "" "s")
                              user user-points
                              opponent opponent-points)]
        (message/reply m reply-msg))
      (message/reply m "Er, who is that?"))))

(defn handle-streak-command [m user]
  (let [streak     (users/get-streak user)
        streak-msg (msg/get-streak-msg streak user)]
    (message/reply m streak-msg)))

(defn handle-challenge [m user challenge-user size difficulty]
  (if (users/user-known? user)
    (if (= (str/lower-case challenge-user) (str/lower-case user))
      (message/reply m "You can't challenge yourself, silly.")
      (let [user1-chatid   (users/user-chat challenge-user)
            user2-chatid   (users/user-chat user)
            user1-chat-key (keyword (str user1-chatid))
            user2-chat-key (keyword (str user2-chatid))
            user1-msg      (msg-parse/create-message :chat-id user1-chatid :sender "goat" :private? true :text "")
            user2-msg      (msg-parse/create-message :chat-id user2-chatid :sender "goat" :private? true :text "")
            challenge-key  (combine-keys user1-chat-key user2-chat-key)]
        (if-not (or
                 (playing? user1-chat-key)
                 (playing? user2-chat-key))
          (do
            (new-challenge! challenge-key user1-chat-key user2-chat-key (message/chat-id m) user challenge-user m)
            (message/reply m "Starting a challenge match!! Let's go!")
            (let [worddata (words/get-word difficulty size)
                  word (:word worddata)
                  definition (:definition worddata)
                  hits (:hits worddata)]
              (new-game! user1-chat-key word size definition hits challenge-user difficulty challenge-key)
              (message/reply-image user1-msg (get-board-img user1-chat-key))
              (new-game! user2-chat-key word size definition hits user difficulty challenge-key)
              (message/reply-image user2-msg (get-board-img user2-chat-key))))
          (message/reply m "There's already a challenge match being played! Can't have too much challenge."))))
    (message/reply m (str "I can't message you privately, " user
                   ", please message me privately the word \"setchat\" and try again."))))

(defn handle-wordle-command [m chat-key user trailing]
  (if-not (playing? chat-key)
    (let [size (parse-size trailing)
          challenge-user (users/user-known? (get-challenge (message/mod-text m)))
          difficulty (get-difficulty trailing)]
      (cond
        (or (> size 10) (< size 2))
            (message/reply m "Don't be an eejit. I won't do more than 10 or less than 2.")
        challenge-user
            (handle-challenge m user challenge-user size difficulty)
        :else (start-new-game m chat-key user size difficulty)))
    (message/reply m "We're already playing a game, smart one.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE ENTRY POINT ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule Wordle
  :commands [:wordle :streak :stats :statsvs :league]
  :receive-messages :all

  (defn process-channel-message [m]
    ;; Skip messages without text (e.g., document uploads)
    (when (message/has-text? m)
      (let [chat-key (keyword (str (message/chat-id m)))
            guess    (str/upper-case (message/text m))
            command  (message/command m)  ; Returns keyword or nil
            trailing (str/lower-case (message/text m))
            user     (get-user m chat-key)]
        (case command
          :stats   (handle-stats-command m chat-key user)
          :statsvs (handle-statsvs-command m chat-key user)
          :league  (handle-league-command m chat-key user)
          :streak  (handle-streak-command m user)
          :wordle  (handle-wordle-command m chat-key user trailing)
          (when (playing? chat-key)
            (handle-gameplay m chat-key guess user)))))))
