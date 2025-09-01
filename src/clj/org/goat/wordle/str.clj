(ns org.goat.wordle.str)

;; All the various fns which are just different response strings etc can go here

(defn get-streak-msg
  "Get a congratulatory (or critical) message depending on user's streak"
  [streak user]
  (cond (zero? streak)
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
  "Chastise user for a guess which is not a word"
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
  "win responses varying from obseqious to disappointed depending on performance"
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
  "Chastise user for losing"
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
  "Dictionary def response when they lost"
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
  "Definition def response when they lost"
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
  "User has made several guesses in a row with no progress.. lets chastise them"
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

(defn last-chance-message
  "User is on their last guess, remind them the stakes are high"
  []
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

;; Analytics-based feedback messages

(def mistake-messages
  "Messages for guesses that reveal no new information (rating 0)"
  ["That guess revealed no new information!"
   "Hmm, that wasn't very helpful..."
   "You might want to think more strategically!"
   "Consider what you already know before guessing."
   "That guess didn't narrow things down at all."
   "Try to choose words that eliminate possibilities!"
   "Think about what letters you've already ruled out."
   "Strategy tip: pick words that test new letters!"
   "That guess was a bit wasteful of your limited tries."
   "Remember, every guess should teach you something new!"])

(def poor-messages
  "Messages for low-quality guesses (rating 1-3)"
  ["There might be better strategic options..."
   "That's not your strongest guess today."
   "Could be more efficient, but you're trying!"
   "Room for improvement on that one."
   "Next time, think about information theory!"
   "Consider which letters would be most informative."
   "That guess was okay, but not optimal."
   "Try to maximize what you learn from each guess!"
   "Strategic thinking could help here."
   "Keep working on your word selection strategy!"])

(def good-messages
  "Messages for decent guesses (rating 4-6)"
  ["Not bad! Solid strategic thinking."
   "Good guess! You're making progress."
   "Nice work! That revealed useful information."
   "Decent choice! You're on the right track."
   "That's a reasonable approach to the puzzle."
   "Good strategy! You're thinking analytically."
   "Well chosen! That helps narrow things down."
   "Smart guess! You're learning as you go."
   "That's the kind of thinking that wins games!"
   "Good job considering your options!"])

(def excellent-messages
  "Messages for high-quality guesses (rating 7-10)"
  ["Excellent guess! Very strategic!"
   "That was a brilliant deduction!"
   "Perfect information gathering!"
   "Outstanding analytical thinking!"
   "Superb strategic choice!"
   "That's exactly what a Wordle expert would guess!"
   "Brilliant! You're playing like a pro!"
   "Masterful strategy! That was optimal."
   "Phenomenal guess! Maximum information gained!"
   "You're thinking like a true Wordle strategist!"
   "That guess was mathematically beautiful!"
   "Perfect! That's how you maximize entropy!"
   "Exceptional analytical play!"
   "That's the gold standard of Wordle guesses!"
   "Absolutely stellar strategic thinking!"])

(defn get-feedback-message
  "Get appropriate feedback message based on guess quality rating.
   
   Args:
   - rating: Integer from 0-10 representing guess quality
   - is-mistake?: Boolean indicating if guess reveals no new information
   
   Returns: Random appropriate feedback message string"
  [rating is-mistake?]
  (cond
    is-mistake?
    (rand-nth mistake-messages)
    
    (<= rating 3)
    (rand-nth poor-messages)
    
    (<= rating 6)
    (rand-nth good-messages)
    
    :else ; rating >= 7
    (rand-nth excellent-messages)))

(def performance-messages
  "Post-game performance feedback templates"
  {:skilled-win 
   ["Masterfully played! Your analytical approach paid off!"
    "You dominated that puzzle with superior strategy!"
    "Excellent strategic play throughout the game!"
    "Your analytical thinking was spot on!"
    "That's how you play Wordle at the highest level!"
    "Outstanding performance! You've mastered the art of Wordle!"]
   
   :lucky-win
   ["You won, but it was more luck than skill..."
    "Victory through fortune rather than strategy!"
    "The wordle gods smiled upon you today!"
    "Sometimes it's better to be lucky than good!"
    "Your luck compensated for some questionable guesses."
    "Fortune favored you despite the analytical shortcomings!"]
   
   :skilled-loss  
   ["Unlucky! You played very well analytically."
    "Great strategy, just didn't work out this time."
    "Your analytical approach was sound, just unlucky!"
    "Excellent strategic thinking, but the word was tricky."
    "You played like a pro, sometimes the answer is just hard."
    "Superior analytical play, but the puzzle got you this time."]
   
   :poor-performance
   ["Room for improvement in your strategy."
    "Consider thinking more analytically next time."
    "Try focusing on information theory in your guesses."
    "Strategic thinking could elevate your game."
    "Work on maximizing information from each guess."
    "Consider learning some Wordle strategy fundamentals."]})

(defn format-performance-message
  "Format a post-game performance message.
   
   Args:
   - performance: Map with :avg-rating, :num-guesses, :luck-factor, :overall-grade
   - user: Player name
   - won?: Whether the player won
   
   Returns: Formatted performance message string"
  [performance user won?]
  (let [grade (:overall-grade performance)
        message-key (cond
                      (and won? (#{:skilled :excellent} grade)) :skilled-win
                      (and won? (= grade :lucky)) :lucky-win
                      (and (not won?) (#{:skilled :excellent} grade)) :skilled-loss
                      :else :poor-performance)
        base-message (rand-nth (get performance-messages message-key))]
    (format "%s %s" user base-message)))
