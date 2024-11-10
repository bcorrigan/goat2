(ns org.goat.wordle.gfx
  (:require [quil.core :as q :include-macros true]
            [clojure.math :as math]
            [org.goat.db.users :as users])
  (:import  [java.awt.image BufferedImage]
            [java.awt Color Font RenderingHints]))

(def letter-size "How many px wide is a letter box" 60)
(def letter-border "How many px between letter boxes on all sides" 10)

;; stats gfx related constants
(def stats-width "Width of the statistics graphic" 800)
(def stats-height "height of the statistics graphic" 1200)
(def small-font-pt "Point size of the small font in stats graphic"  30)
(def big-font-pt "Point size of the big font in stats graphic" 50)
(def stats-square-px "How many px to a side for each square in the stats graphic"  40)
(def text-height-px 70)
(def text-indent (* 6 stats-square-px))
(defn text-row-px
  "Calculate offset for given row"
  [row]
  (+ 10 (* text-height-px row)))

(def pie-width "Width of pie chart" 600)
(def pie-height "Height of pie chart" 600)

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

(defn pgraphics-to-bufferedimage
  "Go under the hood of quil to get the underlying BufferedImage. Bizarrely quil API itself has no way to get this."
  [pg]
  (let [w   (.width pg)
        h   (.height pg)
        bi  (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g2d (.createGraphics bi)]
    (.drawImage g2d (.getImage pg) 0 0 nil)
    (.dispose g2d)
    bi))

(defn draw-letter
  "Draw the given letter on the board at position x,y.
    The style is :revealed, :wrong or :semiknown."
  [c x y style]
  (cond (= :revealed style)  (q/fill 83 140 78)
        (= :wrong style)     (q/fill 58 58 60)
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

(defn draw-board-gr
  "Draws the board letter by letter according to the chat's guesses array"
  [gr guesses-classified size max-guesses]
  (let [guesses (vec (keys guesses-classified))]
    (q/with-graphics
      gr
      (q/background 17 17 18)
      (doseq [i (range (count guesses))
              j (range (count (get guesses i)))]
        (let [guess (get guesses i)
              letter (nth (seq guess) j)
              sym (get (get guesses-classified guess) j)
              y (+ letter-border (* letter-size i))
              x (+ letter-border (* letter-size j))]
          (draw-letter letter x y sym)))
      (doseq [y (range (+ letter-border (* letter-size (count guesses)))
                       (+ letter-border (* letter-size max-guesses))
                       letter-size)
              x (range letter-border
                       (+ (* size letter-size)
                          (* 2 letter-border))
                       letter-size)]
        (draw-unrevealed x y)))))

(defn get-no-cols
  "Calculate the number of columns to draw.
  This is the number of guesses +1, or 6, whichever is smallest"
  [guesses]
  (if (> guesses 5)
    6
    (+ 1 guesses)))

(defn board-width
  "get the width of the board for the given chat-key"
  [size]
  (+ (* size letter-size)
     letter-border))

(defn board-height
  "get the width of the board for the given chat-key"
  [guesses]
  (+ (* (get-no-cols guesses) letter-size) letter-border))

(defn draw-board
  "Initiates all the drawing and puts the image onto img cache.
   This method is called asynchronously in an applet context so it can't just return the image."
  [chat-key guesses-classified size max-guesses]
  (let [gr (q/create-graphics (board-width size)
                              (board-height (count guesses-classified))
                              :p2d)]
    (q/with-graphics gr
      (draw-board-gr gr guesses-classified size max-guesses)
      (set-img chat-key :board (pgraphics-to-bufferedimage (q/current-graphics)))
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

(defn draw-stats-gr
  "Draw onto the given graphics our stats"
  [gr user]
  (let [stats (users/get-stats user)
        games (:results-150 stats)]
    (q/with-graphics gr
      (q/background 17 17 18) ;;black
      (doseq [i (range (count games))]
        (let [col (mod i 5)
              row (int (math/floor (/ i 5)))
              game (nth games i)
              won (:won game)
              guesses (:guesses game)
              boxcol (get-wonbox-col won guesses)]
          (q/fill (first boxcol) (second boxcol) (nth boxcol 2))
          (q/stroke (first boxcol) (second boxcol) (nth boxcol 2))
          (q/rect (* col stats-square-px)
                  (* row stats-square-px)
                  (- stats-square-px 4)
                  (- stats-square-px 4))))
      ;; Text
      (let [games-played (+ (or (:games-won stats) 0) (or (:games-lost stats) 0))
            games-played-150 (+ (or (:games-won-150 stats) 0) (or (:games-lost-150 stats) 0))
            win-ratio (* 100 (double (/ (or (:games-won-150 stats) 0) games-played-150)))
            win-ratio-fmt (format "%.3f" win-ratio)
            guess-rate-150 (format "%.3f" (or (:guess-rate-150 stats) 0))
            guess-rate-20 (format "%.3f" (or (:guess-rate-20 stats) 0))
            games-played-20 (+ (or (:games-won-20 stats) 0) (or (:games-lost-20 stats) 0))
            win-ratio-20 (* 100 (double (/ (or (:games-won-20 stats) 0) games-played-20)))
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
        (if (<= (or (:guess-rate-20 stats) 0) (or (:guess-rate-150 stats) 0))
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
      (draw-stats-gr gr user)
      (set-img chat-key :stats  (pgraphics-to-bufferedimage (q/current-graphics)))
      (q/exit))))

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
      (let [p1-to-angle (* 360 (if (> player1-wins 0)
                       (/ player1-wins total)
                       0))
            p2-to-angle (* 360 (if (> player2-wins 0)
                              (/ (+ player1-wins player2-wins) total)
                              p1-to-angle))]
        (draw-slice 0
                    p1-to-angle
                    p1-color)  ; Red for player 1

        (draw-slice p1-to-angle
                    p2-to-angle
                    p2-color)  ; Blue for player 2
        (draw-slice p2-to-angle
                    360
                    draw-color))  ; Green for draws

                                        ; Add labels
      (q/text-size 20)
      (draw-legend-item (str p1 ": " player1-wins) p1-color (- center-x 280) 450)
      (draw-legend-item (str p2 ": " player2-wins) p2-color (- center-x 280) 500)
      (draw-legend-item (str "Draws: " draws ) draw-color (- center-x 280) 550))))

(defn draw-pie-chart
  "Draw the pie chart representing who's best"
  [player1-wins player2-wins draws p1 p2 chat-key]
  (let [gr (q/create-graphics 600 600 :p2d)]
    (q/with-graphics gr
      (draw-pie-chart-gr gr player1-wins player2-wins draws p1 p2)
      ;; need some get-pie-img - can this not be abstracted?
      (set-img chat-key :pie (pgraphics-to-bufferedimage (q/current-graphics)))
      (q/exit))))

(defn width-for-info
  "Returns the image width needed for the given draw info"
  [info]
  (cond
    (= (:type info) :pie)
        pie-width
    (= (:type info) :stats)
        stats-width
    (= (:type info) :board)
        (board-width (:size info))))

(defn height-for-info
  "Returns the image width needed for the given draw info"
  [info]
  (cond
    (= (:type info) :pie)
        pie-height
    (= (:type info) :stats)
        stats-height
    (= (:type info) :board)
        (board-height (count (:guesses-classified info)))))

(defn drawfn-for-info
  "Returns the draw function required to draw the given draw info"
  [info chat-key]
  (cond
    (= (:type info) :pie)
        (partial draw-pie-chart (:user-wins info) (:opponent-wins info) (:draws info) (:user info) (:opponent info) chat-key)
    (= (:type info) :stats)
        (partial draw-stats (:user info) chat-key)
    (= (:type info) :board)
    (partial draw-board chat-key (:guesses-classified info) (:size info) (:max-guesses info))))

;; FIXME fold get-quil-img into this as well
;; original args - chat-key drawfn width height type
(defn get-img-sync
  "Get an image of the given *type* and return it synchronously.
  info must have :type of :pie :stats or :board - to get a pie chart, stats chart, or board state.
  If it has :board state it should then also have the board info (ie number of letters)
  chat-key is required to allow \"caching\" each ongoing chat seperately.."
  [chat-key info]
  (remove-img! chat-key (:type info))
  (q/defsketch org.goat.module.Wordle
    :host "host"
    :renderer :p2d
    :size [(width-for-info info) (height-for-info info)]
    :setup (drawfn-for-info info chat-key)  )
  ;; defsketch spawns an applet under the hood which has its own thread
  ;; so we wait till it completes by checking if img is ready
  (while (nil? (get-img chat-key (:type info)))
    (Thread/sleep 50))
  (get-img chat-key (:type info)))

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
