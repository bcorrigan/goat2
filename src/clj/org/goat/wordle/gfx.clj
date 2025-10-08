(ns org.goat.wordle.gfx
  "Wordle-specific graphics rendering using generic gfx package"
  (:require [clojure.math :as math]
            [org.goat.db.users :as users]
            [org.goat.gfx.core :as gfx])
  (:import  [java.awt.image BufferedImage]
            [java.awt Color Font RenderingHints Graphics2D]))

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

(defn draw-letter
  "Draw the given letter on the board at position x,y.
    The style is :revealed, :wrong or :semiknown."
  [g2d c x y style]
  ;; Set box color based on style
  (cond (= :revealed style)  (gfx/fill! g2d 83 140 78)
        (= :wrong style)     (gfx/fill! g2d 58 58 60)
        (= :semiknown style) (gfx/fill! g2d 180 158 59))

  ;; Draw filled rectangle (box background)
  (gfx/rect! g2d x y (- letter-size letter-border) (- letter-size letter-border))

  ;; Draw box border
  (gfx/stroke! g2d 17 17 18)
  (gfx/rect-outline! g2d x y (- letter-size letter-border) (- letter-size letter-border))

  ;; Draw letter text
  (gfx/fill! g2d 248 248 248)
  (gfx/set-font! g2d "Source Code Pro" Font/PLAIN (- letter-size (* 2 letter-border)))
  (gfx/text! g2d (str c)
             (+ (/ (- letter-size letter-border) 2) x)
             (+ (- letter-size (* 2 letter-border)) y)
             :align-x :center))

(defn draw-unrevealed
  "Draw an unrevealed letterbox at x,y."
  [g2d x y]
  (gfx/fill! g2d 26 26 28)
  (gfx/rect! g2d x y (- letter-size letter-border) (- letter-size letter-border))
  (gfx/stroke! g2d 168 168 168)
  (gfx/rect-outline! g2d x y (- letter-size letter-border) (- letter-size letter-border)))

(defn draw-board-gr
  "Draws the board letter by letter according to the chat's guesses array"
  [g2d width height guesses-classified size max-guesses]
  (let [guesses (vec (keys guesses-classified))]
    ;; Background
    (gfx/background! g2d width height 17 17 18)

    ;; Draw revealed letters
    (doseq [i (range (count guesses))
            j (range (count (get guesses i)))]
      (let [guess (get guesses i)
            letter (nth (seq guess) j)
            sym (get (get guesses-classified guess) j)
            y (+ letter-border (* letter-size i))
            x (+ letter-border (* letter-size j))]
        (draw-letter g2d letter x y sym)))

    ;; Draw unrevealed boxes
    (doseq [y (range (+ letter-border (* letter-size (count guesses)))
                     (+ letter-border (* letter-size max-guesses))
                     letter-size)
            x (range letter-border
                     (+ (* size letter-size)
                        (* 2 letter-border))
                     letter-size)]
      (draw-unrevealed g2d x y))))

(defn get-no-cols
  "Calculate the number of columns to draw.
  This is the number of guesses +1, or 6, whichever is smallest"
  [guesses]
  (if (> guesses 5)
    6
    (inc guesses)))

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
  "Draw the Wordle board and return BufferedImage.
   Returns the image directly - no longer async or using image cache."
  [guesses-classified size max-guesses]
  (let [width (board-width size)
        height (board-height (count guesses-classified))
        {:keys [canvas g2d]} (gfx/create-canvas width height)]
    (draw-board-gr g2d width height guesses-classified size max-guesses)
    (gfx/finalize-canvas {:canvas canvas :g2d g2d})))

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
  [g2d user]
  (let [stats (users/get-stats user)
        games (:results-150 stats)]
    ;; Background
    (gfx/background! g2d stats-width stats-height 17 17 18)

    ;; Draw game result squares
    (doseq [i (range (count games))]
      (let [col (mod i 5)
            row (int (math/floor (/ i 5)))
            game (nth games i)
            won (:won game)
            guesses (:guesses game)
            boxcol (get-wonbox-col won guesses)]
        (gfx/fill! g2d (first boxcol) (second boxcol) (nth boxcol 2))
        (gfx/rect! g2d (* col stats-square-px)
                       (* row stats-square-px)
                       (- stats-square-px 4)
                       (- stats-square-px 4))))

    ;; Text rendering
    (let [games-played (+ (or (:games-won stats) 0) (or (:games-lost stats) 0))
          games-played-150 (+ (or (:games-won-150 stats) 0) (or (:games-lost-150 stats) 0))
          win-ratio (* 100 (double (/ (or (:games-won-150 stats) 0) games-played-150)))
          win-ratio-fmt (format "%.3f" win-ratio)
          guess-rate-150 (format "%.3f" (or (:guess-rate-150 stats) 0))
          guess-rate-20 (format "%.3f" (or (:guess-rate-20 stats) 0))
          games-played-20 (+ (or (:games-won-20 stats) 0) (or (:games-lost-20 stats) 0))
          win-ratio-20 (* 100 (double (/ (or (:games-won-20 stats) 0) games-played-20)))
          win-ratio-20-fmt (format "%.3f" win-ratio-20)]

      ;; Main stats
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN big-font-pt)
      (gfx/fill! g2d 248 248 248) ;; white
      (gfx/text! g2d (str user "'s records:") text-indent (text-row-px 1))
      (gfx/text! g2d (str "Games played: " games-played) text-indent (text-row-px 2))
      (gfx/text! g2d (str "Streak: " (users/get-streak user)) text-indent (text-row-px 3))

      ;; Last 150 section
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN small-font-pt)
      (gfx/fill! g2d 148 148 148) ;; grey
      (gfx/text! g2d "        ---LAST 150---" text-indent (text-row-px 4))
      (gfx/fill! g2d 248 248 248) ;; white
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN big-font-pt)
      (gfx/text! g2d (str "Win ratio: " win-ratio-fmt "%") text-indent (text-row-px 5))
      (gfx/text! g2d (str "Guess rate: " guess-rate-150) text-indent (text-row-px 6))

      ;; Last 20 section
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN small-font-pt)
      (gfx/fill! g2d 148 148 148) ;; grey
      (gfx/text! g2d "        ---LAST 20---" text-indent (text-row-px 7))
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN big-font-pt)

      ;; Win ratio 20 (colored based on improvement)
      (if (>= win-ratio-20 win-ratio)
        (gfx/fill! g2d 123 177 114)  ;; green
        (gfx/fill! g2d 255 167 255)) ;; pink
      (gfx/text! g2d (str "Win ratio: " win-ratio-20-fmt "%") text-indent (text-row-px 8))

      ;; Guess rate 20 (colored based on improvement)
      (if (<= (or (:guess-rate-20 stats) 0) (or (:guess-rate-150 stats) 0))
        (gfx/fill! g2d 123 177 114)  ;; green
        (gfx/fill! g2d 255 167 255)) ;; pink
      (gfx/text! g2d (str "Guess rate: " guess-rate-20) text-indent (text-row-px 9))

      ;; Personal bests section
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN small-font-pt)
      (gfx/fill! g2d 148 148 148) ;; grey
      (gfx/text! g2d "        ---PERSONAL BESTS---" text-indent (text-row-px 10))
      (gfx/fill! g2d 248 248 248) ;; white
      (gfx/set-font! g2d "Noto Sans" Font/PLAIN big-font-pt)
      (gfx/text! g2d (str "Streak: " (or (users/get-record user :streak) 0)) text-indent (text-row-px 11))
      (gfx/text! g2d (str "/150 Win ratio: " (format "%.3f" (* 100 (or (users/get-record user :won-rate-150) 0.0)))) text-indent (text-row-px 12))
      (gfx/text! g2d (str "/20 Guess ratio: " (format "%.3f" (or (users/get-record user :guess-rate-20) 0.0))) text-indent (text-row-px 13)))))

(defn draw-stats
  "Draw the stats window and return BufferedImage.
   Returns the image directly - no longer async or using image cache."
  [user]
  (let [{:keys [canvas g2d]} (gfx/create-canvas stats-width stats-height)]
    (draw-stats-gr g2d user)
    (gfx/finalize-canvas {:canvas canvas :g2d g2d})))

(defn draw-pie-chart-gr
  "Draw a pie chart in wordle coloring representing head2head combat stats.
  p1 and p2 are the names of p1 and p2, other params are number of wins and draws."
  [g2d player1-wins player2-wins draws p1 p2]
  (let [width            600
        height           600
        center-x         (/ width 2)
        center-y         (- (/ height 2) 80)
        radius           400
        total            (+ player1-wins player2-wins draws)
        p1-color         [83 140 78]
        p2-color         [180 158 59]
        draw-color       [58 58 60]
        draw-slice       (fn [start-angle arc-angle color]
                           (gfx/fill! g2d (first color) (second color) (nth color 2))
                           ;; arc! takes x, y as top-left corner of bounding box
                           (gfx/arc! g2d
                                     (- center-x (/ radius 2))
                                     (- center-y (/ radius 2))
                                     radius radius
                                     (int start-angle)
                                     (int arc-angle)))
        draw-legend-item (fn [text color x y]
                           (gfx/fill! g2d (first color) (second color) (nth color 2))
                           (gfx/rect! g2d x y 60 30)
                           (gfx/fill! g2d 248 248 248)
                           (gfx/text! g2d text (+ x 70) (+ y 15) :align-x :left :align-y :center))]

    ;; Background
    (gfx/background! g2d width height 26 26 28)
    ;; smooth and no-stroke handled by gfx/create-canvas

    ;; Draw the slices
    (let [p1-angle (* 360 (if (pos? player1-wins)
                            (/ player1-wins total)
                            0))
          p2-angle (* 360 (if (pos? player2-wins)
                            (/ player2-wins total)
                            0))
          draws-angle (* 360 (if (pos? draws)
                               (/ draws total)
                               0))]
      ;; Player 1 slice
      (draw-slice 0 p1-angle p1-color)

      ;; Player 2 slice
      (draw-slice p1-angle p2-angle p2-color)

      ;; Draws slice
      (draw-slice (+ p1-angle p2-angle) draws-angle draw-color))

    ;; Add labels
    (gfx/set-font! g2d "Sans Serif" Font/PLAIN 20)
    (draw-legend-item (str p1 ": " player1-wins) p1-color (- center-x 280) 450)
    (draw-legend-item (str p2 ": " player2-wins) p2-color (- center-x 280) 500)
    (draw-legend-item (str "Draws: " draws) draw-color (- center-x 280) 550)))

(defn draw-pie-chart
  "Draw the pie chart and return BufferedImage.
   Returns the image directly - no longer async or using image cache."
  [player1-wins player2-wins draws p1 p2]
  (let [{:keys [canvas g2d]} (gfx/create-canvas pie-width pie-height)]
    (draw-pie-chart-gr g2d player1-wins player2-wins draws p1 p2)
    (gfx/finalize-canvas {:canvas canvas :g2d g2d})))

(defn get-img-sync
  "Get an image of the given type and return it synchronously.
  info must have :type of :pie :stats or :board - to get a pie chart, stats chart, or board state.
  Now directly calls the drawing functions - no more async/threading!"
  [chat-key info]
  (cond
    (= (:type info) :pie)
        (draw-pie-chart (:user-wins info) (:opponent-wins info) (:draws info) (:user info) (:opponent info))
    (= (:type info) :stats)
        (draw-stats (:user info))
    (= (:type info) :board)
        (draw-board (:guesses-classified info) (:size info) (:max-guesses info))))

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
