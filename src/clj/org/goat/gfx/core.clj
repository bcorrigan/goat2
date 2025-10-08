(ns org.goat.gfx.core
  "Generic 2D graphics utilities using Java2D.
   Provides a simple, functional API for creating graphics that can be used
   by any module (Wordle, WordStats, future modules, etc.)"
  (:import [java.awt Graphics2D Color Font RenderingHints]
           [java.awt.image BufferedImage]
           [java.awt.geom Rectangle2D]))

;; =============================================================================
;; Canvas Creation
;; =============================================================================

(defn create-canvas
  "Create a BufferedImage canvas with Graphics2D context.
   Returns a map with :canvas (BufferedImage) and :g2d (Graphics2D).
   Anti-aliasing is enabled by default."
  [width height]
  (let [bi (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        g2d (.createGraphics bi)]
    ;; Enable anti-aliasing by default
    (.setRenderingHint g2d RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g2d RenderingHints/KEY_TEXT_ANTIALIASING
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    (.setRenderingHint g2d RenderingHints/KEY_RENDERING
                       RenderingHints/VALUE_RENDER_QUALITY)
    {:canvas bi :g2d g2d}))

;; =============================================================================
;; Color Management
;; =============================================================================

(defn fill!
  "Set fill color. Accepts either RGB values or a Color object.
   Examples: (fill! g2d 255 0 0) or (fill! g2d (Color. 255 0 0))"
  ([g2d r g b]
   (.setColor g2d (Color. (int r) (int g) (int b))))
  ([g2d color]
   (.setColor g2d color)))

(defn stroke!
  "Set stroke color (RGB)"
  [g2d r g b]
  (.setColor g2d (Color. (int r) (int g) (int b))))

;; =============================================================================
;; Shape Drawing
;; =============================================================================

(defn background!
  "Fill entire canvas with color"
  [g2d width height r g b]
  (let [old-color (.getColor g2d)]
    (.setColor g2d (Color. (int r) (int g) (int b)))
    (.fillRect g2d 0 0 width height)
    (.setColor g2d old-color)))

(defn rect!
  "Draw filled rectangle"
  [g2d x y width height]
  (.fillRect g2d (int x) (int y) (int width) (int height)))

(defn rect-outline!
  "Draw rectangle outline (no fill)"
  [g2d x y width height]
  (.drawRect g2d (int x) (int y) (int width) (int height)))

(defn arc!
  "Draw filled arc (for pie charts, circular segments).
   Angles in degrees, starting from 3 o'clock position, counter-clockwise."
  [g2d x y width height start-angle arc-angle]
  (.fillArc g2d (int x) (int y) (int width) (int height)
            (int start-angle) (int arc-angle)))

;; =============================================================================
;; Text Rendering
;; =============================================================================

(defn set-font!
  "Set current font.
   Style can be Font/PLAIN, Font/BOLD, Font/ITALIC, or (bit-or Font/BOLD Font/ITALIC)"
  [g2d font-name style size]
  (.setFont g2d (Font. font-name style (int size))))

(defn text!
  "Draw text with alignment support.
   align-x: :left (default), :center, :right
   align-y: :baseline (default), :center, :top, :bottom"
  [g2d text x y & {:keys [align-x align-y]
                   :or {align-x :left align-y :baseline}}]
  (let [fm (.getFontMetrics g2d)
        bounds (.getStringBounds fm text g2d)
        text-width (.getWidth bounds)
        ascent (.getAscent fm)
        x-offset (case align-x
                   :left 0
                   :center (- (/ text-width 2))
                   :right (- text-width)
                   0)
        y-offset (case align-y
                   :baseline 0
                   :center (/ (- ascent (.getDescent fm)) 2)
                   :top ascent
                   :bottom (- (.getDescent fm))
                   0)]
    (.drawString g2d text
                 (int (+ x x-offset))
                 (int (+ y y-offset)))))

(defn text-bounds
  "Get the bounding box dimensions of text with current font.
   Returns {:width w :height h :ascent a :descent d}"
  [g2d text]
  (let [fm (.getFontMetrics g2d)
        bounds (.getStringBounds fm text g2d)]
    {:width (.getWidth bounds)
     :height (.getHeight bounds)
     :ascent (.getAscent fm)
     :descent (.getDescent fm)}))

;; =============================================================================
;; Canvas Finalization
;; =============================================================================

(defn finalize-canvas
  "Dispose Graphics2D context and return the BufferedImage.
   Always call this when done drawing to free resources."
  [{:keys [canvas g2d]}]
  (.dispose g2d)
  canvas)

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn with-canvas
  "Execute drawing function with a canvas, automatically finalizing.
   Drawing function receives {:canvas bi :g2d g2d} map.
   Returns BufferedImage.

   Example:
   (with-canvas 800 600
     (fn [{:keys [g2d]}]
       (background! g2d 800 600 0 0 0)
       (fill! g2d 255 255 255)
       (text! g2d \"Hello\" 400 300 :align-x :center)))"
  [width height draw-fn]
  (let [ctx (create-canvas width height)]
    (draw-fn ctx)
    (finalize-canvas ctx)))
