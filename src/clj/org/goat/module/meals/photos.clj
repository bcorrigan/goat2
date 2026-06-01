(ns org.goat.module.meals.photos
  "On-disk storage for meal photos. One file per meal-id under photo-dir.
   Format is sniffed from the leading bytes (JPEG/PNG); defaults to .jpg."
  (:require [clojure.java.io :as io])
  (:import [java.io File ByteArrayInputStream ByteArrayOutputStream]
           [java.awt Graphics2D RenderingHints Image]
           [java.awt.image BufferedImage RenderedImage]
           [javax.imageio IIOImage ImageIO ImageWriteParam]
           [javax.imageio.plugins.jpeg JPEGImageWriteParam]))

(def ^:dynamic *photo-dir*
  "Directory where meal photo files live. Dynamic so tests can rebind."
  "mealpics")

(defn ensure-dir!
  "Create the photo directory if it doesn't exist."
  []
  (.mkdirs (io/file *photo-dir*)))

(defn- sniff-ext
  "Return file extension based on the first few magic bytes.
   Falls back to 'jpg' for unknown types."
  [^bytes bs]
  (cond
    (and bs (>= (alength bs) 3)
         (= (bit-and (aget bs 0) 0xFF) 0xFF)
         (= (bit-and (aget bs 1) 0xFF) 0xD8)
         (= (bit-and (aget bs 2) 0xFF) 0xFF))
    "jpg"

    (and bs (>= (alength bs) 8)
         (= (bit-and (aget bs 0) 0xFF) 0x89)
         (= (bit-and (aget bs 1) 0xFF) 0x50)
         (= (bit-and (aget bs 2) 0xFF) 0x4E)
         (= (bit-and (aget bs 3) 0xFF) 0x47))
    "png"

    :else "jpg"))

(def ^:private max-dimension
  "Photos exceeding this on the longest side will be scaled down."
  1600)

(defn- resize-image
  "Scale a BufferedImage down to fit within max-dimension. Returns a new image."
  [^BufferedImage orig]
  (if (and (<= (.getWidth orig) max-dimension)
           (<= (.getHeight orig) max-dimension))
    orig
    (let [w (.getWidth orig)
          h (.getHeight orig)
          ratio (min (/ (double max-dimension) w) (/ (double max-dimension) h))
          new-w (int (* w ratio))
          new-h (int (* h ratio))
          scaled (BufferedImage. new-w new-h BufferedImage/TYPE_INT_RGB)
          g (.createGraphics scaled)]
      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
      (.drawImage g orig 0 0 new-w new-h nil)
      (.dispose g)
      scaled)))

(defn- image->bytes
  "Encode a BufferedImage to JPEG bytes at the given quality (0.0–1.0)."
  [^BufferedImage img ^double quality]
  (let [out (ByteArrayOutputStream.)
        writer (ImageIO/getImageWritersByFormatName "jpeg")
        iw (.next writer)
        params (.getDefaultWriteParam iw)]
    (.setCompressionMode params ImageWriteParam/MODE_EXPLICIT)
    (.setCompressionQuality params (float quality))
    (try
      (.setOutput iw (ImageIO/createImageOutputStream out))
      (.write iw nil (IIOImage. img nil nil) params)
      (finally
        (.dispose iw)))
    (.toByteArray out)))

(defn- resize-bytes
  "If the image exceeds max-dimension, scale it down and return JPEG bytes."
  [^bytes bs]
  (let [orig (ImageIO/read (ByteArrayInputStream. bs))]
    (if (or (nil? orig)
            (and (<= (.getWidth orig) max-dimension)
                 (<= (.getHeight orig) max-dimension)))
      bs
      (image->bytes (resize-image orig) 0.85))))

(defn- candidate-files
  "All on-disk filenames that could hold a photo for meal-id."
  [meal-id]
  (for [ext ["jpg" "jpeg" "png"]]
    (io/file *photo-dir* (str meal-id "." ext))))

(defn photo-path
  "Return the existing photo File for this meal-id, or nil if none exists."
  [meal-id]
  (->> (candidate-files meal-id)
       (filter #(.exists ^File %))
       first))

(defn save-photo!
  "Persist photo bytes for meal-id, resizing if needed. Returns the File written."
  [meal-id ^bytes bs]
  (ensure-dir!)
  (doseq [^File f (candidate-files meal-id)]
    (when (.exists f) (.delete f)))
  (let [resized (resize-bytes bs)
        ext (sniff-ext resized)
        ^File f (io/file *photo-dir* (str meal-id "." ext))]
    (with-open [out (io/output-stream f)]
      (.write out resized))
    f))

(defn load-photo
  "Load the photo for meal-id as a RenderedImage, resizing if needed.
   Returns nil if no photo exists or it can't be decoded."
  [meal-id]
  (when-let [^File f (photo-path meal-id)]
    (try
      (when-let [img (ImageIO/read f)]
        (resize-image img))
      (catch Exception _ nil))))

(defn load-photo-bytes
  "Load raw photo bytes for meal-id, resizing if needed."
  [meal-id]
  (when-let [^File f (photo-path meal-id)]
    (try
      (let [bs (with-open [in (io/input-stream f)
                            out (java.io.ByteArrayOutputStream.)]
                 (io/copy in out)
                 (.toByteArray out))]
        (resize-bytes bs))
      (catch Exception _ nil))))
