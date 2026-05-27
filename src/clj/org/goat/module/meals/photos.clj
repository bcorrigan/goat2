(ns org.goat.module.meals.photos
  "On-disk storage for meal photos. One file per meal-id under photo-dir.
   Format is sniffed from the leading bytes (JPEG/PNG); defaults to .jpg."
  (:require [clojure.java.io :as io])
  (:import [java.io File ByteArrayInputStream]
           [java.awt.image RenderedImage]
           [javax.imageio ImageIO]))

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
  "Persist photo bytes for meal-id. Returns the File written.
   Removes any pre-existing file for the same meal-id (different extension)."
  [meal-id ^bytes bs]
  (ensure-dir!)
  ;; Clear out any stale file at another extension
  (doseq [^File f (candidate-files meal-id)]
    (when (.exists f) (.delete f)))
  (let [ext (sniff-ext bs)
        ^File f (io/file *photo-dir* (str meal-id "." ext))]
    (with-open [out (io/output-stream f)]
      (.write out bs))
    f))

(defn load-photo
  "Load the photo for meal-id as a RenderedImage suitable for msg/reply-image.
   Returns nil if no photo exists or it can't be decoded."
  [meal-id]
  (when-let [^File f (photo-path meal-id)]
    (try
      (ImageIO/read f)
      (catch Exception _ nil))))

(defn load-photo-bytes
  "Load raw photo bytes for meal-id, or nil if no photo exists."
  [meal-id]
  (when-let [^File f (photo-path meal-id)]
    (with-open [in (io/input-stream f)
                out (java.io.ByteArrayOutputStream.)]
      (io/copy in out)
      (.toByteArray out))))
