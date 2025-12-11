(ns org.goat.util.str
  (:require [clojure.string :as str]))


(defn contains-char?
  "Does the string contain the char c?"
  [string c]
  (boolean (some #(= % c) string)))

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

(defn escape-html
  "Escape HTML special characters to prevent Telegram parsing errors.
   Converts: & → &amp;, < → &lt;, > → &gt;
   Use this when sending user-provided or external text to Telegram with HTML formatting."
  [text]
  (-> text
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))
