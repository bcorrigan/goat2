(ns org.goat.util.str
  "String utility functions.

   Merged from org.goat.util.StringUtil.java - contains only the functions
   actually used in the codebase."
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

;; =============================================================================
;; Duration Formatting
;; =============================================================================

(defn vshort-duration-string
  "Format a duration in milliseconds as a very short string.

   Returns abbreviated format like '1d 2h 3m 5s'.
   Shows all non-zero units.

   Examples:
   (vshort-duration-string 5000) => '5s'
   (vshort-duration-string 3665000) => '1h 1m 5s'
   (vshort-duration-string 90061000) => '1d 1h 1m 1s'"
  [interval-ms]
  (let [seconds (quot interval-ms 1000)
        minutes (quot seconds 60)
        hours (quot minutes 60)
        days (quot hours 24)

        sec-part (rem seconds 60)
        min-part (rem minutes 60)
        hour-part (rem hours 24)

        parts (cond-> []
                (pos? days) (conj (str days "d"))
                (pos? hour-part) (conj (str hour-part "h"))
                (pos? min-part) (conj (str min-part "m"))
                (pos? sec-part) (conj (str sec-part "s")))]

    (if (empty? parts)
      "0s"
      (str/join " " parts))))
