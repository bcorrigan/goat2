(ns org.goat.util.str)


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
