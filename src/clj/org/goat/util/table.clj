(ns org.goat.util.table
  "Utilities for formatting tabular data in monospace."
  (:require [clojure.string :as str]
            [org.goat.util.str :as str-util]))

(defn calculate-column-widths
  "Calculate optimal width for each column based on content.
   Returns vector of widths, respecting max-width.
   Accounts for HTML escaping when calculating widths."
  [headers rows formats max-width]
  (let [num-cols (count headers)
        format-fn (fn [col-idx val]
                    (str-util/escape-html (str ((get formats col-idx str) val))))]
    (vec
     (for [col-idx (range num-cols)]
       (let [header-width (count (str-util/escape-html (get headers col-idx)))
             cell-widths (map #(count (format-fn col-idx (get % col-idx))) rows)]
         (min max-width (apply max header-width (or (seq cell-widths) [0]))))))))

(defn pad-cell
  "Pad a cell value to width with specified alignment.
   Truncates with '...' if exceeds width."
  [value width align]
  (let [s (str value)
        len (count s)]
    (cond
      (= len width) s
      (> len width) (str (subs s 0 (- width 3)) "...")
      (= align :right) (str (apply str (repeat (- width len) \space)) s)
      :else (str s (apply str (repeat (- width len) \space))))))

(defn build-table-lines
  "Build vector of table lines from headers and rows.
   Returns lines without HTML tags (just the table content)."
  [headers rows widths aligns formats]
  (let [;; Format header
        header-cells (map-indexed
                      (fn [idx h]
                        (pad-cell (str-util/escape-html h) (get widths idx) :left))
                      headers)
        header-line (str/join " " header-cells)

        ;; Format separator (dashes under header)
        sep-line (str/join " "
                          (map #(apply str (repeat % \-)) widths))

        ;; Format data rows
        data-lines (for [row rows]
                    (let [cells (map-indexed
                                 (fn [idx val]
                                   (let [fmt-fn (get formats idx str)
                                         aligned (get aligns idx :left)
                                         formatted (fmt-fn val)
                                         escaped (str-util/escape-html (str formatted))]
                                     (pad-cell escaped (get widths idx) aligned)))
                                 row)]
                      (str/join " " cells)))]

    (concat [header-line sep-line] data-lines)))

(defn format-table
  "Generate an aligned table from headers and rows.

   Args:
   - formatter: Platform formatter (from msg/fmt m)
   - headers: Vector of column header strings
   - rows: Vector of vectors containing cell values
   - opts: Optional configuration map
     - :align - Vector of :left or :right per column (default: all :left)
     - :formats - Vector of format functions per column (default: str)
     - :max-width - Max width per column before truncation (default: 20)

   Returns: String with monospace-formatted table wrapped in <pre> tags

   Example:
   (format-table fmt
                 [\"User\" \"Messages\" \"Avg Length\"]
                 [[\"alice\" 150 12.3]
                  [\"bob\" 89 15.7]]
                 {:align [:left :right :right]
                  :formats [str #(format \"%,d\" %) #(format \"%.1f\" %)]})"
  ([formatter headers rows]
   (format-table formatter headers rows {}))
  ([formatter headers rows {:keys [align formats max-width]
                            :or {align (vec (repeat (count headers) :left))
                                 formats (vec (repeat (count headers) str))
                                 max-width 20}}]
   (if (empty? rows)
     ""
     (let [widths (calculate-column-widths headers rows formats max-width)
           table-lines (build-table-lines headers rows widths align formats)
           table-content (str/join "\n" table-lines)]
       (str (:pre formatter) table-content (:end-pre formatter))))))
