(ns org.goat.module.flour
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.core.command-parser :as parser]
            [org.goat.core.format :as fmt]
            [clojure.string :as str]))


;; Algorithm:
;; 1. Define base absorption rates for each flour type
;; 2. Calculate water needed for each flour type
;; 3. Sum total water needed
;; 4. Apply hydration adjustment factor (if supplied by user!)

;; SOO, for input {:rye 50 :wholemeal 75 :white 275 :hydration 0.95}:
;; - Rye water = 50 × 0.88888 = 44.44ml
;; - Wholemeal water = 75 × 0.775 = 58.125ml
;; - White water = 275 * 0.75 = 206.25ml
;; - Total water = 308.815
;; - Hydration adjusted water = (308.815ml * 0.95) = 293.374245 = 293ml

(def absorption {:rye 0.88888888
                 :wrye 0.90
                 :spelt 0.666666
                 :wspelt 0.73333
                 :white 0.75
                 :wholemeal 0.775})

(def flour-descriptions
  {:rye "white rye flour"
   :wrye "wholemeal rye flour"
   :spelt "white spelt flour"
   :wspelt "wholemeal spelt flour"
   :white "very strong white flour"
   :wholemeal "very strong wholemeal flour"})

(defn water-for-grain [grain]
  "For given symbol - :rye :wrye :spelt etc - calculate required ml f water"
  (* (second grain) (get absorption (first grain) 0)))

(defn water-for [flour]
  "Calculate how many ml of water needed for passed in flour mixture.
   Accepts a map with optional keys :rye :wrye :spelt :wspelt :white :wholemeal - each keyed to number of grams of each. w-prefix means wholemeal, otherwise it is white"
  (*
   (reduce + (map water-for-grain flour))
   (get flour :hydration 1)))

(defn- flour-keys
  "Return all valid flour type keywords"
  []
  (keys absorption))

(defn- format-help
  "Format help message listing flour types and example usage"
  [f]
  (let [flour-table (->> (sort (flour-keys))
                        (map (fn [k]
                               (str "  " (fmt/bold f (name k))
                                    " = " (get flour-descriptions k))))
                        (str/join "\n"))]
    (str "🌾 " (fmt/bold f "Flour Hydration Calculator") "\n\n"
         "Calculate water needed for bread baking based on flour types.\n\n"
         (fmt/bold f "Available flour types:") "\n"
         flour-table "\n\n"
         (fmt/bold f "Usage:") "\n"
         "flour white=300 wholemeal=100\n"
         "flour spelt=200 wspelt=100 wrye=50\n"
         "flour white=275 wholemeal=75 rye=50 hydration=0.95\n\n"
         (fmt/bold f "Notes:") "\n"
         "• Amounts are in grams\n"
         "• Optional hydration parameter (default=1.0) adjusts final water amount")))

(defn- parse-flour-params
  "Parse flour parameters from command text.
   Returns a map with:
     :valid - map of valid flour types to gram amounts
     :invalid - set of invalid flour type keywords (excluding hydration/h)
     :has-invalid? - boolean indicating if there were invalid flour types"
  [params]
  (let [valid-keys (set (flour-keys))
        ignored-keys #{:hydration :h}
        result (reduce-kv
                (fn [acc k v]
                  (cond
                    (contains? ignored-keys k)
                    acc

                    (contains? valid-keys k)
                    (let [grams (parser/parse-int v 0)]
                      (if (> grams 0)
                        (update acc :valid assoc k grams)
                        acc))

                    :else
                    (-> acc
                        (update :invalid (fnil conj #{}) k)
                        (assoc :has-invalid? true))))
                {:valid {} :invalid #{} :has-invalid? false}
                params)]
    result))

(defmodule flour
  :commands [:flour]
  :receive-messages :commands
  :wants-private true

  (defn process-message [m]
    (let [f (msg/fmt m)
          text (msg/mod-text m)
          parsed (parser/parse-parameters text)
          params (:params parsed)]

      (if (empty? params)
        ;; No parameters - show help
        (msg/reply m (format-help f))

        ;; Parse flour parameters
        (let [parse-result (parse-flour-params params)
              flour-map (:valid parse-result)
              invalid-flours (:invalid parse-result)
              has-invalid? (:has-invalid? parse-result)]

          (cond
            ;; Has invalid flour types - show error with valid options
            has-invalid?
            (let [invalid-list (str/join ", " (map #(fmt/bold f (name %)) (sort invalid-flours)))
                  valid-list (str/join ", " (map #(fmt/bold f (name %)) (sort (flour-keys))))]
              (msg/reply m (str "❌ Unknown flour type(s): " invalid-list "\n\n"
                              "Valid flour types are:\n" valid-list)))

            ;; No valid flour parameters
            (empty? flour-map)
            (msg/reply m (str "❓ I didn't understand those flour types. "
                            "Type 'flour' with no parameters for help."))

            ;; Valid flour parameters - calculate water
            :else
            (let [;; Add hydration if specified
                  hydration-param (parser/get-param params :hydration :h)
                  flour-with-hydration (if hydration-param
                                         (try
                                           (assoc flour-map :hydration (Double/parseDouble hydration-param))
                                           (catch NumberFormatException e
                                             flour-map))
                                         flour-map)
                  ;; Calculate water needed
                  water-ml (Math/round (water-for flour-with-hydration))
                  sender-name (msg/sender m)
                  ;; Format flour list for display
                  flour-list (->> flour-with-hydration
                                 (filter #(not= :hydration (first %)))
                                 (map (fn [[k v]] (str v "g " (fmt/bold f (name k)))))
                                 (str/join ", "))
                  hydration-note (when-let [h (:hydration flour-with-hydration)]
                                  (when (not= h 1.0)
                                    (str " (hydration factor: " h ")")))]
              (msg/reply m (str "🌾 " sender-name ", you will need "
                              (fmt/bold f (str water-ml "ml"))
                              " of water for " flour-list
                              hydration-note ".")))))))))
