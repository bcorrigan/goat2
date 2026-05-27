(ns org.goat.module.Meals
  "Save and recall meal memories.

   Add flow: user sends a photo with caption 'meal <title>'. The bot then
   asks for a score, ingredients, and an optional description as separate
   follow-up messages. The photo lands on disk under mealpics/<id>.jpg
   and metadata is stored in resources/meals.db.

   Recall: 'randommeal', 'findmeal <term>', or 'meal <id>'."
  (:require [clojure.string :as str]
            [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.meals :as db]
            [org.goat.module.meals.photos :as photos])
  (:import [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(def ^:const wizard-timeout-ms (* 10 60 1000))

(defonce wizards
  ;; key: [chat-id sender] -> {:stage :score|:ingredients|:description
  ;;                           :title <str> :image-bytes <bytes> :score <int>
  ;;                           :ingredients [<str>...] :started-at <ms>}
  (atom {}))

(defn- wizard-key [m]
  [(msg/chat-id m) (msg/sender m)])

(defn- prune-stale!
  "Drop any wizards that have outlived wizard-timeout-ms."
  []
  (let [now (System/currentTimeMillis)]
    (swap! wizards
           (fn [ws]
             (into {}
                   (remove (fn [[_ w]] (> (- now (:started-at w)) wizard-timeout-ms))
                           ws))))))

(defn- start-wizard! [k title image-bytes]
  (swap! wizards assoc k
         {:stage :score
          :title title
          :image-bytes image-bytes
          :ingredients []
          :started-at (System/currentTimeMillis)}))

(defn- update-wizard! [k f & args]
  (swap! wizards update k #(apply f % args)))

(defn- drop-wizard! [k]
  (swap! wizards dissoc k))

(defn- format-date [ts]
  (let [fmt (DateTimeFormatter/ofPattern "d MMM yyyy")]
    (.format (-> (Instant/ofEpochMilli ts)
                 (.atZone (ZoneId/systemDefault))
                 .toLocalDate)
             fmt)))

(defn- stars
  "Render a score (1..5) as filled and empty stars. nil score → 'unrated'."
  [score]
  (if (nil? score)
    "unrated"
    (let [n (max 0 (min 5 score))]
      (str (apply str (repeat n "⭐"))
           (apply str (repeat (- 5 n) "☆"))))))

(defn- escape-html [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;"))))

(defn- format-meal-card
  "Pretty HTML summary of a meal (title, score, ingredients, description, meta)."
  [meal]
  (let [title (escape-html (:title meal))
        score (:score meal)
        ings (:ingredients meal)
        desc (:description meal)
        added-by (:added_by meal)
        added (:added_date meal)]
    (str "<b>🍽️ #" (:meal_id meal) " " title "</b>\n"
         (stars score) "\n"
         (when (seq ings)
           (str "\n<b>Ingredients:</b> " (escape-html (str/join ", " ings)) "\n"))
         (when (and desc (not (str/blank? desc)))
           (str "\n" (escape-html desc) "\n"))
         "\n<i>Added by " (escape-html added-by)
         " on " (format-date added) "</i>")))

(defn- send-meal-card
  "Send a meal's photo (if any) and its formatted card."
  [m meal]
  (when-let [img (photos/load-photo (:meal_id meal))]
    (msg/reply-image m img))
  (msg/reply m (format-meal-card meal)))

(defn- handle-add-start
  "User sent a photo with caption 'meal <title>'. Open the wizard."
  [m title]
  (let [k (wizard-key m)
        bs (msg/image-bytes m)]
    (cond
      (nil? bs)
      (msg/reply m "❌ Couldn't read the photo. Please try again.")

      (get @wizards k)
      (msg/reply m "⚠️ You already have a meal in progress. Type <code>cancel</code> first.")

      :else
      (do
        (start-wizard! k title bs)
        (msg/reply m (str "📸 Got the photo for <b>" (escape-html title)
                          "</b>.\nWhat score out of 5?"))))))

(defn- parse-score [s]
  (try
    (let [n (Integer/parseInt (str/trim s))]
      (when (<= 1 n 5) n))
    (catch Exception _ nil)))

(defn- split-ingredients [s]
  (->> (str/split (or s "") #",")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn- handle-wizard-reply
  "Advance the wizard for this user with their plain-text reply."
  [m text]
  (let [k (wizard-key m)
        w (get @wizards k)]
    (when w
      (case (:stage w)
        :score
        (if-let [n (parse-score text)]
          (do
            (update-wizard! k assoc :score n :stage :ingredients)
            (msg/reply m "👍 List the main ingredients (comma-separated):"))
          (msg/reply m "🤔 Score must be a number 1–5. Try again."))

        :ingredients
        (let [ings (split-ingredients text)]
          (if (empty? ings)
            (msg/reply m "🤔 I didn't catch any ingredients. Try again (comma-separated):")
            (do
              (update-wizard! k assoc :ingredients ings :stage :description)
              (msg/reply m "📝 Add a short description, or type <code>skip</code>:"))))

        :description
        (let [desc (when-not (= "skip" (str/lower-case (str/trim text)))
                     (str/trim text))
              w' (get @wizards k)
              id (db/add-meal {:title (:title w')
                               :score (:score w')
                               :description desc
                               :added-by (msg/sender m)
                               :chat-id (msg/chat-id m)})]
          (db/add-ingredients id (:ingredients w'))
          (photos/save-photo! id (:image-bytes w'))
          (drop-wizard! k)
          (msg/reply m (str "✅ Saved meal #" id ": <b>"
                            (escape-html (:title w')) "</b>.\n"
                            "Use <code>randommeal</code> or "
                            "<code>findmeal &lt;term&gt;</code> to recall it.")))))))

(defn- handle-cancel
  "User typed 'cancel' during a wizard."
  [m]
  (let [k (wizard-key m)]
    (when (get @wizards k)
      (drop-wizard! k)
      (msg/reply m "👌 Cancelled. The meal was not saved.")
      true)))

(defn- handle-meal-command
  "Dispatch :meal — start the wizard (photo+caption) or view a stored meal by id."
  [m args]
  (let [arg (str/trim (or args ""))]
    (cond
      ;; Photo + title → start wizard
      (and (msg/has-image? m) (seq arg))
      (handle-add-start m arg)

      ;; Photo but no title
      (msg/has-image? m)
      (msg/reply m "📸 I see a photo, but no title. Send the photo with a caption like <code>meal Mac &amp; Cheese</code>.")

      ;; No photo, numeric arg → view by id
      (re-matches #"\d+" arg)
      (if-let [meal (db/get-meal (Long/parseLong arg))]
        (send-meal-card m meal)
        (msg/reply m (str "🤷 No meal #" arg " found.")))

      ;; No photo, non-numeric title → prompt for photo
      (seq arg)
      (msg/reply m (str "📸 I need a photo to add this meal. "
                        "Send the photo with the caption: <code>meal " (escape-html arg) "</code>."))

      ;; No photo, no arg → help
      :else
      (msg/reply m
        (str "<b>🍽️ Meal Memories</b>\n\n"
             "• Send a photo with caption <code>meal &lt;title&gt;</code> to add a new meal "
             "(I'll then ask for score, ingredients, description).\n"
             "• <code>meal &lt;id&gt;</code> — view a saved meal\n"
             "• <code>randommeal</code> — pick one at random\n"
             "• <code>findmeal &lt;term&gt;</code> — search title / description / ingredients\n"
             "• Type <code>cancel</code> mid-add to abort.")))))

(defn- handle-randommeal [m]
  (if-let [meal (db/random-meal)]
    (send-meal-card m meal)
    (msg/reply m "🤷 No meals saved yet. Send a photo with caption <code>meal &lt;title&gt;</code> to add one.")))

(defn- format-search-line [meal]
  (str "#" (:meal_id meal) " — <b>" (escape-html (:title meal)) "</b> "
       (stars (:score meal))))

(defn- handle-findmeal [m args]
  (let [term (str/trim (or args ""))]
    (if (str/blank? term)
      (msg/reply m "🔎 Usage: <code>findmeal &lt;term&gt;</code>")
      (let [results (db/search-meals term)
            shown (take 10 results)
            extra (max 0 (- (count results) 10))]
        (if (empty? results)
          (msg/reply m (str "🔎 No meals matching \"" (escape-html term) "\"."))
          (msg/reply m
            (str "🔎 <b>Found " (count results) " meal"
                 (when (not= 1 (count results)) "s")
                 " matching \"" (escape-html term) "\":</b>\n\n"
                 (str/join "\n" (map format-search-line shown))
                 (when (pos? extra) (str "\n\n…and " extra " more."))
                 "\n\nUse <code>meal &lt;id&gt;</code> to view one.")))))))

(defmodule Meals
  :commands [:meal :meals :randommeal :findmeal]
  :receive-messages :all
  :wants-private true

  (defn process-message [m]
    (prune-stale!)
    (let [cmd (msg/command m)
          args (msg/mod-text m)
          text (msg/get-text m)
          k (wizard-key m)
          plain-text (when text (str/trim text))]
      (cond
        (= cmd :meal)        (handle-meal-command m args)
        (= cmd :meals)       (handle-meal-command m "")
        (= cmd :randommeal)  (handle-randommeal m)
        (= cmd :findmeal)    (handle-findmeal m args)

        ;; No command — only matters if a wizard is open for this user.
        (get @wizards k)
        (cond
          ;; "cancel" word during a wizard
          (and plain-text
               (= "cancel" (str/lower-case plain-text)))
          (handle-cancel m)

          ;; Otherwise advance the wizard with whatever text was sent
          (and plain-text (not (str/blank? plain-text)))
          (handle-wizard-reply m plain-text)

          :else nil)

        :else nil))))
