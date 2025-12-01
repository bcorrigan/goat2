(ns org.goat.util.emoji
  "Emoji decoration utilities for adding relevant emojis to text based on content.
   Useful for enhancing display of food items, shopping lists, etc."
  (:require [clojure.string :as str]))

;; ============================================================================
;; Emoji Mapping
;; ============================================================================

(def food-emoji-list
  "Vector of [keyword emoji] pairs for food items.
   Order is preserved for consistent emoji ordering.
   Ordered by priority: specific items first, then categories."
  [;; Meat & Poultry (check first for specificity)
   ["chicken"   "🍗"]
   ["poultry"   "🍗"]
   ["turkey"    "🍗"]
   ["beef"      "🥩"]
   ["steak"     "🥩"]
   ["mince"     "🥩"]
   ["lamb"      "🥩"]

   ;; Vegetables
   ["broccoli"  "🥦"]
   ["onion"     "🧅"]
   ["onions"    "🧅"]
   ["garlic"    "🧄"]
   ["peas"      "🫛"]
   ["ginger"    "🫚"]
   ["mushroom"  "🍄‍🟫"]
   ["mushrooms" "🍄‍🟫"]
   ["aubergine" "🍆"]
   ["aubergines" "🍆"]
   ["sweetcorn" "🌽"]
   ["beetroot" "🫜"]
   ["lemon"    "🍋"]
   ["lime"     "🍋‍🟩"]
   ["banana"   "🍌"]
   ["mango"    "🥭"]
   ["pear"     "🍐"]
   ["pears"     "🍐"]
   ["cherry"    "🍒"]
   ["cherries"  "🍒"]
   ["strawberry" "🍓"]
   ["strawberries" "🍓"]
   ["peach"    "🍑"]
   ["peaches"    "🍑"]
   ["avocado"   "🥑"]
   ["avocados"  "🥑"]
   ["chili"     "🌶️"]
   ["chilli"    "🌶️"]
   ["chilies"   "🌶️"]
   ["chillies"  "🌶️"]
   ["pepper"    "🫑"]
   ["peppers"   "🫑"]
   ["carrot"    "🥕"]
   ["carrots"   "🥕"]
   ["potato"    "🥔"]
   ["potatoes"  "🥔"]
   ["tatties"   "🥔"]
   ["tattie"    "🥔"]
   ["beans"     "🫘"]
   ["tomato"    "🍅"]
   ["tomatoes"  "🍅"]
   ["pea"       "🫛"]
   ["pineapple" "🍍"]
   ["apple"     "🍎"]
   ["orange"    "🍊"]
   ["oranges"   "🍊"]
   ["blueberry" "🫐"]
   ["blueberries" "🫐"]
   ["berry" "🫐"]
   ["berries" "🫐"]
   ["melon" "🍉"]
   ["melons" "🍉"]
   ["watermelon" "🍉"]
   ["watermelons" "🍉"]
   ["grape" "🍇"]
   ["grapes" "🍇"]
   ["coconut" "🥥"]
   ["coconuts" "🥥"]
   ["olive" "🫒"]
   ["olives" "🫒"]
   ["nut" "🥜"]
   ["peanut" "🥜"]
   ["nuts" "🥜"]
   ["peanuts" "🥜"]
   ["banana" "🍌"]
   ["bananas" "🍌"]
   
   ;; Bread & Baked Goods
   ["bread"     "🍞"]
   ["loaf"      "🍞"]
   ["loaves"    "🍞"]
   ["croissant" "🥐"]
   ["croissants" "🥐"]
   ["bagel"     "🥯"]
   ["bagels"    "🥯"]
   ["pancake"   "🥞"]
   ["pancakes"  "🥞"]
   ["waffle"    "🧇"]
   ["waffles"   "🧇"]

   ;; Prepared Foods (check last, more generic)
   ["pizza"     "🍕"]
   ["chips"     "🍟"]
   ["prawn"     "🍤"]
   ["bacon"     "🥓"]
   ["prawns"    "🦐"]
   ["dumpling"  "🥟"]
   ["dumplings" "🥟"]
   ["pie"       "🥧"]
   ["ice cream" "🍨"]
   ["cake"      "🍰"]
   ["stilton"   "🧀"]
   ["cheese"    "🧀"]
   ["falafel"   "🧆"]
   ["curry"     "🍛"]
   ["stew"      "🍲"]
   ["soup"      "🍲"]
   ["casserole" "🥘"]
   ["broth"     "🍲"]])

;; ============================================================================
;; Emoji Matching Functions
;; ============================================================================

(defn find-matching-emojis
  "Find all emojis that match words in the given text.
   Returns a sequence of unique emojis found, in order of appearance.

   Examples:
   (find-matching-emojis \"chicken curry\") => (\"🍗\" \"🍲\")
   (find-matching-emojis \"beef stew\") => (\"🥩\" \"🍲\")
   (find-matching-emojis \"banana\") => ()

   Case-insensitive matching."
  [text]
  (when text
    (let [normalized-text (str/lower-case (str/trim text))
          ;; Find all matching emojis, maintaining order from food-emoji-list
          matches (keep (fn [[keyword emoji]]
                         (when (str/includes? normalized-text keyword)
                           emoji))
                       food-emoji-list)]
      ;; Return unique emojis while preserving order
      (distinct matches))))

(defn emojify
  "Add relevant emojis to text based on content matching.
   Returns a string with emojis appended.

   Options:
   - :separator - String to use between emojis (default: single space)
   - :prefix - String to add before emojis (default: single space)
   - :suffix - String to add after emojis (default: empty string)

   Examples:
   (emojify \"chicken curry\") => \"chicken curry 🍗 🍲\"
   (emojify \"beef stew\" {:prefix \" (\" :suffix \")\" :separator \"\"})
     => \"beef stew (🥩🍲)\"
   (emojify \"banana\") => \"banana\" (no matching emojis)"
  ([text]
   (emojify text {}))
  ([text {:keys [separator prefix suffix]
          :or {separator " " prefix " " suffix ""}}]
   (if-let [emojis (seq (find-matching-emojis text))]
     (str text prefix (str/join separator emojis) suffix)
     text)))

(defn get-emoji
  "Get a single emoji for the given text, or empty string if no match.
   If multiple emojis match, returns the first one.
   Useful when you only want one emoji representation."
  [text]
  (if-let [emoji (first (find-matching-emojis text))]
    emoji
    ""))
