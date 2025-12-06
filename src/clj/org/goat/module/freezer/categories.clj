(ns org.goat.module.freezer.categories
  "Generic category definitions for smart searching.
   Categories map generic terms to specific item aliases.")

;; ============================================================================
;; Category Definitions
;; ============================================================================

(def item-categories
  "Map of generic category names to their specific item aliases.
   Add new categories here to extend the search functionality."
  {"fish" ["salmon" "haddock" "bream" "brill" "coley" "trout" "sole"
           "plaice" "turbot" "sea bass" "cod"]
   "bakery" ["bagel" "bagels" "croissant" "croissants" "bread" "scone" "scones" "crumpet" "crumpets" "rolls" "roll" "bun" "buns" "muffin" "muffins" "ciabatta"]
   "dinners" ["stew" "korma" "curry" "dhal" "dahl" "pasta" "risotto" "chilli" "bolognaise" "macaroni" "haggis" "pie" "quiche" ]  })

;; ============================================================================
;; Category Lookup Functions
;; ============================================================================

(defn get-category-aliases
  "Get the list of aliases for a category name (case-insensitive).
   Returns nil if the term is not a known category."
  [search-term]
  (get item-categories (clojure.string/lower-case search-term)))

(defn is-category?
  "Check if a search term is a known category (case-insensitive)."
  [search-term]
  (boolean (get-category-aliases search-term)))

(defn expand-category
  "If search-term is a category, return its aliases. Otherwise return [search-term]."
  [search-term]
  (if-let [aliases (get-category-aliases search-term)]
    aliases
    [search-term]))
