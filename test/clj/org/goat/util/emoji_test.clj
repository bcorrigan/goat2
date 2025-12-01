(ns org.goat.util.emoji-test
  (:require [clojure.test :refer :all]
            [org.goat.util.emoji :as emoji]))

(deftest test-find-matching-emojis-single
  (testing "Single emoji matches"
    (is (= ["🥦"] (emoji/find-matching-emojis "broccoli")))
    (is (= ["🥕"] (emoji/find-matching-emojis "carrots")))
    (is (= ["🍗"] (emoji/find-matching-emojis "chicken")))
    (is (= ["🥩"] (emoji/find-matching-emojis "beef")))
    (is (= ["🍲"] (emoji/find-matching-emojis "curry")))
    (is (= ["🍲"] (emoji/find-matching-emojis "stew")))
    (is (= ["🍞"] (emoji/find-matching-emojis "bread")))
    (is (= ["🥐"] (emoji/find-matching-emojis "croissant")))))

(deftest test-find-matching-emojis-multiple
  (testing "Multiple emoji matches in compound names"
    (is (= ["🍗" "🍲"] (emoji/find-matching-emojis "chicken curry")))
    (is (= ["🥩" "🍲"] (emoji/find-matching-emojis "beef stew")))
    (is (= ["🧅" "🥕" "🍲"] (emoji/find-matching-emojis "carrot and onion soup")))))

(deftest test-find-matching-emojis-case-insensitive
  (testing "Case-insensitive matching"
    (is (= ["🥦"] (emoji/find-matching-emojis "BROCCOLI")))
    (is (= ["🥦"] (emoji/find-matching-emojis "BrOcCoLi")))
    (is (= ["🍗" "🍲"] (emoji/find-matching-emojis "Chicken Curry")))))

(deftest test-find-matching-emojis-in-context
  (testing "Matching within larger text"
    (is (= ["🥕"] (emoji/find-matching-emojis "5 bags of carrots")))
    (is (= ["🍗"] (emoji/find-matching-emojis "chicken breasts")))
    (is (= ["🥩"] (emoji/find-matching-emojis "minced beef")))
    (is (= ["🍲"] (emoji/find-matching-emojis "vegetable curry")))))

(deftest test-find-matching-emojis-no-match
  (testing "No matches return empty sequence"
    (is (empty? (emoji/find-matching-emojis "banana")))
    (is (empty? (emoji/find-matching-emojis "milk")))
    (is (empty? (emoji/find-matching-emojis "cheese")))
    (is (empty? (emoji/find-matching-emojis "")))))

(deftest test-emojify-basic
  (testing "Basic emojification with default spacing"
    (is (= "chicken curry 🍗 🍲" (emoji/emojify "chicken curry")))
    (is (= "beef stew 🥩 🍲" (emoji/emojify "beef stew")))
    (is (= "broccoli 🥦" (emoji/emojify "broccoli")))
    (is (= "banana" (emoji/emojify "banana")))))

(deftest test-emojify-custom-options
  (testing "Custom separator, prefix, and suffix"
    (is (= "chicken curry (🍗🍲)"
           (emoji/emojify "chicken curry" {:prefix " (" :suffix ")" :separator ""})))
    (is (= "beef stew [🥩 🍲]"
           (emoji/emojify "beef stew" {:prefix " [" :suffix "]"})))
    (is (= "broccoli->🥦"
           (emoji/emojify "broccoli" {:prefix "->" :suffix ""})))))

(deftest test-get-emoji
  (testing "Get single emoji"
    (is (= "🍗" (emoji/get-emoji "chicken curry")))
    (is (= "🥩" (emoji/get-emoji "beef stew")))
    (is (= "🥦" (emoji/get-emoji "broccoli")))
    (is (= "" (emoji/get-emoji "banana")))))

(deftest test-plural-forms
  (testing "Plural forms match correctly"
    (is (= ["🥕"] (emoji/find-matching-emojis "carrot")))
    (is (= ["🥕"] (emoji/find-matching-emojis "carrots")))
    (is (= ["🧅"] (emoji/find-matching-emojis "onion")))
    (is (= ["🧅"] (emoji/find-matching-emojis "onions")))
    (is (= ["🍄"] (emoji/find-matching-emojis "mushroom")))
    (is (= ["🍄"] (emoji/find-matching-emojis "mushrooms")))))

(deftest test-real-world-examples
  (testing "Real-world freezer item examples"
    (is (= "5 portions of chicken curry 🍗 🍲"
           (emoji/emojify "5 portions of chicken curry")))
    (is (= "beef and potato stew 🥩 🥔 🍲"
           (emoji/emojify "beef and potato stew")))
    (is (= "garlic bread 🧄 🍞"
           (emoji/emojify "garlic bread")))
    (is (= "frozen peas 🫛"
           (emoji/emojify "frozen peas")))))
