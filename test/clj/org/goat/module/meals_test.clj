(ns org.goat.module.meals-test
  "Integration tests for the Meals module via the mock-message harness.

   Each test exercises the public process-message entry point so the wiring
   between command parsing, the wizard state machine, the SQLite DB and the
   on-disk photo store is all covered end-to-end."
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as sql]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [org.goat.db.meals :as db]
            [org.goat.module.Meals :as sut]
            [org.goat.module.meals.photos :as photos]
            [org.goat.testutils.message :as msg-utils])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream File]
           [javax.imageio ImageIO]))

;; ---------------------------------------------------------------------------
;; Fixtures: per-test SQLite DB and per-test photos dir
;; ---------------------------------------------------------------------------

(def ^:private test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-meals.db"})

(defn- delete-test-db-file! []
  (let [f (File. "test/resources/test-meals.db")]
    (when (.exists f) (.delete f))))

(defn- create-fresh-tables! []
  (sql/execute! test-db "PRAGMA foreign_keys = ON")
  (sql/db-do-commands test-db
    "create table meals (
       meal_id     integer primary key autoincrement,
       title       text not null,
       score       integer,
       description text,
       added_by    text not null,
       chat_id     integer,
       added_date  integer not null
     )")
  (sql/db-do-commands test-db
    "create table meal_ingredients (
       meal_id    integer not null,
       ingredient text not null collate NOCASE,
       foreign key (meal_id) references meals(meal_id) on delete cascade
     )")
  (sql/execute! test-db "create index meal_ing_idx on meal_ingredients(ingredient)")
  (sql/execute! test-db "create index meal_ing_meal_idx on meal_ingredients(meal_id)"))

(def ^:private test-photo-dir "test/resources/test-mealpics")

(defn- delete-dir! [path]
  (let [f (io/file path)]
    (when (.exists f)
      (doseq [c (.listFiles f)] (.delete c))
      (.delete f))))

(defn- reset-wizards! []
  (reset! sut/wizards {}))

(defn- meals-fixture [f]
  (delete-test-db-file!)
  (create-fresh-tables!)
  (delete-dir! test-photo-dir)
  (reset-wizards!)
  (msg-utils/clear-replies!)
  (binding [photos/*photo-dir* test-photo-dir]
    (with-redefs [db/db test-db]
      (f)))
  (delete-test-db-file!)
  (delete-dir! test-photo-dir)
  (reset-wizards!))

(use-fixtures :each meals-fixture)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- make-jpeg-bytes
  "Generate a small valid JPEG so ImageIO/read works on it."
  []
  (let [img (BufferedImage. 32 32 BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (.setColor g Color/RED)
    (.fillRect g 0 0 32 32)
    (.dispose g)
    (let [baos (ByteArrayOutputStream.)]
      (ImageIO/write img "jpg" baos)
      (.toByteArray baos))))

(defn- photo-msg
  "Mock an incoming photo message with the given caption-as-text."
  [text opts]
  (msg-utils/mock-message
    (merge {:text text
            :image-bytes (make-jpeg-bytes)
            :chat-id 42
            :sender "alice"}
           opts)))

(defn- plain-msg
  "Mock a plain text reply (wizard answer) from the given sender."
  [text opts]
  (msg-utils/mock-message
    (merge {:text text :chat-id 42 :sender "alice"} opts)))

(defn- seed-meal!
  "Insert a meal directly into the DB and save a photo for it.
   Returns the new meal-id."
  [{:keys [title score description ingredients added-by]
    :or {added-by "alice"}}]
  (let [id (db/add-meal {:title title :score score :description description
                         :added-by added-by :chat-id 42})]
    (db/add-ingredients id (or ingredients []))
    (photos/save-photo! id (make-jpeg-bytes))
    id))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest happy-path-full-add
  (testing "Photo+caption → score → ingredients → description saves the meal"
    (msg-utils/with-clean-replies
      ;; 1) Photo with title kicks off the wizard
      (sut/process-message (photo-msg "goat, meal Mac and Cheese" {}))
      (is (msg-utils/replied-with? "score out of 5"))

      ;; 2) Valid score advances to ingredients prompt
      (sut/process-message (plain-msg "4" {}))
      (is (msg-utils/replied-with? "ingredients"))

      ;; 3) Comma-separated ingredients advances to description prompt
      (sut/process-message (plain-msg "macaroni, cheese, milk" {}))
      (is (msg-utils/replied-with? "description"))

      ;; 4) Description completes the wizard
      (sut/process-message (plain-msg "Classic comfort food" {}))
      (is (msg-utils/replied-with? "Saved meal #"))

      ;; DB row + photo file written
      (is (= 1 (db/count-meals)))
      (let [meal (first (db/search-meals "Mac"))]
        (is (= "Mac and Cheese" (:title meal)))
        (is (= 4 (:score meal)))
        (is (= "Classic comfort food" (:description meal)))
        (is (= ["macaroni" "cheese" "milk"] (:ingredients meal)))
        (is (= "alice" (:added_by meal)))
        (is (some? (photos/photo-path (:meal_id meal)))))

      ;; Wizard cleared
      (is (empty? @sut/wizards)))))

(deftest invalid-score-reprompts
  (testing "An out-of-range score keeps the wizard on the :score stage"
    (msg-utils/with-clean-replies
      (sut/process-message (photo-msg "goat, meal Tagliatelle" {}))
      (msg-utils/clear-replies!)

      (sut/process-message (plain-msg "11" {}))
      (is (msg-utils/replied-with? "1–5"))
      (is (= :score (:stage (get @sut/wizards [42 "alice"]))))

      (sut/process-message (plain-msg "notanumber" {}))
      (is (= :score (:stage (get @sut/wizards [42 "alice"]))))

      (sut/process-message (plain-msg "3" {}))
      (is (= :ingredients (:stage (get @sut/wizards [42 "alice"])))))))

(deftest cancel-aborts-wizard
  (testing "'cancel' during the wizard drops state and saves nothing"
    (msg-utils/with-clean-replies
      (sut/process-message (photo-msg "goat, meal Risotto" {}))
      (is (= 1 (count @sut/wizards)))

      (sut/process-message (plain-msg "cancel" {}))
      (is (msg-utils/replied-with? "Cancelled"))
      (is (empty? @sut/wizards))
      (is (zero? (db/count-meals))))))

(deftest timeout-prunes-stale-wizard
  (testing "Wizards older than wizard-timeout-ms are dropped on next message"
    (msg-utils/with-clean-replies
      (sut/process-message (photo-msg "goat, meal Soup" {}))
      (is (= 1 (count @sut/wizards)))

      ;; Backdate the wizard past the 10-min cutoff
      (swap! sut/wizards update-in [[42 "alice"] :started-at]
             - (* 11 60 1000))

      (msg-utils/clear-replies!)
      ;; Send some unrelated text — should not be consumed as a score reply
      (sut/process-message (plain-msg "hello there" {}))
      (is (empty? @sut/wizards))
      (is (zero? (msg-utils/reply-count))
          "Stale wizard shouldn't consume the message")
      (is (zero? (db/count-meals))))))

(deftest two-users-same-chat-independent
  (testing "Two wizards on the same chat-id but different senders advance independently"
    (msg-utils/with-clean-replies
      ;; Alice starts
      (sut/process-message (photo-msg "goat, meal Curry" {:sender "alice"}))
      ;; Bob starts
      (sut/process-message (photo-msg "goat, meal Stew" {:sender "bob"}))

      (is (= 2 (count @sut/wizards)))

      ;; Bob answers his score; Alice's wizard untouched
      (sut/process-message (plain-msg "5" {:sender "bob"}))
      (is (= :score (:stage (get @sut/wizards [42 "alice"]))))
      (is (= :ingredients (:stage (get @sut/wizards [42 "bob"]))))

      ;; Both finish
      (sut/process-message (plain-msg "3" {:sender "alice"}))
      (sut/process-message (plain-msg "rice, chicken" {:sender "alice"}))
      (sut/process-message (plain-msg "skip" {:sender "alice"}))

      (sut/process-message (plain-msg "beef, carrots" {:sender "bob"}))
      (sut/process-message (plain-msg "skip" {:sender "bob"}))

      (is (= 2 (db/count-meals)))
      (is (= #{"Curry" "Stew"}
             (set (map :title (db/search-meals ""))))))))

(deftest findmeal-searches-across-fields
  (testing "findmeal matches title, description, and ingredients case-insensitively"
    (msg-utils/with-clean-replies
      (seed-meal! {:title "Mac and Cheese" :score 4
                   :ingredients ["macaroni" "cheese" "milk"]
                   :description "Comfort"})
      (seed-meal! {:title "Beef Stew" :score 5
                   :ingredients ["beef" "carrots" "potatoes"]
                   :description "Slow cooked"})
      (msg-utils/clear-replies!)

      ;; Ingredient match
      (sut/process-message (plain-msg "goat, findmeal cheese" {}))
      (is (msg-utils/replied-with? "Mac and Cheese"))
      (is (not (msg-utils/replied-with? "Beef Stew")))
      (msg-utils/clear-replies!)

      ;; Title match
      (sut/process-message (plain-msg "goat, findmeal beef" {}))
      (is (msg-utils/replied-with? "Beef Stew"))
      (msg-utils/clear-replies!)

      ;; Case-insensitive description match
      (sut/process-message (plain-msg "goat, findmeal SLOW" {}))
      (is (msg-utils/replied-with? "Beef Stew"))
      (msg-utils/clear-replies!)

      ;; No match
      (sut/process-message (plain-msg "goat, findmeal sushi" {}))
      (is (msg-utils/replied-with? "No meals matching")))))

(deftest randommeal-returns-saved-meal-with-image
  (testing "randommeal sends both an image reply and the formatted card"
    (msg-utils/with-clean-replies
      (seed-meal! {:title "Solo Meal" :score 5 :ingredients ["foo"]})
      (msg-utils/clear-replies!)

      (sut/process-message (plain-msg "goat, randommeal" {}))
      (is (msg-utils/replied-with? "Solo Meal"))
      (is (msg-utils/replied-with-image?)))))

(deftest randommeal-empty-db
  (testing "randommeal explains there's nothing to recall when DB is empty"
    (msg-utils/with-clean-replies
      (sut/process-message (plain-msg "goat, randommeal" {}))
      (is (msg-utils/replied-with? "No meals saved")))))

(deftest meal-by-id-views-saved-meal
  (testing "'meal <id>' fetches and renders the meal with its photo"
    (msg-utils/with-clean-replies
      (let [id (seed-meal! {:title "Lasagne" :score 4
                            :ingredients ["pasta" "ragu" "bechamel"]
                            :description "Layered"})]
        (msg-utils/clear-replies!)
        (sut/process-message (plain-msg (str "goat, meal " id) {}))
        (is (msg-utils/replied-with? "Lasagne"))
        (is (msg-utils/replied-with? "Layered"))
        (is (msg-utils/replied-with-image?))))))

(deftest photo-roundtrip-bytes-match
  (testing "Saving and loading a meal photo returns the same bytes"
    (let [bs (make-jpeg-bytes)
          id (db/add-meal {:title "X" :score 3 :description nil
                           :added-by "alice" :chat-id 42})]
      (photos/save-photo! id bs)
      (is (java.util.Arrays/equals bs (photos/load-photo-bytes id))))))
