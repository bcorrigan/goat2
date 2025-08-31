(ns org.goat.db.users-test
  (:require [org.goat.db.users :as sut]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as sql])
  (:import [java.io File]))

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-users.db"})

(defn setup-test-db []
  (let [db-file (File. "test/resources/test-users.db")]
    (when (.exists db-file) (.delete db-file)))
  
  (sql/db-do-commands test-db
    (sql/create-table-ddl :wordlegames
      [[:type :text]
       [:chatid :int]
       [:username :text]
       [:won :boolean]
       [:guesses :int]
       [:size :int]
       [:difficulty :text]
       [:answer :text]
       [:g1 :text] [:g2 :text] [:g3 :text] [:g4 :text] [:g5 :text] [:g6 :text]
       [:starttime :datetime]
       [:endtime :datetime]]))
  
  (sql/db-do-commands test-db
    (sql/create-table-ddl :challenges
      [[:user1 :text]
       [:user2 :text]
       [:user1_won :boolean]
       [:user2_won :boolean]
       [:user1_guesses :int]
       [:user2_guesses :int]
       [:endtime :datetime]]))
  
  (sql/db-do-commands test-db
    (sql/create-table-ddl :users
      [[:username :text]
       [:chatid :int]])))

(defn teardown-test-db []
  (let [db-file (File. "test/resources/test-users.db")]
    (when (.exists db-file) (.delete db-file))))

(defn db-fixture [f]
  (setup-test-db)
  (with-redefs [sut/db test-db]
    (f))
  (teardown-test-db))

(use-fixtures :each db-fixture)

(defn seed-challenge-data []
  (let [now (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)]
    
    ;; Alice vs Bob scenarios
    (sql/insert! test-db :challenges
      {:user1 "alice" :user2 "bob" :user1_won true :user2_won false 
       :user1_guesses 4 :user2_guesses 5 :endtime (- now day-ms)})
    
    (sql/insert! test-db :challenges  
      {:user1 "bob" :user2 "alice" :user1_won true :user2_won false
       :user1_guesses 3 :user2_guesses 6 :endtime (- now (* 2 day-ms))})
    
    ;; Bug scenario: same guesses, different outcomes
    (sql/insert! test-db :challenges
      {:user1 "alice" :user2 "bob" :user1_won true :user2_won false
       :user1_guesses 6 :user2_guesses 6 :endtime (- now (* 3 day-ms))})
    
    ;; True draw: both won, same guesses  
    (sql/insert! test-db :challenges
      {:user1 "alice" :user2 "bob" :user1_won true :user2_won true
       :user1_guesses 5 :user2_guesses 5 :endtime (- now (* 4 day-ms))})
    
    ;; True draw: both lost, same guesses
    (sql/insert! test-db :challenges
      {:user1 "bob" :user2 "alice" :user1_won false :user2_won false
       :user1_guesses 6 :user2_guesses 6 :endtime (- now (* 5 day-ms))})
    
    ;; Old data beyond 7 days (should be excluded)
    (sql/insert! test-db :challenges
      {:user1 "alice" :user2 "bob" :user1_won true :user2_won false
       :user1_guesses 3 :user2_guesses 4 :endtime (- now (* 10 day-ms))})))

(deftest test-count-recent-wins
  (testing "Counts wins correctly including same guesses with different outcomes"
    (seed-challenge-data)
    
    (let [alice-wins (sut/count-recent-wins "alice" "bob" 7)
          bob-wins (sut/count-recent-wins "bob" "alice" 7)]
      
      ;; Alice should have 2 wins: 4vs5 and 6vs6(won vs lost)  
      (is (= 2 alice-wins) "Alice should have 2 wins including equal guess win")
      
      ;; Bob should have 1 win: the 3vs6 game where alice had more guesses
      (is (= 1 bob-wins) "Bob should have 1 win"))))

(deftest test-count-recent-draws  
  (testing "Only counts true draws where both players have same outcome and guesses"
    (seed-challenge-data)
    
    (let [draws (sut/count-recent-draws "alice" "bob" 7)]
      
      ;; Should only count 2 draws: both-won-5vs5 and both-lost-6vs6
      ;; Should NOT count alice-won-6vs6-bob-lost
      (is (= 2 draws) "Should only count 2 true draws"))))

(deftest test-challenge-stats-comprehensive
  (testing "Win/draw counts sum correctly for comprehensive stats"
    (seed-challenge-data)
    
    (let [alice-wins (sut/count-recent-wins "alice" "bob" 7)
          bob-wins (sut/count-recent-wins "bob" "alice" 7)
          draws (sut/count-recent-draws "alice" "bob" 7)
          total-games 5] ; 5 games in last 7 days
      
      (is (= total-games (+ alice-wins bob-wins draws))
          "All games should be accounted for: wins + draws = total"))))