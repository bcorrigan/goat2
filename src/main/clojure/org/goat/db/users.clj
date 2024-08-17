(ns org.goat.db.users (:require [clojure.java.jdbc :refer :all :as sql ]
                                [clojure.edn :as edn]
                                [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/users.db"})

(def std-game-sql
  ( str " from wordlegames"
  " where type='single'"
  " and size=5"
  " and difficulty='easy' "))

(defn create-db
  "If no DB file found, create the user db and table"
  []
  (try (sql/db-do-commands db
           (if (not (util/tbl-exists? db :wordlegames))
                       (sql/create-table-ddl :wordlegames
                                         [[:type :text]
                                          [:chatid :int]
                                          [:username :text]
                                          [:won :boolean]
                                          [:guesses :int]
                                          [:size :int]
                                          [:difficulty :text]
                                          [:answer :text]
                                          [:g1 :text]
                                          [:g2 :text]
                                          [:g3 :text]
                                          [:g4 :text]
                                          [:g5 :text]
                                          [:g6 :text]
                                          [:starttime :datetime]
                                          [:endtime :datetime]
                                          ])))
       (if (not (util/tbl-exists? db :wordlegames))
         (sql/db-do-commands db
                             (sql/create-table-ddl :records
                                                   [[:username :text]
                                                    [:record :text]
                                                    [:recordval :text]
                                                    [:recordtime :datetime]]))

         (sql/execute! db "create unique index recidx on records(username, record)" ))
       (if (not (util/tbl-exists? db :users))
         (sql/db-do-commands db (sql/create-table-ddl :users
                                                      [[:username :text]
                                                       [:chatid :int]
                                                       ]))
         (sql/execute! db "create unique index usridx on users(username)" ))

       (if (not (util/tbl-exists? db :challenges))
         (sql/db-do-commands db (sql/create-table-ddl :challenges
                                                      [[:user1 :text]
                                                       [:user2 :text]
                                                       [:user1_won :boolean]
                                                       [:user2_won :boolean]
                                                       [:user1_guesses :int]
                                                       [:user2_guesses :int]
                                                       [:endtime :datetime]])))

       (catch Exception e
         (println "Fatal error" (.getMessage e)))))

(defn record-challenge-game
  "Record a challenge match outcome."
  [stats]
  (sql/insert! db :challenges {
                               :user1 (get stats :user1)
                               :user2 (get stats :user2)
                               :user1_won (get stats :user1-won)
                               :user2_won (get stats :user2-won)
                               :user1_guesses (get stats :user1-guesses)
                               :user2_guesses (get stats :user2-guesses)
                               :endtime (get stats :endtime)}))

(defn user-known?
  "True if the user is already known to us."
  [username]
  (not (empty? (query db [(format (str "select username"
                               " from users"
                               " where username='%s'") username)]))))

(defn user-add
  "Add a new user - just their string name and chatid"
  [username chatid]
  (sql/insert! db :users {
                          :username username
                          :chatid chatid
                          }))

(defn user-chat
  "Get a given user chat-key"
  [username]
  (keyword (str (get (first (query db [(format (str "select chatid"
                          " from users"
                          " where username='%s'") username)])) :chatid))))

(defn get-losttime
  "Return timestamp at which given user last lost a normal game"
  [user]
  (get (first (sql/query db [(format (str "select max(endtime) as losttime"
                                          std-game-sql
                                          " and won=false"
                                          " and username='%s'") user)])) :losttime))

(defn get-streak
  "Return the current streak for the given user."
  [user]
  (let [losttime (get-losttime user) ]
    (get (first (sql/query db [(format (str "select count(*) as streak "
                                   std-game-sql
                                   " and endtime>%d"
                                   " and username='%s'") losttime user)])) :streak)))


;; not sure how this might behave if we have non-integer record types
(defn get-record
  "Get some record from records table"
  [user record]
  (edn/read-string (get (first (sql/query db [(format (str "select recordval as record"
                          " from records"
                          " where username='%s'"
                          " and record='%s'") user (symbol record))])) :record)))

;; needs to be an upsert really
(defn save-record
  "Update the given record in records table"
  [user record recordval time]
  (sql/execute! db [(str "insert into records (username, record, recordval, recordtime) "
                     " values(?,?,?,?)"
                     " on conflict(username, record)"
                     " do update set recordval=excluded.recordval,"
                     " recordtime=excluded.recordtime") user (symbol record) recordval time  ]))

(defn get-records-set-at
  "Get all records set at time t for user"
  [user t]
  (map #(hash-map (keyword (get %1 :record)) (get %1 :recordval))
         (sql/query db [(format (str "select record,recordval
                                   from records
                                   where username='%s'
                                   and recordtime=%s;") user t)])))

(defn count-where
  "How many records for given user match given condition?"
  [cond user]
  (get (first (sql/query db [(format (str "select count(*) as count"
                                          std-game-sql
                                          " and username='%s'"
                                          " and " cond
                                          ) user)])) :count))

(defn games-won
  "How many games has given user won"
  [user]
  (count-where "won=true" user))

(defn games-lost
  "How many games has given user lost"
  [user]
  (count-where "won=false" user))

;; subquery works:
;; select count(*) from (select won from wordlegames where username='Barry' order by endtime limit 10) where won=true;

(defn count-where-limit
  "How many records match a given condition, out of the last 10 games?"
  [cond user n]
  (get (first (sql/query db [(format (str "select count(*) as count"
                                          " from (select *"
                                          std-game-sql
                                          " and username='%s'"
                                          " order by endtime desc"
                                          " limit %s)"
                                          " where %s "
                                          ) user n cond)])) :count))

(defn games-won-n
  "How many games won out of last N"
  [user n]
  (count-where-limit "won=true" user n))

(defn games-lost-n
  "How many games won out of last N"
  [user n]
  (count-where-limit "won=false" user n))

(defn results-n
  "Get results of last N games"
  [user n]
  (map (fn [x] (update x :won #(if (= 1 %) true false)))
       (sql/query db [(format (str "select won,guesses "
                                          std-game-sql
                                          " and username='%s'"
                                          " order by endtime desc"
                                          " limit %s "
                                          ) user n)])))
(defn get-guess-rate
  "Calculate the avg. guesses-to-win for last n games"
  [user n]
  (let [results-n (results-n user n)
        results-won (filter #(= true (get % :won)) results-n)
        num-results (count results-won)
        total-guesses (reduce #(+ %1 (get %2 :guesses)) 0 results-won )]
      (double (/ total-guesses num-results ))
    ))


  (defn record-wordle-game
    "Save all interesting facts about a game of wordle.
      Set any new records, streaks etc.
     Return a map indicating any new PBs."
    [chat-key match]
    (sql/insert! db :wordlegames {
                              :type (str (symbol (get match :type)))
                              :chatid (str (symbol chat-key))
                              :username (get match :user)
                              :won (get match :won)
                              :guesses (count (get match :guesses))
                              :size (get match :size)
                              :difficulty (str (symbol (get match :difficulty)))
                              :answer (get match :answer)
                              :g1 (get (get match :guesses) 0)
                              :g2 (get (get match :guesses) 1)
                              :g3 (get (get match :guesses) 2)
                              :g4 (get (get match :guesses) 3)
                              :g5 (get (get match :guesses) 4)
                              :g6 (get (get match :guesses) 5)
                              :starttime (get match :starttime)
                              :endtime (get match :endtime)
                              })
    (let [user (get match :user)
          streak (get-streak user)
          max-streak (or (get-record user :streak) 0)
          games-won-150 (games-won-n user 150)
          games-lost-150 (games-lost-n user 150)
          games-played-150 (+ games-won-150 games-lost-150)
          won-rate-150 (double (/ games-won-150 games-played-150))
          guess-rate-20 (get-guess-rate user 20)
          max-won-rate-150 (or (get-record user :won-rate-150 ) 0 )
          max-guess-rate-20 (or (get-record user :guess-rate-20) 6 ) ]
      (if (> streak max-streak)
        (save-record user :streak streak (get match :endtime)))
      (if (> won-rate-150 max-won-rate-150)
        (save-record user :won-rate-150 won-rate-150 (get match :endtime)))
      (if (< guess-rate-20 max-guess-rate-20)
        (save-record user :guess-rate-20 guess-rate-20 (get match :endtime))))
    (get-records-set-at (get match :user) (get match :endtime)))

(defn best-wordle-player
  "Who is the best at standard wordle?"
  []
  (sql/query db [(format (str "select distinct username
                                              from wordlegames
                                              where type='single'
                                              and size=5
                                              and difficulty='easy'"))]))


(defn get-stats
  "Stats for the given user: games won, games played, win ratio,
  win ratio last 10, avg guesses-to-win, avg guesses-to-win last 10,
  current streak, best ever streak."
  [user]
  { :games-won (games-won user)
     :games-won-20 (games-won-n user 20)
     :games-lost (games-lost user)
   :games-lost-20 (games-lost-n user 20)
   :games-won-150 (games-won-n user 150)
   :games-lost-150 (games-lost-n user 150)
   :guess-rate-150 (get-guess-rate user 150)
   :guess-rate-20 (get-guess-rate user 20)
     :results-150 (reverse (results-n user 150))})
