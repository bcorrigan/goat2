(ns org.goat.db.users (:require [clojure.java.jdbc :refer :all :as sql ]
                                [clojure.edn :as edn]
                                [org.goat.db.util :as util]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/users.db"})

(def std-game-sql
  "Standard game select condition common to many queries"
  " from wordlegames
    where type='single'
    and size=5
    and difficulty='easy' ")

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
       (when-not (util/tbl-exists? db :wordlegames) (sql/db-do-commands db (sql/create-table-ddl :records [[:username :text] [:record :text] [:recordval :text] [:recordtime :datetime]])) (sql/execute! db "create unique index recidx on records(username, record)"))
       (when-not (util/tbl-exists? db :users) (sql/db-do-commands db (sql/create-table-ddl :users [[:username :text] [:chatid :int]])) (sql/execute! db "create unique index usridx on users(username)"))

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

(defn audit-challenge-game
  "Record a challenge match outcome."
  [p1 p2 p1-guesses p2-guesses p1-won p2-won]
  (sql/insert! db :challenges {
                               :user1 p1
                               :user2 p2
                               :user1_won p1-won
                               :user2_won p2-won
                               :user1_guesses p1-guesses
                               :user2_guesses p2-guesses
                               :endtime (System/currentTimeMillis)}))

(defn user-known?
  "True if the user is already known to us."
  [username]
  (-> (sql/query db ["select username
                     from users
                     where username=? collate NOCASE" username])
      first
      :username ))

(defn user-add
  "Upsert the given user in users table"
  [username chatid]
  (sql/execute! db ["insert into users (username, chatid)
                     values(?,?)
                     on conflict(username)
                     do update set chatid=excluded.chatid" username chatid]))

(defn user-chat
  "Get a given user chat"
  [username]
  (-> (query db [ "select chatid
                   from users
                   where username=? collate NOCASE" username])
      first
      :chatid
      long))

(defn get-losttime
  "Return timestamp at which given user last lost a normal game"
  [user]
  (-> (sql/query db [(str "select max(endtime) as losttime"
                           std-game-sql
                         " and won=false
                           and username=?") user])
      first
      :losttime))

(defn get-streak
  "Return the current streak for the given user."
  [user]
  (let [losttime (get-losttime user) ]
    (-> (sql/query db [(str "select count(*) as streak "
                                   std-game-sql
                                   " and endtime>?
                                     and username=?") losttime user])
    first
    :streak)))


;; not sure how this might behave if we have non-integer record types
(defn get-record
  "Get some record from records table"
  [user record]
  (-> (sql/query db ["select recordval as record
                         from records
                         where username=?
                         and record=?" user (symbol record)])
     first
     :record
     edn/read-string))

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
         (sql/query db ["select record,recordval
                                   from records
                                   where username=?
                                   and recordtime=?" user t])))

(defn count-where
  "How many records for given user match given condition?"
  [cond user]
  (-> (sql/query db [(str "select count(*) as count"
                           std-game-sql
                          " and username=?"
                          " and " cond) user])
      first
      :count))

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
  (-> (sql/query db [(str "select count(*) as count
                           from (select *"
                           std-game-sql
                          " and username=?
                            order by endtime desc
                            limit ?)
                            where " cond ) user n])
      first
      :count))

(defn games-won-n
  "How many games won out of last N"
  [user n]
  (count-where-limit "won=true" user n))

(defn games-lost-n
  "How many games won out of last N"
  [user n]
  (count-where-limit "won=false" user n))

(defn count-recent-wins
  "How many games were won out of last N days by player A vs player B.
   This is only for p1 vs p2 where p1 wins - it doesn't count if p1 is 2nd in the table"
  [winner loser days]
  (let [days-param (str "-" days " days")]
        (+
         (-> (sql/query db [ "select count(*) as count
                                from challenges
                                where lower(user1)=lower(?)
                                and lower(user2)=lower(?)
                                and user1_won=true
                                and user1_guesses<user2_guesses
                                and datetime(endtime / 1000, 'unixepoch') > datetime('now', ?)" winner loser days-param])
             first
             :count)
         (-> (sql/query db [ "select count(*) as count
                                 from challenges
                                 where lower(user1)=lower(?)
                                 and lower(user2)=lower(?)
                                 and user2_won=true
                                 and user2_guesses<user1_guesses
                                 and datetime(endtime / 1000, 'unixepoch') > datetime('now', ?)" loser winner days-param])
             first
             :count))))

(defn count-recent-draws
  "How many games were drawn out of last N days by player A vs player B."
  [p1 p2 days]
  (let [days-param (str "-" days " days")]
    (+
     (-> (sql/query db [ "select count(*) as count
                                from challenges
                                where lower(user1)=lower(?)
                                and lower(user2)=lower(?)
                                and user1_guesses=user2_guesses
                                and datetime(endtime / 1000, 'unixepoch') > datetime('now', ?)" p1 p2 days-param])
         first
         :count)
     (-> (sql/query db [ "select count(*) as count
                                 from challenges
                                 where lower(user1)=lower(?)
                                 and lower(user2)=lower(?)
                                 and user2_guesses=user1_guesses
                                 and datetime(endtime / 1000, 'unixepoch') > datetime('now', ?)" p2 p1 days-param])
         first
         :count))))

(defn results-n
  "Get results of last N games"
  [user n]
  (map (fn [x] (update x :won #(if (= 1 %) true false)))
       (sql/query db [(str "select won,guesses "
                           std-game-sql
                           " and username=?
                             order by endtime desc
                             limit ? ") user n])))
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
                              :type (str (symbol (match :type)))
                              :chatid (str (symbol chat-key))
                              :username (match :user)
                              :won (match :won)
                              :guesses (count (match :guesses))
                              :size (match :size)
                              :difficulty (str (symbol (match :difficulty)))
                              :answer (match :answer)
                              :g1 (get (match :guesses) 0)
                              :g2 (get (match :guesses) 1)
                              :g3 (get (match :guesses) 2)
                              :g4 (get (match :guesses) 3)
                              :g5 (get (match :guesses) 4)
                              :g6 (get (match :guesses) 5)
                              :starttime (match :starttime)
                              :endtime (match :endtime)
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
        (save-record user :streak streak (match :endtime)))
      (if (> won-rate-150 max-won-rate-150)
        (save-record user :won-rate-150 won-rate-150 (match :endtime)))
      (if (< guess-rate-20 max-guess-rate-20)
        (save-record user :guess-rate-20 guess-rate-20 (get match :endtime))))
    (get-records-set-at (match :user) (match :endtime)))

(defn best-wordle-player
  "Who is the best at standard wordle?"
  []
  (sql/query db ["select distinct username
                  from wordlegames
                  where type='single'
                  and size=5
                  and difficulty='easy'"]))


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
