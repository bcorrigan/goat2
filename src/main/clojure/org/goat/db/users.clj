(ns org.goat.db.users (:require [clojure.java.jdbc :refer :all]
                                [clojure.edn :as edn]))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/users.db"})

(defn create-db
  "If no DB file found, create the user db and table"
  []
  (try (db-do-commands db
                       (create-table-ddl :wordlegames
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
                                          ]))
       (db-do-commands db
                       (create-table-ddl :records
                                         [[:username :text]
                                          [:record :text]
                                          [:recordval :text]
                                          [:recordtime :datetime]]))

       (execute! db "create unique index recidx on records(username, record)" )
       (catch Exception e
         (println "Fatal error" (.getMessage e)))))

(defn get-losttime
  "Return timestamp at which given user last lost a normal game"
  [user]
  (get (first (query db [(format (str "select max(endtime) as losttime"
                                      " from wordlegames"
                                      " where won=false"
                                      " and type='single'"
                                      " and size=5"
                                      " and difficulty='easy'"
                                      " and username='%s'") user)])) :losttime))

(defn get-streak
  "Return the current streak for the given user."
  [user]
  (let [losttime (get-losttime user) ]
    (get (first (query db [(format (str "select count(*) as streak "
                                   " from wordlegames"
                                   " where won=true"
                                   " and type='single'"
                                   " and size=5"
                                   " and difficulty='easy' "
                                   " and endtime>%d"
                                   " and username='%s'") losttime user)])) :streak)))


;; not sure how this might behave if we have non-integer record types
(defn get-record
  "Get some record from records table"
  [user record]
  (edn/read-string (get (first (query db [(format (str "select recordval as record"
                          " from records"
                          " where username='%s'"
                          " and record='%s'") user (symbol record))])) :record)))

;; needs to be an upsert really
(defn save-record
  "Update the given record in records table"
  [user record recordval time]
  (execute! db [(str "insert into records (username, record, recordval, recordtime) "
                     " values(?,?,?,?)"
                     " on conflict(username, record)"
                     " do update set recordval=excluded.recordval,"
                     " recordtime=excluded.recordtime") user record recordval time  ]))

(defn record-wordle-game
  "Save all interesting facts about a game of wordle."
  [chat-key match]
  (insert! db :wordlegames {
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
        max-streak (or (get-record user :streak) 0)]
    (if (> streak max-streak)
      (save-record user :streak streak (get match :endtime))
    )))

(defn best-wordle-player
  "Who is the best at standard wordle?"
  []
  (query db [(format (str "select distinct username
                                              from wordlegames
                                              where type='single'
                                              and size=5
                                              and difficulty='easy'"))]))
