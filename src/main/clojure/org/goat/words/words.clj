(ns org.goat.words.words
  (:require [clojure.java.jdbc :refer :all])
  (:gen-class))

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/words.db"})

(defn process-file-by-lines
  "Process file reading it line-by-line"
  ([file process-fn]
   (with-open [rdr (clojure.java.io/reader file)]
     (doseq [line (line-seq rdr)]
       (process-fn line)))))

(defn insert-def-line!
  "Take line from words-with-defs, break into word/length/def, write to db"
  [transaction line]
  (let [parts (clojure.string/split line #"\t")
        word (get parts 0)
        def (get parts 1)
        length (count word)]
    (insert! transaction :defs {:word word :length length :definition def})))

(defn insert-rank-line!
  "Take line from words-googlehits-ranks, split,, write to db"
  [transaction line]
  (let [parts (clojure.string/split line #"\t")
        word (clojure.string/upper-case (get parts 0))
        hit (get parts 1)
        rank (get parts 2)
        length (count word)]
    (insert! transaction :ranks {:word word :hits hit :rank rank :length length})))

(defn create-db
  "If not DB file found, we create db and table"
  []
  (try (db-do-commands db
                       (create-table-ddl :defs
                                         [[:word :text]
                                          [:length :int]
                                          [:definition :text]]))
       (catch Exception e
         (println "Cannot create defs table" (.getMessage e))))
  (try (db-do-commands db
                       (create-table-ddl :ranks
                                         [[:word :text]
                                          [:hits :int]
                                          [:rank :int]
                                          [:length :int]]))
       (catch Exception e
         (println "Fatal error" (.getMessage e))))

  ;; Load words file
  (with-db-transaction [transaction db]
    (process-file-by-lines "resources/words_with_defs" (partial insert-def-line! transaction)))

  (with-db-transaction [transaction db]
    (process-file-by-lines "resources/words-googlehits-rank" (partial insert-rank-line! transaction))))

(defn tbl-exists?
  "Check the given table exists"
  [tbl]
  (not (empty? (query db [(format "select name from sqlite_master where type='table' AND name='%s'" tbl)]))))

(defn get-word
  "Get a random word. Provide argument :hard to get a hard word, :easy to get an easy word."
  ([] (get-word :normal))
  ([difficulty]
   (let [hits (cond
                (= :normal difficulty) "and r.hits>100000"
                (= :hard difficulty) "and r.hits<100000"
                (= :easy difficulty) "and r.hits>2000000")]
     (query db [(format (str "select d.word,d.definition,r.hits"
                             " from defs d, ranks r"
                             " where d.word=r.word"
                             " and d.length=5"
                             " %s"
                             " order by random()"
                             " limit 1") hits)]))))

(defn real-word?
  "Check the given word is in the defs dictionary."
  [word]
  (not (empty? (query db [(format "select * from defs where word='%s' limit 1"
                                  (clojure.string/upper-case word))]))))
