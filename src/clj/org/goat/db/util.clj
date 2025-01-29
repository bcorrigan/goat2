(ns org.goat.db.util (:require [clojure.java.jdbc :refer :all]))

(defn tbl-exists?
  "Check the given table exists"
  [db tbl]
  (not (empty? (query db ["select name
                           from sqlite_master
                           where type='table'
                           AND name=?" (name tbl)]))))
