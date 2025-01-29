(ns org.goat.db.util (:require [clojure.java.jdbc :refer :all]))

(defn tbl-exists?
  "Check the given table exists"
  [db tbl]
  (seq (query db ["select name\n                           from sqlite_master\n                           where type='table'\n                           AND name=?" (name tbl)])))
