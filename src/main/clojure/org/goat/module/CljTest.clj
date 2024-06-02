(ns org.goat.module.CljTest (:gen-class  
          :extends org.goat.core.Module))

(defn -processChannelMessage
  [_ m] 
  (.reply m "WOOHOO GOT INTO CLOJURE"))

(defn -processPrivateMessage
  [this m]
  (-processChannelMessage this m))

(defn -getCommands
  [_]
  (into-array String '("clojure")))
