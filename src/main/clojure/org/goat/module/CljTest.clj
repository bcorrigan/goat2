(ns org.goat.module.CljTest (:gen-class  
          :extends org.goat.core.Module))

(import (org.goat.core Module Message BotStats Constants))

(defn -processChannelMessage
  [this m] 
  (.reply m "WOOHOO GOT INTO CLOJURE"))

(defn -processPrivateMessage
  [this m]
  (-processChannelMessage this m))

(defn -getCommands
  [this]
  (into-array String '("clojure")))
