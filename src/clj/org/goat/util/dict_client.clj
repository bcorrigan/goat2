(ns org.goat.util.dict-client
  "Pure Clojure implementation of RFC 2229 DICT protocol client"
  (:require [clojure.string :as str])
  (:import [java.net Socket InetAddress ConnectException UnknownHostException]
           [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter PrintWriter IOException]))

;; RFC 2229 Protocol Constants
(def ^:private standard-port 2628)

;; Commands
(def ^:private client-command "CLIENT")
(def ^:private define-command "DEFINE")
(def ^:private match-command "MATCH")
(def ^:private show-info-command "SHOW INFO")
(def ^:private show-databases-command "SHOW DB")
(def ^:private show-strategies-command "SHOW STRAT")
(def ^:private show-server-info-command "SHOW SERVER")
(def ^:private quit-connection-command "QUIT")

;; Response Codes
(def ^:private databases-available-response "110")
(def ^:private strategies-available-response "111")
(def ^:private database-information-response "112")
(def ^:private server-information-response "114")
(def ^:private definitions-found-response "150")
(def ^:private matches-found-response "152")
(def ^:private successful-connection-response "220")
(def ^:private command-completed-response "250")
(def ^:private invalid-database-response "550")
(def ^:private invalid-strategy-response "551")
(def ^:private no-match-response "552")
(def ^:private no-databases-available-response "554")
(def ^:private no-strategies-available-response "555")

;; Private helper functions
(defn- send-command
  "Send a command to the DICT server"
  [conn cmd]
  (.println (:writer conn) cmd))

(defn- receive-line
  "Read a single line from the DICT server"
  [conn]
  (try
    (.readLine (:reader conn))
    (catch IOException e
      (throw (RuntimeException. (str "IOException in receive: " (.getMessage e)))))))

(defn- remove-end-newlines
  "Remove trailing newlines and spaces from text"
  [text]
  (str/replace text #"[\n\s]+$" ""))

(defn- normalize-definition-text
  "Normalize definition text by replacing single newlines with spaces.
   Preserves double newlines as paragraph breaks."
  [text]
  (-> text
      ;; Replace double+ newlines with a placeholder
      (str/replace #"\n\n+" "<<<PARAGRAPH>>>")
      ;; Replace single newlines with spaces
      (str/replace #"\n" " ")
      ;; Restore paragraph breaks
      (str/replace "<<<PARAGRAPH>>>" "\n\n")
      ;; Clean up multiple spaces
      (str/replace #" +" " ")
      ;; Trim
      str/trim))

(defn- remove-quotation-marks
  "Remove leading and trailing quotation marks if present"
  [text]
  (let [trimmed (str/trim text)
        len (count trimmed)]
    (if (and (> len 1)
             (= \" (first trimmed))
             (= \" (last trimmed)))
      (subs trimmed 1 (dec len))
      trimmed)))

;; Public API

(defn connect
  "Establish connection to DICT server.
   Returns connection map: {:socket :reader :writer :message-id :capabilities :short-server-info}
   Throws UnknownHostException or ConnectException on failure"
  ([host] (connect host standard-port))
  ([host port]
   (let [address (try
                   (InetAddress/getByName host)
                   (catch UnknownHostException e
                     (throw (UnknownHostException. (str "Could not resolve host " host)))))
         socket (try
                  (Socket. address port)
                  (catch IOException e
                    (throw (ConnectException. (str "Could not connect to " address ":" port)))))
         reader (BufferedReader.
                 (InputStreamReader. (.getInputStream socket) "UTF-8"))
         writer (PrintWriter.
                 (BufferedWriter.
                  (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))
                 true)]
     ;; Read initial banner (RFC 2229 section 3.1)
     (let [banner (.readLine reader)
           response-code (subs banner 0 3)]
       (if-not (= response-code successful-connection-response)
         (do
           (.close socket)
           (throw (ConnectException. (str "Error connecting to server: " banner))))
         (let [last-bracket (.lastIndexOf banner "<")
               message-id (subs banner last-bracket)
               first-bracket (.indexOf banner "<")
               capabilities (str/trim (subs banner first-bracket last-bracket))
               short-server-info (str/trim (subs banner 4 first-bracket))]
           ;; Send client identification
           (.println writer (str client-command " "))
           (.readLine reader) ; Consume response
           {:socket socket
            :reader reader
            :writer writer
            :message-id message-id
            :capabilities capabilities
            :short-server-info short-server-info}))))))

(defn close
  "Close connection to DICT server"
  [conn]
  (send-command conn quit-connection-command)
  (try
    (.close (:socket conn))
    (catch IOException e
      (println "Socket closing was unsuccessful"))))

(defn get-databases
  "Get list of available databases from server.
   Returns vector of maps: [{:short \"web1913\" :long \"Webster's Revised...\"}]"
  [conn]
  (send-command conn show-databases-command)
  (let [response-code (receive-line conn)]
    (cond
      (nil? response-code)
      (let [new-conn (connect (:short-server-info conn))]
        (get-databases new-conn))

      (str/starts-with? response-code databases-available-response)
      (loop [line (receive-line conn)
             dbs []]
        (if (= line ".")
          (do
            (receive-line conn) ; Consume completion code
            dbs)
          (let [space-pos (.indexOf line " ")
                short-name (subs line 0 space-pos)
                long-name (str/trim (subs line space-pos))]
            (recur (receive-line conn)
                   (conj dbs {:short short-name :long long-name})))))

      (str/starts-with? response-code no-databases-available-response)
      []

      :else
      (throw (ConnectException. (str "Error connecting to server: " response-code))))))

(defn get-definitions
  "Get definitions for a word from specified databases.
   databases: vector of database short names, or [\"*\"] for all
   word: the word to define
   Returns vector of maps: [{:database-short :database-long :word :definition}]"
  [conn databases word]
  (let [word (remove-quotation-marks word)]
    (loop [dbs databases
           all-defs []]
      (if (empty? dbs)
        all-defs
        (let [db (first dbs)]
          (send-command conn (str define-command " " db " \"" word "\""))
          (let [response (receive-line conn)]
            (cond
              (nil? response)
              (let [new-conn (connect (:short-server-info conn))]
                (get-definitions new-conn databases word))

              (str/starts-with? response no-match-response)
              (recur (rest dbs) all-defs)

              (str/starts-with? response definitions-found-response)
              (let [new-defs
                    (loop [line (receive-line conn)
                           defs all-defs]
                      (if (str/starts-with? line command-completed-response)
                        defs  ; Exit inner loop, return accumulated definitions
                        (let [;; Parse definition header
                              line (str/trim (subs line (inc (.indexOf line "\""))))
                              returned-word (str/trim (subs line 0 (.indexOf line "\"")))
                              line (str/trim (subs line (inc (.indexOf line "\""))))
                              dict (subs line 0 (.indexOf line " "))
                              description (str/trim (subs line (.indexOf line " ")))
                              ;; Read definition text
                              def-text (loop [def-line (receive-line conn)
                                             text []]
                                        (if (and (str/starts-with? def-line ".")
                                                 (not (str/starts-with? def-line "..")))
                                          (normalize-definition-text (str/join "\n" text))
                                          (recur (receive-line conn)
                                                 (conj text (if (str/starts-with? def-line "..")
                                                             (subs def-line 1)
                                                             def-line)))))]
                          (recur (receive-line conn)
                                 (conj defs {:database-short dict
                                            :database-long description
                                            :word returned-word
                                            :definition def-text})))))]
                (recur (rest dbs) new-defs))

              (str/starts-with? response invalid-database-response)
              (throw (IllegalArgumentException. (str "Invalid database: " db)))

              :else
              (throw (ConnectException. (str "Error connecting to server: " response))))))))))

(defn get-matches
  "Get spelling matches for a word from specified databases.
   databases: vector of database short names, or [\"*\"] for all
   strategy: matching strategy name, or \".\" for default
   word: the word to match
   Returns vector of maps: [{:database :match}]"
  [conn databases strategy word]
  (let [word (remove-quotation-marks word)]
    (loop [dbs databases
           all-matches []]
      (if (empty? dbs)
        all-matches
        (let [db (first dbs)]
          (send-command conn (str match-command " " db " " strategy " \"" word "\""))
          (let [response (receive-line conn)]
            (cond
              (nil? response)
              (let [new-conn (connect (:short-server-info conn))]
                (get-matches new-conn databases strategy word))

              (str/starts-with? response matches-found-response)
              (let [trimmed (str/trim (subs response (.indexOf response " ")))
                    num-matches (Integer/parseInt (subs trimmed 0 (.indexOf trimmed " ")))
                    matches (loop [i 0
                                  results []]
                             (if (= i num-matches)
                               results
                               (let [line (receive-line conn)
                                     db-name (subs line 0 (.indexOf line " "))
                                     match (str/trim (subs line (.indexOf line " ")))]
                                 (recur (inc i)
                                        (conj results {:database db-name :match match})))))]
                ;; Consume "." and completion code
                (receive-line conn)
                (receive-line conn)
                (recur (rest dbs) (into all-matches matches)))

              (str/starts-with? response no-match-response)
              (recur (rest dbs) all-matches)

              (str/starts-with? response invalid-database-response)
              (throw (IllegalArgumentException. (str "Invalid database: " db)))

              (str/starts-with? response invalid-strategy-response)
              (throw (IllegalArgumentException. (str "Invalid strategy: " strategy)))

              :else
              (throw (ConnectException. (str "Error connecting to server: " response))))))))))

(defn get-strategies
  "Get list of available matching strategies from server.
   Returns vector of maps: [{:name :description}]"
  [conn]
  (send-command conn show-strategies-command)
  (let [response (receive-line conn)]
    (cond
      (nil? response)
      (let [new-conn (connect (:short-server-info conn))]
        (get-strategies new-conn))

      (str/starts-with? response strategies-available-response)
      (loop [line (receive-line conn)
             strats []]
        (if (= line ".")
          (do
            (receive-line conn) ; Consume completion code
            strats)
          (let [space-pos (.indexOf line " ")
                name (subs line 0 space-pos)
                description (subs line (inc space-pos))]
            (recur (receive-line conn)
                   (conj strats {:name name :description description})))))

      (str/starts-with? response no-strategies-available-response)
      []

      :else
      (throw (ConnectException. (str "Error connecting to server: " response))))))

(defn get-db-info
  "Get detailed information about a specific database.
   Returns string with database information"
  [conn database]
  (send-command conn (str show-info-command " " database))
  (let [response (receive-line conn)]
    (cond
      (nil? response)
      (let [new-conn (connect (:short-server-info conn))]
        (get-db-info new-conn database))

      (str/starts-with? response database-information-response)
      (loop [line (receive-line conn)
             info []]
        (if (= line ".")
          (do
            (receive-line conn) ; Consume completion code
            (normalize-definition-text (str/join "\n" info)))
          (recur (receive-line conn)
                 (conj info line))))

      (str/starts-with? response invalid-database-response)
      (throw (IllegalArgumentException. (str "Invalid database: " database)))

      :else
      (throw (ConnectException. (str "Error connecting to server: " response))))))

(defn get-server-info
  "Get detailed server information.
   Returns string with full server information"
  [conn]
  (send-command conn show-server-info-command)
  (let [response (receive-line conn)]
    (cond
      (nil? response)
      (let [new-conn (connect (:short-server-info conn))]
        (get-server-info new-conn))

      (str/starts-with? response server-information-response)
      (loop [line (receive-line conn)
             info []]
        (if (= line ".")
          (do
            (receive-line conn) ; Consume completion code
            (normalize-definition-text (str/join "\n" info)))
          (recur (receive-line conn)
                 (conj info line))))

      :else
      (throw (ConnectException. (str "Error connecting to server: " response))))))
