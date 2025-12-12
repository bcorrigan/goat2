(ns org.goat.core.command-parser
  "Utilities for parsing command parameters from text.

  Supports key=value parameter syntax like:
    'dict=web1913 num=2 remaining text'

  Returns parsed parameters and remaining text separately."
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARAMETER PARSING ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-parameters
  "Parse command parameters like 'dict=web1913 num=2 remaining text'.

  Takes a string containing space-separated tokens. Tokens with '=' are
  treated as key=value parameters. Tokens without '=' are treated as
  remaining text.

  Returns a map with:
    :params - Map of keyword keys to string values (e.g., {:dict \"web1913\" :num \"2\"})
    :text   - Remaining text after parameters removed (e.g., \"remaining text\")

  Examples:
    (parse-parameters \"dict=web1913 num=2 hello world\")
    => {:params {:dict \"web1913\" :num \"2\"} :text \"hello world\"}

    (parse-parameters \"hello world\")
    => {:params {} :text \"hello world\"}

    (parse-parameters \"key=value\")
    => {:params {:key \"value\"} :text \"\"}

    (parse-parameters nil)
    => {:params {} :text \"\"}

    (parse-parameters \"\")
    => {:params {} :text \"\"}"
  [text]
  (if (or (nil? text) (str/blank? text))
    {:params {} :text ""}
    (let [trimmed (str/trim text)
          tokens (str/split trimmed #"\s+")
          ;; Separate key=value pairs from remaining text
          {params true words false}
          (group-by #(str/includes? % "=") tokens)
          ;; Parse key=value pairs
          param-map (reduce (fn [acc token]
                             (let [[k v] (str/split token #"=" 2)]
                               (assoc acc (keyword k) v)))
                           {}
                           params)
          ;; Remaining text
          remaining (str/join " " words)]
      {:params param-map
       :text remaining})))

(defn get-param
  "Get parameter value from params map, supporting aliases.

  Takes a params map (as returned by parse-parameters) and one or more
  keyword keys. Returns the value of the first key that exists in the map.

  This allows supporting multiple parameter names for the same value,
  such as :dict and :dictionary.

  Examples:
    (get-param {:dict \"web1913\"} :dict)
    => \"web1913\"

    (get-param {:dictionary \"web1913\"} :dict :dictionary)
    => \"web1913\"

    (get-param {:dict \"web1913\"} :dict :dictionary)
    => \"web1913\"

    (get-param {:other \"value\"} :dict :dictionary)
    => nil"
  [params & keys]
  (some params keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGER PARSING ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-int
  "Parse a string to an integer, returning default-val on error.

  Examples:
    (parse-int \"42\" 1)
    => 42

    (parse-int \"invalid\" 1)
    => 1

    (parse-int nil 1)
    => 1"
  [s default-val]
  (if (nil? s)
    default-val
    (try
      (Integer/parseInt s)
      (catch NumberFormatException e
        default-val))))
