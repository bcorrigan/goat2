(ns org.goat.core.command-parser-test
  (:require [clojure.test :refer :all]
            [org.goat.core.command-parser :as sut]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSE-PARAMETERS TESTS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-parse-parameters-with-params-and-text
  (testing "Parse parameters with both key=value pairs and remaining text"
    (let [result (sut/parse-parameters "dict=web1913 num=2 hello world")]
      (is (= {:dict "web1913" :num "2"} (:params result)))
      (is (= "hello world" (:text result))))))

(deftest test-parse-parameters-params-only
  (testing "Parse parameters with only key=value pairs"
    (let [result (sut/parse-parameters "dict=web1913 num=2")]
      (is (= {:dict "web1913" :num "2"} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-text-only
  (testing "Parse parameters with only text (no key=value pairs)"
    (let [result (sut/parse-parameters "hello world")]
      (is (= {} (:params result)))
      (is (= "hello world" (:text result))))))

(deftest test-parse-parameters-single-param
  (testing "Parse single parameter"
    (let [result (sut/parse-parameters "key=value")]
      (is (= {:key "value"} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-nil
  (testing "Parse nil text returns empty params and text"
    (let [result (sut/parse-parameters nil)]
      (is (= {} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-empty-string
  (testing "Parse empty string returns empty params and text"
    (let [result (sut/parse-parameters "")]
      (is (= {} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-whitespace-only
  (testing "Parse whitespace-only string returns empty params and text"
    (let [result (sut/parse-parameters "   ")]
      (is (= {} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-value-with-equals
  (testing "Parse parameter with equals sign in value (only first = is split)"
    (let [result (sut/parse-parameters "url=http://example.com?foo=bar")]
      (is (= {:url "http://example.com?foo=bar"} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-mixed-order
  (testing "Parse parameters and text in mixed order"
    (let [result (sut/parse-parameters "hello dict=web1913 world num=2 test")]
      (is (= {:dict "web1913" :num "2"} (:params result)))
      (is (= "hello world test" (:text result))))))

(deftest test-parse-parameters-extra-whitespace
  (testing "Parse parameters with extra whitespace"
    (let [result (sut/parse-parameters "  dict=web1913   num=2   hello   world  ")]
      (is (= {:dict "web1913" :num "2"} (:params result)))
      (is (= "hello world" (:text result))))))

(deftest test-parse-parameters-param-without-value
  (testing "Parse parameter with key but no value (empty string value)"
    (let [result (sut/parse-parameters "key= hello")]
      (is (= {:key ""} (:params result)))
      (is (= "hello" (:text result))))))

(deftest test-parse-parameters-numeric-values
  (testing "Parse parameters with numeric values (stored as strings)"
    (let [result (sut/parse-parameters "num=42 count=100")]
      (is (= {:num "42" :count "100"} (:params result)))
      (is (= "" (:text result))))))

(deftest test-parse-parameters-special-chars-in-text
  (testing "Parse parameters with special characters in remaining text"
    (let [result (sut/parse-parameters "dict=web1913 hello! world? test#")]
      (is (= {:dict "web1913"} (:params result)))
      (is (= "hello! world? test#" (:text result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET-PARAM TESTS ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-get-param-single-key
  (testing "Get parameter with single key"
    (is (= "web1913" (sut/get-param {:dict "web1913"} :dict)))))

(deftest test-get-param-alias-first-match
  (testing "Get parameter with alias - first key matches"
    (is (= "web1913" (sut/get-param {:dict "web1913"} :dict :dictionary)))))

(deftest test-get-param-alias-second-match
  (testing "Get parameter with alias - second key matches"
    (is (= "web1913" (sut/get-param {:dictionary "web1913"} :dict :dictionary)))))

(deftest test-get-param-alias-both-present
  (testing "Get parameter with alias - both keys present, returns first"
    (is (= "first" (sut/get-param {:dict "first" :dictionary "second"} :dict :dictionary)))))

(deftest test-get-param-not-found
  (testing "Get parameter returns nil when key not found"
    (is (nil? (sut/get-param {:other "value"} :dict)))))

(deftest test-get-param-multiple-aliases-not-found
  (testing "Get parameter with multiple aliases returns nil when none found"
    (is (nil? (sut/get-param {:other "value"} :dict :dictionary :db)))))

(deftest test-get-param-empty-map
  (testing "Get parameter from empty map returns nil"
    (is (nil? (sut/get-param {} :dict)))))

(deftest test-get-param-nil-value
  (testing "Get parameter can return nil value"
    (is (nil? (sut/get-param {:dict nil} :dict)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSE-INT TESTS ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-parse-int-valid
  (testing "Parse valid integer string"
    (is (= 42 (sut/parse-int "42" 1)))
    (is (= 0 (sut/parse-int "0" 1)))
    (is (= -100 (sut/parse-int "-100" 1)))))

(deftest test-parse-int-invalid
  (testing "Parse invalid integer string returns default"
    (is (= 1 (sut/parse-int "not-a-number" 1)))
    (is (= 99 (sut/parse-int "12.34" 99)))
    (is (= 0 (sut/parse-int "abc123" 0)))))

(deftest test-parse-int-nil
  (testing "Parse nil returns default"
    (is (= 1 (sut/parse-int nil 1)))
    (is (= 42 (sut/parse-int nil 42)))))

(deftest test-parse-int-empty-string
  (testing "Parse empty string returns default"
    (is (= 1 (sut/parse-int "" 1)))))

(deftest test-parse-int-whitespace
  (testing "Parse whitespace string returns default"
    (is (= 1 (sut/parse-int "  " 1)))))

(deftest test-parse-int-with-leading-zeros
  (testing "Parse integer with leading zeros"
    (is (= 42 (sut/parse-int "042" 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGRATION TESTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-full-parsing-workflow
  (testing "Full workflow: parse parameters, get values, parse ints"
    (let [input "dict=web1913 num=5 hello world"
          parsed (sut/parse-parameters input)
          dict (sut/get-param (:params parsed) :dict :dictionary)
          num-str (sut/get-param (:params parsed) :num :number)
          num (sut/parse-int num-str 1)
          text (:text parsed)]
      (is (= "web1913" dict))
      (is (= 5 num))
      (is (= "hello world" text)))))

(deftest test-workflow-with-missing-params
  (testing "Full workflow with missing parameters uses defaults"
    (let [input "hello world"
          parsed (sut/parse-parameters input)
          dict (sut/get-param (:params parsed) :dict :dictionary)
          num-str (sut/get-param (:params parsed) :num :number)
          num (sut/parse-int num-str 1)
          text (:text parsed)]
      (is (nil? dict))
      (is (= 1 num))  ; Default used
      (is (= "hello world" text)))))

(deftest test-workflow-with-aliases
  (testing "Full workflow using parameter aliases"
    (let [input "dictionary=moby num=3 thesaurus"
          parsed (sut/parse-parameters input)
          ;; Try :dict first, fall back to :dictionary
          dict (sut/get-param (:params parsed) :dict :dictionary)
          num-str (sut/get-param (:params parsed) :num :number)
          num (sut/parse-int num-str 1)
          text (:text parsed)]
      (is (= "moby" dict))
      (is (= 3 num))
      (is (= "thesaurus" text)))))
