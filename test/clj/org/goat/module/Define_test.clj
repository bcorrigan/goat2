(ns org.goat.module.Define-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [org.goat.module.Define :as sut]
            [org.goat.testutils.message :as msg-utils]))

;; Test Fixture
(use-fixtures :each
  (fn [f]
    (msg-utils/clear-replies!)
    (f)
    (msg-utils/clear-replies!)))

;; OED Command Tests

(deftest test-oed-basic
  (testing "OED command generates correct URL"
    (let [m (msg-utils/mock-command-message "oed" "hello")]
      (sut/-processChannelMessage nil m)
      (is (= 1 (msg-utils/reply-count)))
      (is (msg-utils/replied-with? "dictionary.oed.com"))
      (is (msg-utils/replied-with? "queryword=hello")))))

(deftest test-oed-with-spaces
  (testing "OED command handles words with spaces"
    (let [m (msg-utils/mock-command-message "oed" "hello world")]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "queryword=hello%20world")))))

(deftest test-oed-no-word
  (testing "OED command requires a word"
    (let [m (msg-utils/mock-command-message "oed" nil)]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "look up what")))))

;; Randef Command Tests

(deftest test-randef
  (testing "Randef command shows not implemented message"
    (let [m (msg-utils/mock-command-message "randef" nil)]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "Not implemented")))))

;; Dictionaries Command Tests

(deftest test-dictionaries
  (testing "Dictionaries command lists available dictionaries"
    (let [m (msg-utils/mock-command-message "dictionaries" nil)]
      (sut/-processChannelMessage nil m)
      (is (= 1 (msg-utils/reply-count)))
      ;; Should list dictionaries from DICT server
      (is (msg-utils/replied-with? "dictionaries")))))

;; Dictionary Command Tests

(deftest test-dictionary-no-argument
  (testing "Dictionary command requires an argument"
    (let [m (msg-utils/mock-command-message "dictionary" nil)]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "which dictionary")))))

(deftest test-dictionary-invalid
  (testing "Dictionary command handles invalid dictionary gracefully"
    (let [m (msg-utils/mock-command-message "dictionary" "invalid-dict-xyz")]
      (sut/-processChannelMessage nil m)
      ;; Should show "not found" and list available dictionaries
      (is (or (msg-utils/replied-with? "not found")
              (msg-utils/replied-with? "available dictionaries"))))))

;; Define Command Tests

(deftest test-define-basic
  (testing "Define command works with a simple word"
    (let [m (msg-utils/mock-command-message "define" "test")]
      (sut/-processChannelMessage nil m)
      ;; Should get at least one reply (either definition or error)
      (is (>= (msg-utils/reply-count) 1)))))

(deftest test-define-monster
  (testing "Define command works with 'monster'"
    (let [m (msg-utils/mock-command-message "define" "monster")]
      (sut/-processChannelMessage nil m)
      ;; Should get at least one reply
      (is (>= (msg-utils/reply-count) 1))
      ;; Should not have an error about EmptyList
      (is (not (some #(and (= :text (:type %))
                          (clojure.string/includes? (:content %) "EmptyList"))
                    (msg-utils/get-replies)))))))

(deftest test-define-no-word-no-fallback
  (testing "Define command without word and no WordGame fallback"
    (let [m (msg-utils/mock-command-message "define" nil)]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "define what")))))

(deftest test-define-negative-number
  (testing "Define command rejects negative number parameter"
    (let [m (msg-utils/mock-command-message "define" "num=-1 test")]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "funny"))
      (is (msg-utils/replied-with? "cubicle")))))

(deftest test-define-zero-number
  (testing "Define command rejects zero number parameter"
    (let [m (msg-utils/mock-command-message "define" "num=0 test")]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "funny")))))

(deftest test-define-with-dictionary-param
  (testing "Define command accepts dictionary parameter"
    (let [m (msg-utils/mock-command-message "define" "dict=gcide test")]
      (sut/-processChannelMessage nil m)
      ;; Should either succeed or show error about dictionary
      (is (>= (msg-utils/reply-count) 1)))))

(deftest test-define-with-number-param
  (testing "Define command accepts number parameter"
    (let [m (msg-utils/mock-command-message "define" "num=1 test")]
      (sut/-processChannelMessage nil m)
      (is (>= (msg-utils/reply-count) 1)))))

(deftest test-define-invalid-dictionary
  (testing "Define command handles invalid dictionary parameter"
    (let [m (msg-utils/mock-command-message "define" "dict=invalid-xyz-dict test")]
      (sut/-processChannelMessage nil m)
      ;; Should show error about invalid dictionary
      (is (msg-utils/replied-with? "not a valid dictionary")))))

(deftest test-define-number-too-high
  (testing "Define command handles number parameter greater than available definitions"
    (let [m (msg-utils/mock-command-message "define" "num=999 test")]
      (sut/-processChannelMessage nil m)
      ;; Should either show "don't have X definitions" or succeed if there are that many
      (is (>= (msg-utils/reply-count) 1)))))

;; Thesaurus Command Tests

(deftest test-thesaurus-basic
  (testing "Thesaurus command works with a simple word"
    (let [m (msg-utils/mock-command-message "thesaurus" "test")]
      (sut/-processChannelMessage nil m)
      ;; Should get at least one reply
      (is (>= (msg-utils/reply-count) 1)))))

(deftest test-thesaurus-no-word
  (testing "Thesaurus command without word shows error"
    (let [m (msg-utils/mock-command-message "thesaurus" nil)]
      (sut/-processChannelMessage nil m)
      (is (msg-utils/replied-with? "define what")))))

;; Integration Tests

(deftest test-all-commands-respond
  (testing "All six commands respond without crashing"
    (doseq [cmd ["define" "thesaurus" "dictionaries" "dictionary" "oed" "randef"]]
      (msg-utils/clear-replies!)
      (let [args (cond
                   (contains? #{"define" "thesaurus" "oed"} cmd) "test"
                   (= cmd "dictionary") "oed"
                   :else nil)
            m (msg-utils/mock-command-message cmd args)]
        (sut/-processChannelMessage nil m)
        (is (>= (msg-utils/reply-count) 1)
            (str "Command " cmd " should send at least one reply"))))))

(deftest test-parameter-aliases
  (testing "Parameter aliases work correctly"
    (testing "dict and dictionary are equivalent"
      (msg-utils/clear-replies!)
      (let [m1 (msg-utils/mock-command-message "define" "dict=gcide test")
            _ (sut/-processChannelMessage nil m1)
            replies1 (msg-utils/get-replies)]
        (msg-utils/clear-replies!)
        (let [m2 (msg-utils/mock-command-message "define" "dictionary=gcide test")
              _ (sut/-processChannelMessage nil m2)
              replies2 (msg-utils/get-replies)]
          ;; Both should behave the same way
          (is (= (count replies1) (count replies2))))))

    (testing "num and number are equivalent"
      (msg-utils/clear-replies!)
      (let [m1 (msg-utils/mock-command-message "define" "num=1 test")
            _ (sut/-processChannelMessage nil m1)
            replies1 (msg-utils/get-replies)]
        (msg-utils/clear-replies!)
        (let [m2 (msg-utils/mock-command-message "define" "number=1 test")
              _ (sut/-processChannelMessage nil m2)
              replies2 (msg-utils/get-replies)]
          ;; Both should behave the same way
          (is (= (count replies1) (count replies2))))))))

(deftest test-multiple-parameters
  (testing "Multiple parameters can be combined"
    (let [m (msg-utils/mock-command-message "define" "dict=gcide num=1 test")]
      (sut/-processChannelMessage nil m)
      (is (>= (msg-utils/reply-count) 1)))))

(comment
  ;; Manual test runs for development
  (run-tests 'org.goat.module.Define-test))
