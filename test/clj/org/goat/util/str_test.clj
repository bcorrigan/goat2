(ns org.goat.util.str-test
  (:require [org.goat.util.str :as sut]
            [clojure.test :refer :all]))

(deftest test-tokenize-message
  (testing "Tokenizes message into words"
    (is (= ["hello" "world"] (sut/tokenize-message "Hello world")))
    (is (= ["test" "message"] (sut/tokenize-message "Test, message!")))
    (is (= ["one" "two" "three"] (sut/tokenize-message "one, two, three")))))

(deftest test-extract-sentences
  (testing "Extracts sentences from text"
    (is (= ["Hello" "How are you"] (sut/extract-sentences "Hello. How are you?")))
    (is (= ["Stop"] (sut/extract-sentences "Stop!")))
    (is (= ["Really" "Yes"] (sut/extract-sentences "Really? Yes.")))))

(deftest test-normalise-word
  (testing "Normalizes words for vocabulary tracking"
    (is (= "hello" (sut/normalise-word "Hello")))
    (is (= "world" (sut/normalise-word "world!")))
    (is (= "test" (sut/normalise-word "TEST?")))))
