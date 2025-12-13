(ns org.goat.module.TestCapture
  (:require [org.goat.module.Capture :as sut]
            [org.goat.testutils.message :as msg-utils]
            [clojure.test :as t :refer [deftest is testing]]))

(deftest test-extract-url
  (is (= "https://example.com/bum?thingy=this5"
         (:url (sut/extract-url (msg-utils/mock-message {:text "Check out this cool site: https://example.com/bum?thingy=this5 for more info!"}))
         )))
  (is (nil? (sut/extract-url (msg-utils/mock-message {:text "Just some normal message without a url.."}))))
  (is (nil? (sut/extract-url (msg-utils/mock-message {:text ""}))))
  (is (= "http://crap.com/"
       (:url (sut/extract-url (msg-utils/mock-message {:text "http://crap.com/"}))))))

