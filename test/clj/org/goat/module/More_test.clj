(ns org.goat.module.More-test
  (:require [clojure.test :refer :all]
            [org.goat.module.More :as sut]
            [org.goat.testutils.message :as msg-utils])
  (:import [org.goat.core Message]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-mock-message
  "Create a mock message with paging capabilities."
  [command args has-next-page?]
  (let [send-called (atom false)
        base-msg (msg-utils/mock-command-message command args
                                                  {:sender "alice" :chat-id 123})]
    {:msg (proxy [org.goat.core.Message] [123 (str "goat " command " " args) false "alice"]
            (getText [] (.getText base-msg))
            (getChatId [] (.getChatId base-msg))
            (getSender [] (.getSender base-msg))
            (getModCommand [] (.getModCommand base-msg))
            (getModText [] (.getModText base-msg))
            (isPrivate [] false)
            (hasNextPage [] has-next-page?)
            (createNextPage []
              (proxy [org.goat.core.Message] [123 "next" false "goat"]
                (send []
                  (reset! send-called true)
                  nil))))
     :send-called send-called}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGRATION TESTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-more-command-with-next-page
  (testing "More command sends next page when available"
    (let [{:keys [msg send-called]} (create-mock-message "more" "" true)]
      (#'sut/-processChannelMessage nil msg)
      (is (true? @send-called)))))

(deftest test-moar-command-with-next-page
  (testing "Moar command (alternative spelling) works"
    (let [{:keys [msg send-called]} (create-mock-message "moar" "" true)]
      (#'sut/-processChannelMessage nil msg)
      (is (true? @send-called)))))

(deftest test-more-command-without-next-page
  (testing "More command does nothing when no next page available"
    (let [{:keys [msg send-called]} (create-mock-message "more" "" false)]
      (#'sut/-processChannelMessage nil msg)
      (is (false? @send-called)))))

(deftest test-more-command-with-args-ignored
  (testing "More command with additional text is ignored"
    (let [{:keys [msg send-called]} (create-mock-message "more" "some extra text" true)]
      (#'sut/-processChannelMessage nil msg)
      (is (false? @send-called)))))

(deftest test-more-command-with-whitespace-only
  (testing "More command with only whitespace still triggers"
    (let [{:keys [msg send-called]} (create-mock-message "more" "   " true)]
      (#'sut/-processChannelMessage nil msg)
      (is (true? @send-called)))))

(deftest test-more-private-message
  (testing "More works in private messages"
    (let [send-called (atom false)
          base-msg (msg-utils/mock-command-message "more" ""
                                                    {:sender "bob" :chat-id 456 :is-private true})
          msg (proxy [org.goat.core.Message] [456 "goat more" true "bob"]
                (getText [] (.getText base-msg))
                (getChatId [] (.getChatId base-msg))
                (getSender [] (.getSender base-msg))
                (getModCommand [] (.getModCommand base-msg))
                (getModText [] (.getModText base-msg))
                (isPrivate [] true)
                (hasNextPage [] true)
                (createNextPage []
                  (proxy [org.goat.core.Message] [456 "next" true "goat"]
                    (send []
                      (reset! send-called true)
                      nil))))]

      (#'sut/-processPrivateMessage nil msg)
      (is (true? @send-called)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-handle-more-directly
  (testing "handle-more function directly"
    (let [send-called (atom false)
          msg (proxy [org.goat.core.Message] [123 "goat more" false "alice"]
                (getModText [] "")
                (hasNextPage [] true)
                (createNextPage []
                  (proxy [org.goat.core.Message] [123 "next" false "goat"]
                    (send []
                      (reset! send-called true)
                      nil))))]

      (#'sut/handle-more msg)
      (is (true? @send-called)))))
