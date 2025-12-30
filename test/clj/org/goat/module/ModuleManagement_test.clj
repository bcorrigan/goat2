(ns org.goat.module.ModuleManagement-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [org.goat.module.ModuleManagement :as sut]
            [org.goat.db.module-settings :as db]
            [org.goat.testutils.message :as msg-utils]
            [org.goat.core.registry :as registry]
            [clojure.java.jdbc :as sql])
  (:import [java.io File]))

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-module-settings.db"})

(defn db-fixture [f]
  ;; Setup: Create test database
  (let [db-file (File. "test/resources/test-module-settings.db")]
    (when (.exists db-file) (.delete db-file)))

  (with-redefs [db/db test-db]
    (db/create-db)
    (msg-utils/clear-replies!)
    (f))

  ;; Teardown
  (let [db-file (File. "test/resources/test-module-settings.db")]
    (when (.exists db-file) (.delete db-file))))

(use-fixtures :each db-fixture)

(deftest test-chatignore-valid-module
  (testing "chatignore command ignores valid module"
    (msg-utils/with-clean-replies
      ;; Mock registry to return a valid module
      (with-redefs [registry/get-module-by-name (fn [name] {:module-name name})]
        (let [msg (msg-utils/mock-command-message "chatignore" "WordStats"
                    {:chat-id 123 :sender "alice"})]
          (sut/process-message msg)
          (is (msg-utils/replied-with? "will now ignore"))
          (is (db/is-module-ignored? 123 "WordStats")))))))

(deftest test-chatignore-invalid-module
  (testing "chatignore rejects invalid module names"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "chatignore" "FakeModule"
                  {:chat-id 123 :sender "alice"})]
        (sut/process-message msg)
        (is (msg-utils/replied-with? "not found"))
        (is (not (db/is-module-ignored? 123 "FakeModule")))))))

(deftest test-chatignore-no-args
  (testing "chatignore without module name shows usage"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "chatignore" ""
                  {:chat-id 123 :sender "alice"})]
        (sut/process-message msg)
        (is (msg-utils/replied-with? "Usage"))))))

(deftest test-chatignore-already-ignored
  (testing "chatignore on already ignored module shows info message"
    (msg-utils/with-clean-replies
      ;; Mock registry to return a valid module
      (with-redefs [registry/get-module-by-name (fn [name] {:module-name name})]
        ;; First ignore it
        (db/ignore-module! 123 "WordStats" "alice")

        ;; Try to ignore again
        (let [msg (msg-utils/mock-command-message "chatignore" "WordStats"
                    {:chat-id 123 :sender "alice"})]
          (sut/process-message msg)
          (is (msg-utils/replied-with? "already ignored")))))))

(deftest test-chatunignore
  (testing "chatunignore removes module from ignore list"
    (msg-utils/with-clean-replies
      ;; Mock registry to return a valid module
      (with-redefs [registry/get-module-by-name (fn [name] {:module-name name})]
        ;; First ignore it
        (db/ignore-module! 123 "WordStats" "alice")
        (is (db/is-module-ignored? 123 "WordStats"))

        ;; Then unignore
        (let [msg (msg-utils/mock-command-message "chatunignore" "WordStats"
                    {:chat-id 123 :sender "alice"})]
          (sut/process-message msg)
          (is (msg-utils/replied-with? "will now receive"))
          (is (not (db/is-module-ignored? 123 "WordStats"))))))))

(deftest test-chatunignore-not-ignored
  (testing "chatunignore on non-ignored module shows info message"
    (msg-utils/with-clean-replies
      ;; Mock registry to return a valid module
      (with-redefs [registry/get-module-by-name (fn [name] {:module-name name})]
        (let [msg (msg-utils/mock-command-message "chatunignore" "WordStats"
                    {:chat-id 123 :sender "alice"})]
          (sut/process-message msg)
          (is (msg-utils/replied-with? "not currently ignored")))))))

(deftest test-chatlist-ignored
  (testing "chatlist shows ignored modules"
    (msg-utils/with-clean-replies
      (db/ignore-module! 123 "WordStats" "alice")
      (db/ignore-module! 123 "Wordle" "alice")

      (let [msg (msg-utils/mock-command-message "chatlist" ""
                  {:chat-id 123 :sender "alice"})]
        (sut/process-message msg)
        (is (msg-utils/replied-with? "WordStats"))
        (is (msg-utils/replied-with? "Wordle"))))))

(deftest test-chatlist-no-ignored
  (testing "chatlist shows message when no modules ignored"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "chatlist" ""
                  {:chat-id 123 :sender "alice"})]
        (sut/process-message msg)
        (is (msg-utils/replied-with? "No modules are ignored"))))))

(deftest test-chatlist-all
  (testing "chatlist all shows available modules"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "chatlist" "all"
                  {:chat-id 123 :sender "alice"})]
        (sut/process-message msg)
        (is (msg-utils/replied-with? "Available Modules"))))))

(deftest test-different-chats-independent
  (testing "Ignoring in one chat doesn't affect another"
    (msg-utils/with-clean-replies
      ;; Ignore in chat 123
      (db/ignore-module! 123 "WordStats" "alice")
      (is (db/is-module-ignored? 123 "WordStats"))

      ;; Check it's not ignored in chat 456
      (is (not (db/is-module-ignored? 456 "WordStats"))))))
