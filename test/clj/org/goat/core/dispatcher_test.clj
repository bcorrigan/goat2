(ns org.goat.core.dispatcher-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [org.goat.core.dispatcher :as dispatcher]
            [org.goat.db.module-settings :as db]
            [org.goat.core.module-protocol :as mp])
  (:import [java.io File]))

(def test-db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test/resources/test-dispatcher-settings.db"})

(defn db-fixture [f]
  (let [db-file (File. "test/resources/test-dispatcher-settings.db")]
    (when (.exists db-file) (.delete db-file)))

  (with-redefs [db/db test-db]
    (db/create-db)
    (f))

  (let [db-file (File. "test/resources/test-dispatcher-settings.db")]
    (when (.exists db-file) (.delete db-file))))

(use-fixtures :each db-fixture)

(deftest test-should-dispatch-not-ignored
  (testing "Module receives messages from non-ignored chats"
    (let [msg {:message/chat-id 123 :message/private? false}
          module (mp/map->ModuleMetadata
                   {:module-name "TestModule" :wants-private true})]
      (is (#'dispatcher/should-dispatch-to-module? msg module)))))

(deftest test-should-dispatch-ignored
  (testing "Module does not receive messages from ignored chats"
    (db/ignore-module! 123 "TestModule" "alice")
    (let [msg {:message/chat-id 123 :message/private? false}
          module (mp/map->ModuleMetadata
                   {:module-name "TestModule" :wants-private true})]
      (is (not (#'dispatcher/should-dispatch-to-module? msg module))))))

(deftest test-should-dispatch-different-chat
  (testing "Module still receives messages from other chats"
    (db/ignore-module! 123 "TestModule" "alice")
    (let [msg {:message/chat-id 456 :message/private? false}
          module (mp/map->ModuleMetadata
                   {:module-name "TestModule" :wants-private true})]
      (is (#'dispatcher/should-dispatch-to-module? msg module)))))

(deftest test-should-dispatch-private-message-wants-private
  (testing "Module receives private messages when wants-private is true"
    (let [msg {:message/chat-id 123 :message/private? true}
          module (mp/map->ModuleMetadata
                   {:module-name "TestModule" :wants-private true})]
      (is (#'dispatcher/should-dispatch-to-module? msg module)))))

(deftest test-should-dispatch-private-message-no-wants-private
  (testing "Module does not receive private messages when wants-private is false"
    (let [msg {:message/chat-id 123 :message/private? true}
          module (mp/map->ModuleMetadata
                   {:module-name "TestModule" :wants-private false})]
      (is (not (#'dispatcher/should-dispatch-to-module? msg module))))))

(deftest test-should-dispatch-ignored-and-private
  (testing "Module does not receive messages when both ignored and private"
    (db/ignore-module! 123 "TestModule" "alice")
    (let [msg {:message/chat-id 123 :message/private? true}
          module (mp/map->ModuleMetadata
                   {:module-name "TestModule" :wants-private true})]
      (is (not (#'dispatcher/should-dispatch-to-module? msg module))))))
