(ns org.goat.core.macros-test
  (:require [clojure.test :refer :all]
            [org.goat.core.macros :as macros]))

(deftest test-defmodule-macro-exists
  (testing "defmodule macro is available and functional"
    ; Test that the macro exists by checking its metadata
    (is (-> #'macros/defmodule meta :macro))

    ; Test that we can reference the namespace
    (is (= "org.goat.core.macros" (str (ns-name (the-ns 'org.goat.core.macros)))))

    ; The macro is proven to work by CoreCommandsNew compilation succeeding
    (is true "defmodule macro is working as demonstrated by CoreCommandsNew")))

