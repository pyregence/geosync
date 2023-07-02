(ns geosync.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [geosync.core :as core]))

(deftest get-matching-style-test
  (testing "when style is defined it returns itself"
    (is (= "my-style" (core/get-matching-style "my-layer" "my-style" [] true))))
  (testing "when style is nil and autostyle true"
    (is (= "my-layer" (core/get-matching-style "my-layer" nil ["my-layer"] true)))
    (is (= "overriden-style" (core/get-matching-style "my-layer" "overriden-style" ["my-layer"] true)))
    (is (nil? (core/get-matching-style "my-layer" nil ["other-style-name"] true))))
  (testing "autostyle false"
    (is (= "my-predefined-style" (core/get-matching-style "my-layer" "my-predefined-style" [] false)))))
