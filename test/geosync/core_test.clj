(ns geosync.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [geosync.core :as core]))

(defn geosync-conf
  ([]
   {:geoserver-workspace "my-workspace"})
  ([override-map]
   (merge (geosync-conf) override-map)))

(deftest get-matching-style-test
  (testing "when style is defined it returns itself"
    (is (= "my-style" (core/get-matching-style "my-layer" "my-style" [] true))))
  (testing "when style is nil and autostyle true"
    (is (= "sierra-nevada:my-layer" (core/get-matching-style "my-layer" nil ["sierra-nevada:my-layer"] true)))
    (is (= "overriden-style" (core/get-matching-style "my-layer" "overriden-style" ["my-layer"] true)))
    (is (nil? (core/get-matching-style "my-layer" nil ["other-style-name"] true))))
  (testing "real case"
    (is (= "sierra-nevada:PotentialSmokeHighSeverity_2021" (core/get-matching-style "test-sierra-nevada:airQuality_Tier2_PotentialSmokeHighSeverity_2021" nil ["sierra-nevada:PotentialSmokeHighSeverity_2021"] true))))
  (testing "autostyle false"
    (is (= "my-predefined-style" (core/get-matching-style "my-layer" "my-predefined-style" [] false)))))


(deftest get-style-name-test
  (testing "get-style-name returns workspace prefixed"
    (is (= "sierra-nevada:test-style" (core/get-style-name "sierra-nevada" "test/data/test-style.css")))
    (is (= "other-workspace:test-style" (core/get-style-name "other-workspace" "test/data/test-style.css")))
    (is (= "another-workspace_:test-style" (core/get-style-name "another-workspace_" "test/data/test-style.css")))
    (is (= "test-style" (core/get-style-name nil "test/data/test-style.css")))
    (is (= "test-style" (core/get-style-name "" "test/data/test-style.css")))))


(deftest file-path->style-spec-test
  (testing "returns create spec when style does not exist - overwrite false"
    (is (= ["POST" "/workspaces/my-workspace/styles?name=my-workspace:test-style" "* {\nraster-channels: auto;\nraster-color-map: \t\n\tcolor-map-entry(#000000, -340282306073709652508363335590014353408.00, 0, \"nodata\")\n\tcolor-map-entry(#7a0403, 0.01, 1)\n\tcolor-map-entry(#d93807, 0.03, 1)\n\tcolor-map-entry(#fe992c, 0.04, 1)\n\tcolor-map-entry(#d3e835, 0.06, 1)\n\tcolor-map-entry(#64fd6a, 0.07, 1)\n\tcolor-map-entry(#4777ef, 0.09, 1)\n\tcolor-map-entry(#30123b, 0.10, 1);\nraster-color-map-type: ramp;\nraster-contrast-enhancement: none;\nraster-gamma: 1.0;\n}\n    " "application/vnd.geoserver.geocss+css"]
           (core/file-path->style-spec (geosync-conf) "test/data/test-style.css" []))))
  (testing "returns create spec when style does not exist - overwrite true"
    (is (= ["POST" "/workspaces/my-workspace/styles?name=my-workspace:test-style" "* {\nraster-channels: auto;\nraster-color-map: \t\n\tcolor-map-entry(#000000, -340282306073709652508363335590014353408.00, 0, \"nodata\")\n\tcolor-map-entry(#7a0403, 0.01, 1)\n\tcolor-map-entry(#d93807, 0.03, 1)\n\tcolor-map-entry(#fe992c, 0.04, 1)\n\tcolor-map-entry(#d3e835, 0.06, 1)\n\tcolor-map-entry(#64fd6a, 0.07, 1)\n\tcolor-map-entry(#4777ef, 0.09, 1)\n\tcolor-map-entry(#30123b, 0.10, 1);\nraster-color-map-type: ramp;\nraster-contrast-enhancement: none;\nraster-gamma: 1.0;\n}\n    " "application/vnd.geoserver.geocss+css"]
           (core/file-path->style-spec (geosync-conf {:overwrite-styles true}) "test/data/test-style.css" []))))
  (testing "returns update spec when style exsits and overwrite is true"
    (is (= ["PUT" "/workspaces/my-workspace/styles/my-workspace:test-style" "* {\nraster-channels: auto;\nraster-color-map: \t\n\tcolor-map-entry(#000000, -340282306073709652508363335590014353408.00, 0, \"nodata\")\n\tcolor-map-entry(#7a0403, 0.01, 1)\n\tcolor-map-entry(#d93807, 0.03, 1)\n\tcolor-map-entry(#fe992c, 0.04, 1)\n\tcolor-map-entry(#d3e835, 0.06, 1)\n\tcolor-map-entry(#64fd6a, 0.07, 1)\n\tcolor-map-entry(#4777ef, 0.09, 1)\n\tcolor-map-entry(#30123b, 0.10, 1);\nraster-color-map-type: ramp;\nraster-contrast-enhancement: none;\nraster-gamma: 1.0;\n}\n    " "application/vnd.geoserver.geocss+css"]
           (core/file-path->style-spec (geosync-conf {:overwrite-styles true}) "test/data/test-style.css" #{"my-workspace:test-style"}))))
  (testing "returns nil spec when style exists and overwrite is false"
    (is (nil?
         (core/file-path->style-spec (geosync-conf) "test/data/test-style.css" #{"my-workspace:test-style"})))))


(deftest file-path->style-specs-test
  (testing "returns non nil specs with non overlapping styles"
    (is (= (count (core/file-paths->style-specs (geosync-conf) #{"my-workspace:other-style"} ["test/data/test-style.css"]))
           1)))
  (testing "returns zero specs if styles already exists and overwrite-styles is false"
    (is (= (count (core/file-paths->style-specs (geosync-conf) #{"my-workspace:test-style"} ["test/data/test-style.css"]))
           0)))
  (testing "returns one spec if styles already exists and overwrite-styles is false"
    (is (= (count (core/file-paths->style-specs (geosync-conf {:overwrite-styles true}) #{"my-workspace:test-style"} ["test/data/test-style.css"]))
           1))))
