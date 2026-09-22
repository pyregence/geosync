(ns geosync.rest-api-test
  (:require [clojure.string   :as s]
            [clojure.test     :refer [deftest is testing]]
            [geosync.rest-api :as rest]))

(def ^:private gridsubsets
  [{:gridSetName "EPSG:900913"
    :extent      {:coords [-1.3846911764546365E7 2903056.3663414554
                           -7855025.754906493 6284985.119406082]}}])

(deftest update-cached-layer-metatiling-test
  (let [[method uri body content-type] (rest/update-cached-layer-metatiling
                                        "fire-detections_fire-history"
                                        "fire-history"
                                        1
                                        ["application/vnd.mapbox-vector-tile" "image/png" "image/jpeg"]
                                        gridsubsets)]
    (testing "PUTs to the layer's gwc endpoint"
      (is (= "PUT" method))
      (is (= "/../gwc/rest/layers/fire-detections_fire-history:fire-history.xml" uri))
      (is (= "application/xml" content-type)))

    (testing "asks for a 1x1 metatile, so one request renders one tile"
      (is (s/includes? body "<metaWidthHeight><int>1</int><int>1</int></metaWidthHeight>")))

    (testing "keeps the mime formats GeoServer already serves"
      (is (s/includes? body "<string>application/vnd.mapbox-vector-tile</string>"))
      (is (s/includes? body "<string>image/png</string>"))
      (is (s/includes? body "<string>image/jpeg</string>")))

    (testing "keeps the grid subsets it was handed"
      (is (s/includes? body "<gridSetName>EPSG:900913</gridSetName>")))

    (testing "sets no TIME filter: vector stores carry no time dimension"
      (is (not (s/includes? body "TIME"))))))

(deftest update-cached-layer-metatiling-honours-size-test
  (testing "the metatile size is the one passed in, not a constant"
    (let [[_ _ body] (rest/update-cached-layer-metatiling "ws" "l" 4 ["image/png"] gridsubsets)]
      (is (s/includes? body "<metaWidthHeight><int>4</int><int>4</int></metaWidthHeight>")))))
