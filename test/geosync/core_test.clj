(ns geosync.core-test
  (:require [clojure.string      :as s]
            [clojure.test        :refer [deftest is testing]]
            [geosync.core        :as core]))

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
    (is (= ["POST"
            "/workspaces/my-workspace/styles?name=my-workspace:test-style"
            "* {\n\traster-channels: auto;\n\traster-color-map:\n\t\tcolor-map-entry(#000000, -340282306073709652508363335590014353408.00, 0, \"nodata\")\n\t\tcolor-map-entry(#7a0403, 0.01, 1)\n\t\tcolor-map-entry(#d93807, 0.03, 1)\n\t\tcolor-map-entry(#fe992c, 0.04, 1)\n\t\tcolor-map-entry(#d3e835, 0.06, 1)\n\t\tcolor-map-entry(#64fd6a, 0.07, 1)\n\t\tcolor-map-entry(#4777ef, 0.09, 1)\n\t\tcolor-map-entry(#30123b, 0.10, 1);\n\traster-color-map-type: ramp;\n\traster-contrast-enhancement: none;\n\traster-gamma: 1.0;\n}"
            "application/vnd.geoserver.geocss+css"]
           (core/file-path->style-spec (geosync-conf) "test/data/test-style.css" []))))
  (testing "returns create spec when style does not exist - overwrite true"
    (is (= ["POST"
            "/workspaces/my-workspace/styles?name=my-workspace:test-style"
            "* {\n\traster-channels: auto;\n\traster-color-map:\n\t\tcolor-map-entry(#000000, -340282306073709652508363335590014353408.00, 0, \"nodata\")\n\t\tcolor-map-entry(#7a0403, 0.01, 1)\n\t\tcolor-map-entry(#d93807, 0.03, 1)\n\t\tcolor-map-entry(#fe992c, 0.04, 1)\n\t\tcolor-map-entry(#d3e835, 0.06, 1)\n\t\tcolor-map-entry(#64fd6a, 0.07, 1)\n\t\tcolor-map-entry(#4777ef, 0.09, 1)\n\t\tcolor-map-entry(#30123b, 0.10, 1);\n\traster-color-map-type: ramp;\n\traster-contrast-enhancement: none;\n\traster-gamma: 1.0;\n}"
            "application/vnd.geoserver.geocss+css"]
           (core/file-path->style-spec (geosync-conf {:overwrite-styles true}) "test/data/test-style.css" []))))
  (testing "returns update spec when style exsits and overwrite is true"
    (is (= ["PUT"
            "/workspaces/my-workspace/styles/my-workspace:test-style"
            "* {\n\traster-channels: auto;\n\traster-color-map:\n\t\tcolor-map-entry(#000000, -340282306073709652508363335590014353408.00, 0, \"nodata\")\n\t\tcolor-map-entry(#7a0403, 0.01, 1)\n\t\tcolor-map-entry(#d93807, 0.03, 1)\n\t\tcolor-map-entry(#fe992c, 0.04, 1)\n\t\tcolor-map-entry(#d3e835, 0.06, 1)\n\t\tcolor-map-entry(#64fd6a, 0.07, 1)\n\t\tcolor-map-entry(#4777ef, 0.09, 1)\n\t\tcolor-map-entry(#30123b, 0.10, 1);\n\traster-color-map-type: ramp;\n\traster-contrast-enhancement: none;\n\traster-gamma: 1.0;\n}"
            "application/vnd.geoserver.geocss+css"]
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

(defn- gpkg-file-spec
  [store-name layer-name native-name]
  {:store-type  :geopackage
   :store-name  store-name
   :layer-name  layer-name
   :native-name native-name
   :file-url    (str "file:///srv/gis/" store-name ".gpkg")
   :style       nil
   :indexed?    false})

(defn- published-feature-type-names
  "The feature type names a spec list POSTs to the featuretypes endpoint."
  [specs]
  (->> specs
       (filter (fn [[method uri]]
                 (and (= "POST" method) (s/ends-with? uri "/featuretypes"))))
       ;; the first <name> belongs to the nested <store>; the feature type's own follows it
       (map (fn [[_ _ body]] (second (re-find #"</store><name>([^<]+)</name>" body))))))

(defn- deleted-feature-type-names
  [specs]
  (->> specs
       (filter (fn [[method uri]]
                 (and (= "DELETE" method) (s/includes? uri "/featuretypes/"))))
       (map (fn [[_ uri]] (last (s/split uri #"/"))))))

(deftest geopackage-layer-specs-test
  (testing "a nested GeoPackage publishes under the path-joined store name, not the bare filename"
    (let [store-name "elmfire_landfire_fire-area_20260909_130000"
          specs      (core/file-spec->layer-specs (geosync-conf)
                                                  #{}
                                                  []
                                                  (gpkg-file-spec store-name
                                                                  "fire-area_20260909_130000"
                                                                  "fire_area"))]
      (is (= [store-name] (published-feature-type-names specs)))
      (is (= ["fire_area"] (deleted-feature-type-names specs)))))
  (testing "a flat GeoPackage keeps the name it publishes under today"
    (let [specs (core/file-spec->layer-specs (geosync-conf)
                                             #{}
                                             []
                                             (gpkg-file-spec "state-boundaries" "state-boundaries" "states"))]
      (is (= ["state-boundaries"] (published-feature-type-names specs)))))
  (testing "no alias is created when the internal table already matches the store name"
    (let [specs (core/file-spec->layer-specs (geosync-conf)
                                             #{}
                                             []
                                             (gpkg-file-spec "viirs-timestamped" "viirs-timestamped" "viirs-timestamped"))]
      (is (empty? (published-feature-type-names specs)))
      (is (= 2 (count specs))))))

(defn- shp-file-spec
  [store-name layer-name style]
  {:store-type :shapefile
   :store-name store-name
   :layer-name layer-name
   :file-url   (str "file:///srv/gis/" store-name ".shp")
   :style      style
   :indexed?   false})

(defn- auto-publishing-puts
  "PUTs to external.shp: each one makes GeoServer publish a feature type named
   after the shapefile, suffixed when another store already took that name."
  [specs]
  (filter (fn [[method uri]] (and (= "PUT" method) (s/includes? uri "external.shp"))) specs))

(deftest shapefile-layer-specs-test
  (testing "a nested shapefile publishes one feature type under the store name, from the shapefile's native name"
    (let [store-name "elmfire_landfire_30_isochrones"
          specs      (core/file-spec->layer-specs (geosync-conf)
                                                  #{}
                                                  []
                                                  (shp-file-spec store-name "isochrones" nil))]
      (is (= [store-name] (published-feature-type-names specs)))
      (is (s/includes? (nth (first (filter #(s/ends-with? (second %) "/featuretypes") specs)) 2)
                       "<nativeName>isochrones</nativeName>"))
      (is (empty? (auto-publishing-puts specs)))
      (is (empty? (deleted-feature-type-names specs)))))
  (testing "a flat shapefile publishes under its own name without the auto-publishing PUT"
    (let [specs (core/file-spec->layer-specs (geosync-conf)
                                             #{}
                                             []
                                             (shp-file-spec "boundaries" "boundaries" nil))]
      (is (= ["boundaries"] (published-feature-type-names specs)))
      (is (empty? (auto-publishing-puts specs)))))
  (testing "a matching style is still applied to the published layer"
    (let [specs (core/file-spec->layer-specs (geosync-conf)
                                             #{}
                                             []
                                             (shp-file-spec "boundaries" "boundaries" "my-workspace:boundaries-css"))
          [method uri] (last specs)]
      (is (= "PUT" method))
      (is (s/ends-with? uri "/layers/boundaries")))))

(deftest cached-layer-delete-ok?-test
  (testing "the codes that mean the tile layer is gone"
    (is (true? (core/cached-layer-delete-ok? 200)))
    (is (true? (core/cached-layer-delete-ok? 404))))
  (testing "anything else leaves the tile layer behind"
    (is (false? (core/cached-layer-delete-ok? 400)))
    (is (false? (core/cached-layer-delete-ok? 500)))))

(deftest file-specs->vector-gwc-specs-test
  (let [file-specs [{:store-type :geopackage  :store-name "fire-history"}
                    {:store-type :shapefile   :store-name "boundaries"}
                    {:store-type :imagemosaic :store-name "hrrr-ws"}
                    {:store-type :geotiff     :store-name "cbh"}]]
    (testing "picks the vector stores, whose tile layers GeoServer auto-creates as vector tiles"
      (is (= [{:store-type :geopackage :store-name "fire-history"}
              {:store-type :shapefile  :store-name "boundaries"}]
             (core/file-specs->vector-gwc-specs file-specs))))
    (testing "leaves the raster stores to file-specs->gwc-specs"
      (is (= [{:store-type :imagemosaic :store-name "hrrr-ws"}]
             (core/file-specs->gwc-specs file-specs))))))
