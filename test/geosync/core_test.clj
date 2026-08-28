(ns geosync.core-test
  (:import java.io.File)
  (:require [clojure.java.io :as io]
            [clojure.test    :refer [deftest is testing]]
            [geosync.core    :as core]))

(defn- delete-tree!
  [^File file]
  (when (.isDirectory file)
    (doseq [child (.listFiles file)] (delete-tree! child)))
  (.delete file))

(defn- temp-dir!
  []
  (doto (io/file (System/getProperty "java.io.tmpdir")
                 (str "geosync-test-" (System/currentTimeMillis) "-" (rand-int 100000)))
    (.mkdirs)))

(defn- touch!
  [dir file-name]
  (let [file (io/file dir file-name)]
    (io/make-parents file)
    (spit file "")
    file))

(defn- file-names
  [dir]
  (set (map #(.getName ^File %) (.listFiles (io/file dir)))))

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

(deftest timestamped-tif-groups-test
  (let [dir (temp-dir!)]
    (try
      (touch! dir "ws_20260813_010000.tif")
      (touch! dir "ws_20260813_020000.tif")
      (touch! dir "tmpf_20260813_010000.tif")
      (touch! dir "fuels.tif")
      (touch! dir "perimeters_20260813_010000.shp")
      (let [groups (core/timestamped-tif-groups dir)]
        (testing "groups timestamped GeoTIFFs by their layer prefix"
          (is (= #{"ws" "tmpf"} (set (keys groups))))
          (is (= 2 (count (get groups "ws"))))
          (is (= 1 (count (get groups "tmpf")))))
        (testing "an untimestamped GeoTIFF keeps its own layer"
          (is (not (contains? groups "fuels"))))
        (testing "only GeoTIFFs are grouped"
          (is (not (contains? groups "perimeters")))))
      (finally (delete-tree! dir)))))

(deftest convert-time-series-to-imagemosaics!-test
  (testing "folds a flat cycle directory into one mosaic per parameter"
    (let [dir   (temp-dir!)
          cycle (io/file dir "cansac-wrf" "20260813_00")]
      (try
        (.mkdirs cycle)
        (touch! cycle "ws_20260813_010000.tif")
        (touch! cycle "ws_20260813_020000.tif")
        (touch! cycle "tmpf_20260813_010000.tif")
        (is (= 3 (core/convert-time-series-to-imagemosaics! dir)))
        (is (= #{"ws" "tmpf"} (file-names cycle)))
        (is (= #{"ws_20260813_010000.tif" "ws_20260813_020000.tif"
                 "datastore.properties" "indexer.properties" "timeregex.properties"}
               (file-names (io/file cycle "ws"))))
        (finally (delete-tree! dir)))))

  (testing "is idempotent and folds in timesteps that arrive later"
    (let [dir   (temp-dir!)
          cycle (io/file dir "20260813_00")]
      (try
        (.mkdirs cycle)
        (touch! cycle "ws_20260813_010000.tif")
        (core/convert-time-series-to-imagemosaics! dir)
        (testing "a second run with nothing new moves nothing"
          (is (= 0 (core/convert-time-series-to-imagemosaics! dir))))
        (touch! cycle "ws_20260813_020000.tif")
        (testing "a newly arrived timestep joins the existing mosaic"
          (is (= 1 (core/convert-time-series-to-imagemosaics! dir)))
          (is (= #{"ws_20260813_010000.tif" "ws_20260813_020000.tif"
                   "datastore.properties" "indexer.properties" "timeregex.properties"}
                 (file-names (io/file cycle "ws")))))
        (finally (delete-tree! dir)))))

  (testing "leaves an existing ImageMosaic directory untouched"
    (let [dir    (temp-dir!)
          mosaic (io/file dir "hours-since-burned")]
      (try
        (.mkdirs mosaic)
        (touch! mosaic "datastore.properties")
        (touch! mosaic "hours-since-burned_20260813_010000.tif")
        (is (= 0 (core/convert-time-series-to-imagemosaics! dir)))
        (is (= #{"datastore.properties" "hours-since-burned_20260813_010000.tif"}
               (file-names mosaic)))
        (finally (delete-tree! dir)))))

  (testing "leaves timestamped shapefiles alone - psps_zonal and fire_detections
            publish one layer per timestep and are not rasters"
    (let [dir (temp-dir!)]
      (try
        (doseq [ext ["shp" "dbf" "prj" "shx"]]
          (touch! dir (str "deenergization-zones_20260812_180000." ext))
          (touch! dir (str "deenergization-zones_20260812_190000." ext)))
        (is (= 0 (core/convert-time-series-to-imagemosaics! dir)))
        (is (= 8 (count (file-names dir))))
        (finally (delete-tree! dir)))))

  (testing "leaves untimestamped rasters registering as their own layers"
    (let [dir (temp-dir!)]
      (try
        (touch! dir "fbp.tif")
        (touch! dir "dem.tif")
        (is (= 0 (core/convert-time-series-to-imagemosaics! dir)))
        (is (= #{"fbp.tif" "dem.tif"} (file-names dir)))
        (finally (delete-tree! dir))))))
