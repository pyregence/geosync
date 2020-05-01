;;; Copyright 2020 Gary W. Johnson (gjohnson@sig-gis.com)
;;;
;;; This file is part of geosync.
;;;
;;; geosync is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; geosync is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with geosync.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Description:
;;;
;;;   geosync is a simple command-line application that traverses a
;;;   directory of raster and vector GIS files (e.g., GeoTIFFs,
;;;   Shapefiles) and generates the necessary REST commands to add
;;;   layers for each file to a running GeoServer instance.

(ns geosync.core
  (:require [clojure.edn        :as edn]
            [clojure.java.io    :as io]
            [clojure.java.shell :refer [with-sh-dir sh]]
            [clojure.string     :as str]
            [clojure.tools.cli  :refer [parse-opts]]
            [hiccup2.core       :refer [html]]
            [clj-http.client    :as client])
  (:import java.util.Base64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def base64-encoder (Base64/getUrlEncoder))

(defn encode-str [s]
  (.encodeToString base64-encoder (.getBytes s)))

(defmacro xml [& args]
  `(str (html {:mode :xml} ~@args)))

(defn make-rest-request
  [{:keys [geoserver-rest-uri geoserver-rest-http-headers]}
   [http-method uri-suffix http-body]]
  (try
    (client/request {:url     (str geoserver-rest-uri uri-suffix)
                     :method  http-method
                     :headers (geoserver-rest-http-headers http-method)
                     :body    http-body})
    (catch Exception e (println "REST Exception:" http-method uri-suffix "->" (.getMessage e)))))

;; FIXME: Update this for the pyregence application
(defn extract-filename
  [uri]
  (second (re-find #"^file:.*/([^/]*).tif$" uri)))

;; FIXME: Update this for the pyregence application
(defn extract-path
  [uri]
  (second (re-find #"^file:/raid/geodata/(.*)$" uri)))

;; FIXME: Update this for the pyregence application
(defn run-gdal-info
  [geoserver-data-dir uri]
  (let [result (with-sh-dir geoserver-data-dir
                 (:out (sh "gdalinfo" (extract-path uri))))]
    (if (.isEmpty result)
      (throw (Exception. (str "gdalinfo failed for file: "
                              geoserver-data-dir
                              (if-not (.endsWith geoserver-data-dir "/") "/")
                              (extract-path uri))))
      result)))

;; FIXME: Update this for the pyregence application
(defn dms->dd
  [dms]
  (let [[d m s dir] (map read-string (rest (re-find #"^([ \d]+)d([ \d]+)'([ \.0123456789]+)\"(\w)$" dms)))
        unsigned-dd (+ d (/ m 60.0) (/ s 3600.0))]
    (if (#{'S 'W} dir)
      (- unsigned-dd)
      unsigned-dd)))

;; FIXME: Update this for the pyregence application
(defn radians->degrees
  [rads]
  (/ (* rads 180.0) Math/PI))

;; FIXME: This function must return these fields:
;; - native-crs
;; - native-min-x
;; - native-max-x
;; - native-min-y
;; - native-max-y
;; - latlon-min-x
;; - latlon-max-x
;; - latlon-min-y
;; - latlon-max-y
;; - cols-rows
;; - pixel-width
;; - pixel-height
;; - shear-x
;; - shear-y
;; - x-origin
;; - y-origin
;; - color-interp
(defn extract-georeferences
  [geoserver-data-dir uri]
  (let [gdal-info (run-gdal-info geoserver-data-dir uri)
        cols-rows-regex    #"(?s)Size is (\d+), (\d+)"
        pixel-size-regex   #"(?s)Pixel Size = \(([\-\.0123456789]+),([\-\.0123456789]+)\)"
        origin-regex       #"(?s)Origin = \(([\-\.0123456789]+),([\-\.0123456789]+)\)"

        color-interp-regex #"(?s)ColorInterp=(\w+)"
        native-crs-regex   #"(?s)Coordinate System is:\s*\n(.+)Origin"

        upper-left-regex   #"(?s)Upper Left\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        lower-left-regex   #"(?s)Lower Left\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        upper-right-regex  #"(?s)Upper Right\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        lower-right-regex  #"(?s)Lower Right\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"

        [cols rows]                (rest (re-find cols-rows-regex  gdal-info))
        [pixel-width pixel-height] (rest (re-find pixel-size-regex gdal-info))
        [x-origin y-origin]        (rest (re-find origin-regex     gdal-info))

        [ul-native-x ul-native-y ul-latlon-x ul-latlon-y] (rest (re-find upper-left-regex gdal-info))
        [ll-native-x ll-native-y ll-latlon-x ll-latlon-y] (rest (re-find lower-left-regex gdal-info))
        [ur-native-x ur-native-y ur-latlon-x ur-latlon-y] (rest (re-find upper-right-regex gdal-info))
        [lr-native-x lr-native-y lr-latlon-x lr-latlon-y] (rest (re-find lower-right-regex gdal-info))

        [ul-native-x ul-native-y] (map read-string [ul-native-x ul-native-y])
        [ll-native-x ll-native-y] (map read-string [ll-native-x ll-native-y])
        [ur-native-x ur-native-y] (map read-string [ur-native-x ur-native-y])
        [lr-native-x lr-native-y] (map read-string [lr-native-x lr-native-y])]

    {:cols-rows    (str cols " " rows)
     :pixel-width  pixel-width
     :pixel-height pixel-height
     :x-origin     x-origin
     :y-origin     y-origin
     :color-interp (second (re-find color-interp-regex gdal-info))
     :native-crs   (second (re-find native-crs-regex gdal-info))
     :native-min-x (str (min ul-native-x ll-native-x))
     :native-max-x (str (max ur-native-x lr-native-x))
     :native-min-y (str (min ll-native-y lr-native-y))
     :native-max-y (str (max ul-native-y ur-native-y))
     :latlon-min-x (str (apply min (map dms->dd [ul-latlon-x ll-latlon-x])))
     :latlon-max-x (str (apply max (map dms->dd [ur-latlon-x lr-latlon-x])))
     :latlon-min-y (str (apply min (map dms->dd [ll-latlon-y lr-latlon-y])))
     :latlon-max-y (str (apply max (map dms->dd [ul-latlon-y ur-latlon-y])))
     :shear-x      (str (radians->degrees (Math/asin (/ (- ll-native-x ul-native-x) (- ul-native-y ll-native-y)))))
     :shear-y      (str (radians->degrees (Math/asin (/ (- ur-native-y ul-native-y) (- ur-native-x ul-native-x)))))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GeoServer REST API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Workspaces (http://docs.geoserver.org/latest/en/api/#1.0.0/workspaces.yaml)

(defn get-workspaces []
  ["GET"
   "/workspaces"
   nil])

(defn get-workspace [workspace]
  ["GET"
   (str "/workspaces/" workspace)
   nil])

(defn create-workspace [workspace]
  ["POST"
   "/workspaces"
   (xml
    [:workspace
     [:name workspace]])])

(defn update-workspace [workspace]
  ["PUT"
   (str "/workspaces/" workspace)
   (xml
    [:workspace
     [:name workspace]])])

(defn delete-workspace [workspace]
  ["DELETE"
   (str "/workspaces/" workspace)
   nil])

;; Namespaces (http://docs.geoserver.org/latest/en/api/#1.0.0/namespaces.yaml)

(defn get-namespaces []
  ["GET"
   "/namespaces"
   nil])

(defn get-namespace [workspace]
  ["GET"
   (str "/namespaces/" workspace)
   nil])

(defn create-namespace [uri-prefix workspace]
  ["POST"
   "/namespaces"
   (xml
    [:namespace
     [:prefix workspace]
     [:uri (str uri-prefix workspace)]])])

(defn update-namespace [uri-prefix workspace]
  ["PUT"
   (str "/namespaces/" workspace)
   (xml
    [:namespace
     [:prefix workspace]
     [:uri (str uri-prefix workspace)]])])

(defn delete-namespace [workspace]
  ["DELETE"
   (str "/namespaces/" workspace)
   nil])

;; Data Stores (Vector) (http://docs.geoserver.org/latest/en/api/#1.0.0/datastores.yaml)

(defn get-data-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/datastores")
   nil])

(defn get-data-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

;; FIXME: Only Shapefile stores are currently supported.
;; NOTE: file-url should look like file:/path/to/nyc.shp
(defn create-data-store [workspace store file-url]
  ["POST"
   (str "/workspaces/" workspace "/datastores")
   (xml
    [:dataStore
     [:name store]
     [:enabled true]
     [:connectionParameters
      [:url file-url]]])])

;; FIXME: Only Shapefile stores are currently supported.
;; NOTE: file-url should look like file:/path/to/nyc.shp
(defn update-data-store [workspace store file-url enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store)
   (xml
    [:dataStore
     [:name store]
     [:enabled enabled?]
     [:connectionParameters
      [:url file-url]]])])

(defn delete-data-store [workspace store]
  ["DELETE"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

;; Coverage Stores (Raster) (http://docs.geoserver.org/latest/en/api/#1.0.0/coveragestores.yaml)

(defn get-coverage-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/coveragestores")
   nil])

(defn get-coverage-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/coveragestores/" store)
   nil])

;; NOTE: file-url should look like file:/path/to/nyc.tiff
(defn create-coverage-store [workspace store file-url]
  ["POST"
   (str "/workspaces/" workspace "/coveragestores")
   (xml
    [:coverageStore
     [:name store]
     [:enabled true]
     [:url file-url]])])

;; NOTE: file-url should look like file:/path/to/nyc.tiff
(defn update-coverage-store [workspace store file-url enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/coveragestores/" store)
   (xml
    [:coverageStore
     [:name store]
     [:enabled enabled?]
     [:url file-url]])])

(defn delete-coverage-store [workspace store]
  ["DELETE"
   (str "/workspaces/" workspace "/coveragestores/" store)
   nil])

;; Feature Types (Vector) (http://docs.geoserver.org/latest/en/api/#1.0.0/featuretypes.yaml)

(defn get-feature-types
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/featuretypes")
    nil])
  ([workspace store]
   ["GET"
    (str "/workspaces/" workspace "/datastores/" store "/featuretypes")
    nil]))

(defn get-feature-type
  ([workspace feature-type]
   ["GET"
    (str "/workspaces/" workspace "/featuretypes/" feature-type)
    nil])
  ([workspace store feature-type]
   ["GET"
    (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
    nil]))

;; FIXME: Only Shapefile feature types are currently supported.
(defn create-feature-type-via-put [workspace store file-url]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/external.shp")
   file-url])

(defn create-feature-type [workspace store feature-type title abstract description keywords crs srs max-features num-decimals]
  ["POST"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes")
   (xml
    [:featureType
     [:name feature-type]
     [:nativeName feature-type]
     [:title title]
     [:abstract abstract]
     [:description description]
     [:keywords
      (map (fn [k] [:string k]) keywords)]
     [:nativeCRS crs]
     [:srs srs]
     [:maxFeatures max-features]
     [:numDecimals num-decimals]
     [:enabled true]])])

(defn update-feature-type [workspace store feature-type title abstract description keywords crs srs max-features num-decimals enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
   (xml
    [:featureType
     [:title title]
     [:abstract abstract]
     [:description description]
     [:keywords
      (map (fn [k] [:string k]) keywords)]
     [:nativeCRS crs]
     [:srs srs]
     [:maxFeatures max-features]
     [:numDecimals num-decimals]
     [:enabled enabled?]])])

(defn delete-feature-type [workspace store feature-type]
  ["DELETE"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
   nil])

;; Coverages (Raster) (http://docs.geoserver.org/latest/en/api/#1.0.0/coverages.yaml)

(defn get-coverages
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/coverages")
    nil])
  ([workspace store]
   ["GET"
    (str "/workspaces/" workspace "/coveragestores/" store "/coverages")
    nil]))

(defn get-coverage
  ([workspace coverage]
   ["GET"
    (str "/workspaces/" workspace "/coverages/" coverage)
    nil])
  ([workspace store coverage]
   ["GET"
    (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
    nil]))

(defn create-coverage [workspace store coverage title abstract description keywords proj-code interpolation-method file-url]
  (let [gdal-info (extract-georeferences file-url)]
    ["POST"
     (str "/workspaces/" workspace "/coveragestores/" store "/coverages")
     (xml
      [:coverage
       [:name coverage]
       [:nativeName coverage]
       [:title title]
       [:abstract abstract]
       [:description description]
       [:keywords
        (map (fn [k] [:string k]) keywords)]
       [:nativeCRS (:native-crs gdal-info)]
       [:srs proj-code]
       [:nativeBoundingBox
        [:crs proj-code]
        [:minx (:native-min-x gdal-info)]
        [:maxx (:native-max-x gdal-info)]
        [:miny (:native-min-y gdal-info)]
        [:maxy (:native-max-y gdal-info)]]
       [:latLonBoundingBox
        [:crs "EPSG:4326"]
        [:minx (:latlon-min-x gdal-info)]
        [:maxx (:latlon-max-x gdal-info)]
        [:miny (:latlon-min-y gdal-info)]
        [:maxy (:latlon-max-y gdal-info)]]
       [:projectionPolicy "REPROJECT_TO_DECLARED"]
       [:nativeFormat "GEOTIFF"]
       [:grid {:dimension "2"}
        [:crs proj-code]
        [:range
         [:low "0 0"]
         [:high (:cols-rows gdal-info)]]
        [:transform
         [:scaleX     (:pixel-width  gdal-info)]
         [:scaleY     (:pixel-height gdal-info)]
         [:shearX     (:shear-x      gdal-info)]
         [:shearY     (:shear-y      gdal-info)]
         [:translateX (:x-origin     gdal-info)]
         [:translateY (:y-origin     gdal-info)]]]
       [:supportedFormats
        [:string "GIF"]
        [:string "PNG"]
        [:string "JPEG"]
        [:string "TIFF"]
        [:string "GEOTIFF"]]
       [:interpolationMethods
        [:string "nearest neighbor"]
        [:string "bilinear"]
        [:string "bicubic"]]
       [:defaultInterpolationMethod interpolation-method]
       [:dimensions
        [:coverageDimension
         [:name (str/upper-case (str (:color-interp gdal-info) "_INDEX"))]
         [:description "GridSampleDimension[-Infinity,Infinity]"]]]
       [:requestSRS
        [:string "EPSG:4326"]
        (if (not= proj-code "EPSG:4326")
          [:string proj-code])]
       [:responseSRS
        [:string "EPSG:4326"]
        (if (not= proj-code "EPSG:4326")
          [:string proj-code])]
       [:enabled true]])]))

(defn update-coverage [workspace store coverage title abstract description keywords proj-code interpolation-method file-url enabled?]
  (let [gdal-info (extract-georeferences file-url)]
    ["PUT"
     (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
     (xml
      [:coverage
       [:title title]
       [:abstract abstract]
       [:description description]
       [:keywords
        (map (fn [k] [:string k]) keywords)]
       [:nativeCRS (:native-crs gdal-info)]
       [:srs proj-code]
       [:nativeBoundingBox
        [:crs proj-code]
        [:minx (:native-min-x gdal-info)]
        [:maxx (:native-max-x gdal-info)]
        [:miny (:native-min-y gdal-info)]
        [:maxy (:native-max-y gdal-info)]]
       [:latLonBoundingBox
        [:crs "EPSG:4326"]
        [:minx (:latlon-min-x gdal-info)]
        [:maxx (:latlon-max-x gdal-info)]
        [:miny (:latlon-min-y gdal-info)]
        [:maxy (:latlon-max-y gdal-info)]]
       [:grid {:dimension "2"}
        [:crs proj-code]
        [:range
         [:low "0 0"]
         [:high (:cols-rows gdal-info)]]
        [:transform
         [:scaleX     (:pixel-width  gdal-info)]
         [:scaleY     (:pixel-height gdal-info)]
         [:shearX     (:shear-x      gdal-info)]
         [:shearY     (:shear-y      gdal-info)]
         [:translateX (:x-origin     gdal-info)]
         [:translateY (:y-origin     gdal-info)]]]
       [:defaultInterpolationMethod interpolation-method]
       [:dimensions
        [:coverageDimension
         [:name (str/upper-case (str (:color-interp gdal-info) "_INDEX"))]
         [:description "GridSampleDimension[-Infinity,Infinity]"]]]
       [:requestSRS
        [:string "EPSG:4326"]
        (if (not= proj-code "EPSG:4326")
          [:string proj-code])]
       [:responseSRS
        [:string "EPSG:4326"]
        (if (not= proj-code "EPSG:4326")
          [:string proj-code])]
       [:enabled enabled?]])]))

(defn delete-coverage [workspace store coverage]
  ["DELETE"
   (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
   nil])

;; Layers (https://docs.geoserver.org/latest/en/api/#1.0.0/layers.yaml)

(defn get-layers
  ([]
   ["GET"
    "/layers"
    nil])
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/layers")
    nil]))

(defn get-layer
  ([layer]
   ["GET"
    (str "/layers/" layer)
    nil])
  ([workspace layer]
   ["GET"
    (str "/workspaces/" workspace "/layers/" layer)
    nil]))

;; NOTE: This is not part of the REST API. Use create-coverage or create-feature-type instead.
(defn create-layer [])

(defn update-layer
  ([layer path layer-type default-style styles resource-type resource-name opaque?]
   ["PUT"
    (str "/layers/" layer)
    (xml
     [:layer
      [:path path]
      [:type layer-type]
      [:defaultStyle
       [:name default-style]]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]
      [:resource {:class resource-type}
       [:name resource-name]]
      [:opaque opaque?]])])
  ([workspace layer path layer-type default-style styles resource-type resource-name opaque?]
   ["PUT"
    (str "/workspaces/" workspace "/layers/" layer)
    (xml
     [:layer
      [:path path]
      [:type layer-type]
      [:defaultStyle
       [:name default-style]]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]
      [:resource {:class resource-type}
       [:name resource-name]]
      [:opaque opaque?]])]))

(defn delete-layer
  ([layer]
   ["DELETE"
    (str "/layers/" layer)
    nil])
  ([workspace layer]
   ["DELETE"
    (str "/workspaces/" workspace "/layers/" layer)
    nil]))

;; Layer Groups (http://docs.geoserver.org/latest/en/api/#1.0.0/layergroups.yaml)

;; Styles (http://docs.geoserver.org/latest/en/api/#1.0.0/styles.yaml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Directory Traversal
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn paths->xml
  "Generates a sequence of REST request specifications as triplets of
  [http-method uri-suffix http-body]. Each file path may contribute
  one or more of these to the final sequence."
  [config-params file-paths]
  (->> file-paths
       (mapcat (partial translate-row config-params))
       (remove nil?)))

(defn load-file-paths [data-dir]
  (->> (io/file data-dir)
       (file-seq)
       (filter #(.isFile %))
       (map #(.getPath %))))

(defn update-geoserver
  "Loads the row data in from the spreadsheet according to
   the :spreadsheet-filename, :spreadsheet-sheetname, and :column-spec
   config-params.  Rows beginning with a # are ignored.  If any rows
   begin with a + or -, then only those rows will be processed
   further.  Otherwise, all spreadsheet rows will be processed by the
   algorithm.  For each row to be processed, a triplet of [http-method
   uri-suffix http-body] is created to describe the REST request that
   will be sent to our geoserver for that row.  Each such pair is then
   sent off as a REST request, and the number of successful and failed
   requests is printed to STDOUT along with the error messages for any
   failed requests."
  [{:keys [data-dir] :as config-params}]
  (let [http-responses   (->> (load-file-paths data-dir)
                              (paths->xml config-params)
                              (map (partial make-rest-request config-params))) ;; FIXME: use client/with-connection-pool for speed
        failed-responses (filter #(not= 200 (:status %)) http-responses)]
    (println "\nFinished updating Geoserver.\nSuccessful requests:"
             (- (count http-responses) (count failed-responses))
             "\nFailed requests:"
             (count failed-responses)
             "\n\nErrors:")
    (if (empty? failed-responses)
      (println "None")
      (doseq [[status reason] (map (juxt :status :reason-phrase) failed-responses)]
        (println status reason)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: Use clojure.spec to validate the map
(defn read-config-params
  "Opens config-file-path as a java.io.PushbackReader and calls the
   Clojure Reader on it once in order to load the first object in the
   file in as a Clojure data structure.  If the object is a Clojure
   map whose keys are keywords, it will be returned.  If
   config-file-path is nil, returns {}."
  [config-file-path]
  (if config-file-path
    (let [file-params (edn/read-string (slurp config-file-path))]
      (if (and (map? file-params)
               (every? keyword? (keys file-params)))
        file-params
        (throw (Exception. (str "The config-file must contain a clojure map whose keys are keywords: " config-file-path)))))
    {}))

(def cli-options
  [["-i" "--config-file EDN"         "Path to a clojure file containing a map of configuration parameters."]
   ["-d" "--data-dir PATH"           "Path to the directory containing your GIS files."]
   ["-n" "--namespace-prefix NS"     "URI prefix for constructing namespaces from workspace names."]
   ["-g" "--geoserver-rest-uri URI"  "URI of your Geoserver's REST extensions."]
   ["-u" "--geoserver-username USER" "Geoserver admin username."]
   ["-p" "--geoserver-password PASS" "Geoserver admin password."]
   ["-D" "--geoserver-data-dir PATH" "Path to your Geoserver's data_dir."]
   ["-U" "--postgis-user USER"       "Username for postgis database access."]])

(defn -main
  "Call this with the name of a Clojure file containing the
  config-params map. The params will be read into a hash-map and
  passed on to the update-geoserver function. So that we only have to
  calculate it once, the geoserver-auth-code is generated here from
  the :geoserver-username and :geoserver-password fields in the
  passed-in map and added to the in-memory hash-map under
  the :geoserver-rest-http-headers entry."
  [& args]
  (println (str "geosync: Update a running Geoserver instance from an XLS spreadsheet.\n"
                "Copyright 2010-2012 Gary W. Johnson (gjohnson@sig-gis.com)"))
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (if (seq errors)
      (println errors)
      (let [config-file-params  (read-config-params (:config-file options))
            command-line-params (into {} (remove (comp nil? val)) (dissoc options :config-file))
            config-params       (merge config-file-params command-line-params)
            geoserver-auth-code (str "Basic " (encode-str (str (:geoserver-username config-params)
                                                               ":"
                                                               (:geoserver-password config-params))))]
        (update-geoserver
         (assoc config-params
                :geoserver-rest-http-headers {"POST"   {"Accept"        "text/xml"
                                                        "Content-Type"  "text/xml"
                                                        "Authorization" geoserver-auth-code}
                                              "PUT"    {"Accept"        "*/*"
                                                        "Content-Type"  "text/plain"
                                                        "Authorization" geoserver-auth-code}
                                              "DELETE" {"Accept"        "*/*"
                                                        "Content-Type"  "*/*"
                                                        "Authorization" geoserver-auth-code}})))))
  ;; Exit cleanly.
  (shutdown-agents)
  (flush)
  (System/exit 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Unfiled Legacy Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-store-type
  "Returns a string describing the class of data or coverage store
   implied by the structure of the passed-in URI."
  [URI]
  (condp re-matches URI
    #"^file:.*\.tif$"    "GeoTIFF"
    #"^file:.*\.shp$"    "Shapefile"
    #"^postgis:.*\.shp$" "PostGIS-converted Shapefile"
    #"^postgis:.*$"      "PostGIS Database"
    :otherwise           (throw (Exception. (str "Unrecognized URI: " URI)))))

#_(defn translate-row
  "Returns a vector of one or more REST request specifications as
   triplets of [http-method uri-suffix http-body] depending on the
   contents of the passed-in row."
  [config-params {:keys [Workspace Store Layer URI Delete?] :as row}]
  (if URI
    (let [store-type (get-store-type URI)]
      (cond Layer
            (condp = store-type
              "GeoTIFF"
              (if Delete?
                ((juxt delete-layer delete-coverage delete-coverage-store) config-params row)
                ((juxt create-coverage-store create-coverage) config-params row))

              "Shapefile"
              (if Delete?
                ((juxt delete-layer delete-shapefile-feature-type delete-shapefile-data-store) config-params row)
                ((juxt create-shapefile-data-store create-shapefile-feature-type) config-params row))

              "PostGIS-converted Shapefile"
              (if Delete?
                ((juxt delete-layer delete-postgis-feature-type remove-shapefile-from-postgis-db) config-params row)
                ((juxt add-shapefile-to-postgis-db create-postgis-feature-type) config-params row))

              "PostGIS Database"
              (if Delete?
                ((juxt delete-layer delete-postgis-feature-type) config-params row)
                [(create-postgis-feature-type config-params row)]))

            Store
            (if (= store-type "PostGIS Database")
              (if Delete?
                ((juxt delete-postgis-data-store drop-postgis-database) config-params row)
                ((juxt create-postgis-database create-postgis-data-store) config-params row))
              (throw (Exception. (str "Cannot declare file-based store without layer on same row: " Workspace ":" Store " (" URI ")"))))

            :otherwise (throw (Exception. (str "A row with a defined URI must also declare either a new Store or Layer: "
                                               Workspace " (" URI ")")))))

    (if (and Workspace
             (nil? Store)
             (nil? Layer))
      (if Delete?
        [(delete-workspace config-params row)]
        [(create-workspace config-params row)])
      (throw (Exception. "Rows without URIs must declare new workspaces: " row)))))
