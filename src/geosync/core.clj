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
            [clojure.tools.cli  :refer [parse-opts]]
            [hiccup2.core       :refer [html]]
            [clj-http.client    :as client])
  (:import java.util.Base64))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GeoServer REST API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Workspaces

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

;; Namespaces (these have the same name as their workspaces)

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

;; Data Stores (Vector)
;; FIXME: Only Shapefile stores are currently supported. See https://docs.geoserver.org/latest/en/api/#1.0.0/datastores.yaml for more types.

(defn get-data-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/datastores")
   nil])

(defn get-data-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

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

;; RESUME HERE

(defn create-shapefile-store [uri-prefix workspace store description uri]
  ["POST"
   (str "/workspaces/" workspace "/datastores")
   (xml
    [:dataStore
     [:name store]
     [:description description]
     [:type "Shapefile"]
     [:enabled "true"]
     [:connectionParameters
      [:entry {:key "memory mapped buffer"} "true"]
      [:entry {:key "create spatial index"} "true"]
      [:entry {:key "charset"}              "ISO-8859-1"]
      [:entry {:key "url"}                  uri]
      [:entry {:key "namespace"}            (str uri-prefix workspace)]]])])

(defn create-postgis-data-store
  [namespace-prefix postgis-user workspace store description db-name]
  (println "create-postgis-data-store" (str workspace ":" store))
  ["POST"
   (str "/workspaces/" workspace "/datastores")
   (xml
    [:dataStore
     [:name store]
     [:description description]
     [:type "PostGIS"]
     [:enabled "true"]
     [:connectionParameters
      [:entry {:key "host"}                         "localhost"]
      [:entry {:key "port"}                         "5432"]
      [:entry {:key "dbtype"}                       "postgis"]
      [:entry {:key "database"}                     db-name]
      [:entry {:key "user"}                         postgis-user]

      [:entry {:key "namespace"}                    (str namespace-prefix workspace)]
      [:entry {:key "schema"}                       "public"]

      [:entry {:key "min connections"}              "1"]
      [:entry {:key "max connections"}              "10"]
      [:entry {:key "validate connections"}         "true"]
      [:entry {:key "Connection timeout"}           "20"]

      [:entry {:key "fetch size"}                   "1000"]
      [:entry {:key "Loose bbox"}                   "false"]
      [:entry {:key "Expose primary keys"}          "false"]
      [:entry {:key "preparedStatements"}           "false"]
      [:entry {:key "Max open prepared statements"} "50"]]])])

(defn delete-postgis-data-store
  [workspace store]
  (println "delete-postgis-data-store" (str workspace ":" store))
  ["DELETE"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

(defn remove-epsg-prefix
  [string]
  (second (re-find #"^EPSG:(.*)$" string)))

(defn extract-postgis-path
  [uri]
  (second (re-find #"^postgis:/raid/geodata/(.*).shp$" uri)))

(defn add-shapefile-to-postgis-db
  [{:keys [geoserver-data-dir postgis-user]} {:keys [Layer URI DeclaredSRS]}]
  (println "add-shapefile-to-postgis-db" Layer URI (str "(" DeclaredSRS ")"))
  (with-sh-dir geoserver-data-dir
    (let [result (sh "shp2db"
                     (remove-epsg-prefix DeclaredSRS)
                     (extract-postgis-path URI)
                     Layer
                     (deref current-postgis-database)
                     postgis-user)]
      (if-not (zero? (:exit result))
        (println (:err result))))))

;; FIXME: Find a way to remove the hard-coded database name.
(defn remove-shapefile-from-postgis-db
  [{:keys [postgis-user]} {:keys [Layer]}]
  (println "remove-shapefile-from-postgis-db" Layer)
  (let [result (sh "psql" "-d" (deref current-postgis-database) "-U" postgis-user :in (str "DROP TABLE " Layer ";"))]
    (if-not (zero? (:exit result))
      (println (:err result)))))

(defn create-postgis-feature-type
  [config-params {:keys [Workspace Store Layer Description]}]
  (println "create-postgis-feature-type" (str Workspace ":" Store ":" Layer))
  ["POST"
   (str "/workspaces/" Workspace "/datastores/" Store "/featuretypes")
   (xml
    [:featureType
     [:name Layer]
     [:nativeName Layer]
     [:title Description]
     [:abstract Description]
     [:enabled "true"]
     [:maxFeatures "0"]
     [:numDecimals "0"]])])

(defn delete-postgis-feature-type
  [config-params {:keys [Workspace Store Layer]}]
  (println "delete-postgis-feature-type" (str Workspace ":" Store ":" Layer))
  ["DELETE"
   (str "/workspaces/" Workspace "/datastores/" Store "/featuretypes/" Layer)
   nil])

(defn create-shapefile-feature-type
  [config-params {:keys [Workspace Store Layer Description]}]
  (println "create-shapefile-feature-type" (str Workspace ":" Store ":" Layer))
  ["POST"
   (str "/workspaces/" Workspace "/datastores/" Store "/featuretypes")
   (xml
    [:featureType
     [:name Layer]
     [:nativeName Store]
     [:title Description]
     [:abstract Description]
     [:enabled "true"]
     [:maxFeatures "0"]
     [:numDecimals "0"]])])

(defn create-shapefile-feature-type-via-put
  [config-params {:keys [Workspace Store URI]}]
  (println "create-shapefile-feature-type-via-put" (str Workspace ":" Store))
  ["PUT"
   (str "/workspaces/" Workspace "/datastores/" Store "/external.shp?configure=first")
   URI])

(defn delete-shapefile-feature-type
  [config-params {:keys [Workspace Store Layer]}]
  (println "delete-shapefile-feature-type" (str Workspace ":" Store ":" Layer))
  ["DELETE"
   (str "/workspaces/" Workspace "/datastores/" Store "/featuretypes/" Layer)
   nil])

(defn create-coverage-store
  [config-params {:keys [Workspace Store Description URI]}]
  (println "create-coverage-store" (str Workspace ":" Store))
  ["POST"
   (str "/workspaces/" Workspace "/coveragestores")
   (xml
    [:coverageStore
     [:name Store]
     [:description Description]
     [:type "GeoTIFF"]
     [:enabled "true"]
     [:workspace
      [:name Workspace]]
     [:url URI]])])

(defn delete-coverage-store
  [config-params {:keys [Workspace Store]}]
  (println "delete-coverage-store" (str Workspace ":" Store))
  ["DELETE"
   (str "/workspaces/" Workspace "/coveragestores/" Store)
   nil])

(defn extract-filename
  [uri]
  (second (re-find #"^file:.*/([^/]*).tif$" uri)))

(defn extract-path
  [uri]
  (second (re-find #"^file:/raid/geodata/(.*)$" uri)))

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

(defn dms->dd
  [dms]
  (let [[d m s dir] (map read-string (rest (re-find #"^([ \d]+)d([ \d]+)'([ \.0123456789]+)\"(\w)$" dms)))
        unsigned-dd (+ d (/ m 60.0) (/ s 3600.0))]
    (if (#{'S 'W} dir)
      (- unsigned-dd)
      unsigned-dd)))

(defn radians->degrees
  [rads]
  (/ (* rads 180.0) Math/PI))

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
     :nativeCRS    (second (re-find native-crs-regex gdal-info))
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

(defn create-coverage
  [{:keys [geoserver-data-dir]} {:keys [Workspace Store Layer Description URI NativeSRS DeclaredSRS]}]
  (println "create-coverage" (str Workspace ":" Store ":" Layer))
  (let [gdal-info (extract-georeferences geoserver-data-dir URI)]
    ["POST"
     (str "/workspaces/" Workspace "/coveragestores/" Store "/coverages")
     (xml
      [:coverage
       [:name Layer]
       [:title Description]
       [:description Description]
       [:abstract Description]
       [:enabled "true"]
       [:keywords
        [:string "WCS"]
        [:string "GeoTIFF"]
        [:string (extract-filename URI)]]
       [:nativeCRS (:nativeCRS gdal-info)]
       [:srs DeclaredSRS]
       [:nativeBoundingBox
        [:minx (:native-min-x gdal-info)]
        [:maxx (:native-max-x gdal-info)]
        [:miny (:native-min-y gdal-info)]
        [:maxy (:native-max-y gdal-info)]
        (if (and NativeSRS (not= NativeSRS "UNKNOWN"))
          [:crs NativeSRS])
        ]
       [:latLonBoundingBox
        [:minx (:latlon-min-x gdal-info)]
        [:maxx (:latlon-max-x gdal-info)]
        [:miny (:latlon-min-y gdal-info)]
        [:maxy (:latlon-max-y gdal-info)]
        [:crs "EPSG:4326"]]
       [:projectionPolicy "REPROJECT_TO_DECLARED"]
       [:metadata
        [:entry {:key "cachingEnabled"} "false"]
        [:entry {:key "dirName"} (str Store "_" (extract-filename URI))]]
       [:nativeFormat "GeoTIFF"]
       [:grid {:dimension "2"}
        [:range
         [:low "0 0"]
         [:high (:cols-rows gdal-info)]]
        [:transform
         [:scaleX (:pixel-width  gdal-info)]
         [:scaleY (:pixel-height gdal-info)]
         [:shearX (:shear-x gdal-info)]
         [:shearY (:shear-y gdal-info)]
         [:translateX (:x-origin gdal-info)]
         [:translateY (:y-origin gdal-info)]]
        [:crs DeclaredSRS]]
       [:supportedFormats
        [:string "GIF"]
        [:string "PNG"]
        [:string "JPEG"]
        [:string "TIFF"]
        [:string "GEOTIFF"]]
       [:interpolationMethods
        [:string "bilinear"]
        [:string "bicubic"]]
       [:dimensions
        [:coverageDimension
         [:name (.toUpperCase (str (:color-interp gdal-info) "_INDEX"))]
         [:description "GridSampleDimension[-Infinity,Infinity]"]]]
       [:requestSRS
        [:string "EPSG:4326"]
        (if (not= DeclaredSRS "EPSG:4326")
          [:string DeclaredSRS])]
       [:responseSRS
        [:string "EPSG:4326"]
        (if (not= DeclaredSRS "EPSG:4326")
          [:string DeclaredSRS])]])]))

(defn delete-coverage
  [config-params {:keys [Workspace Store Layer]}]
  (println "delete-coverage" (str Workspace ":" Store ":" Layer))
  ["DELETE"
   (str "/workspaces/" Workspace "/coveragestores/" Store "/coverages/" Layer)
   nil])

(defn delete-layer
  [config-params {:keys [Workspace Store Layer]}]
  (println "delete-layer" (str Workspace ":" Store ":" Layer))
  ["DELETE"
   (str "/layers/" Layer)
   nil])

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

(defn translate-row
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
