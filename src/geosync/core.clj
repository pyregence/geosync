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
;;; GeoSync is a simple command-line application that traverses a
;;; directory of raster and vector GIS files (e.g., GeoTIFFs, Shapefiles)
;;; and generates the necessary REST commands to add workspaces,
;;; coveragestores, datastores, coverages, featuretypes, layers, and
;;; layergroups for each file to a running GeoServer instance.

(ns geosync.core
  (:require [clojure.edn       :as edn]
            [clojure.java.io   :as io]
            [clojure.string    :as str]
            [clojure.data.json :as json]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-http.client   :as client]
            [geosync.rest-api  :as rest])
  (:import java.util.Base64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Files -> WMS Requests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-feature-type-spatial-index
  [{:keys [geoserver-rest-uri geoserver-workspace]} {:keys [store-name]}]
  (try
    (let [geoserver-wms-uri (as-> (str/replace geoserver-rest-uri "/rest" "/wms") %
                              (if (str/ends-with? % "/")
                                (subs % 0 (dec (count %)))
                                %))
          layer-name        (str geoserver-workspace ":" store-name)
          response          (client/request {:url       (str geoserver-wms-uri
                                                             "?SERVICE=WMS"
                                                             "&VERSION=1.3.0"
                                                             "&REQUEST=GetFeatureInfo"
                                                             "&INFO_FORMAT=application/json"
                                                             "&LAYERS=" layer-name
                                                             "&QUERY_LAYERS=" layer-name
                                                             "&FEATURE_COUNT=1"
                                                             "&TILED=true"
                                                             "&I=0"
                                                             "&J=0"
                                                             "&WIDTH=1"
                                                             "&HEIGHT=1"
                                                             "&CRS=EPSG:4326"
                                                             "&BBOX=-180.0,-90.0,180.0,90.0")
                                             :method    "GET"
                                             :insecure? true})]
      (println "GetFeatureInfo" layer-name "->" (select-keys response [:status :reason-phrase]))
      response)
    (catch Exception e
      (let [layer-name (str geoserver-workspace ":" store-name)]
        (println "GetFeatureInfo"
                 layer-name
                 "->"
                 (select-keys (ex-data e) [:status :reason-phrase :body]))
        (ex-data e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Files -> REST Requests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def success-code? #{200 201 202 203 204 205 206 207 300 301 302 303 307})

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request
  [{:keys [geoserver-rest-uri geoserver-auth-code]} [http-method uri-suffix http-body]]
  (try
    (let [geoserver-rest-uri (if (str/ends-with? geoserver-rest-uri "/")
                               (subs geoserver-rest-uri 0 (dec (count geoserver-rest-uri)))
                               geoserver-rest-uri)
          response           (client/request {:url       (str geoserver-rest-uri uri-suffix)
                                              :method    http-method
                                              :insecure? true
                                              :headers   {"Content-Type"  "text/xml"
                                                          "Accept"        "application/json"
                                                          "Authorization" geoserver-auth-code}
                                              :body      http-body})]
      (println (format "%6s %s%n    -> %s"
                       http-method
                       uri-suffix
                       (select-keys response [:status :reason-phrase])))
      response)
    (catch Exception e
      (do (println (format "%6s %s%n    -> %s"
                           http-method
                           uri-suffix
                           (select-keys (ex-data e) [:status :reason-phrase :body])))
          (ex-data e)))))

(defn file-spec->layer-specs
  "Returns a sequence of one or more REST request specifications as
  triplets of [http-method uri-suffix http-body] depending on the
  structure of the passed-in file-spec or nil if the store-type is
  unsupported."
  [{:keys [data-dir geoserver-workspace]}
   existing-stores
   {:keys [store-type store-name layer-name file-url style]}]
  (when-not (contains? existing-stores store-name)
    (case store-type
      :geotiff   [(rest/create-coverage-via-put geoserver-workspace store-name file-url)
                  (when style
                    (rest/update-layer-style geoserver-workspace store-name style :raster))]

      :shapefile (concat
                  [(rest/create-data-store geoserver-workspace store-name file-url)
                   (rest/create-feature-type-via-put geoserver-workspace store-name file-url)]
                  (when (not= store-name layer-name)
                    [(rest/create-feature-type-alias geoserver-workspace
                                                     store-name
                                                     layer-name
                                                     store-name)
                     (rest/delete-layer geoserver-workspace layer-name)
                     (rest/delete-feature-type geoserver-workspace store-name layer-name)])
                  (when style
                    [(rest/update-layer-style geoserver-workspace store-name style :vector)]))

      (throw (ex-info "Unsupported store type detected."
                      {:store-type store-type :file-url file-url})))))

(defn file-specs->layer-group-specs
  [{:keys [geoserver-workspace layer-groups]} existing-layer-groups file-specs]
  (let [layer-names (map #(str geoserver-workspace ":" (:store-name %)) file-specs)]
    (->> layer-groups
         (remove #(contains? existing-layer-groups (:name %)))
         (keep (fn [{:keys [layer-pattern name]}]
                 (when-let [matching-layers (seq (filter #(str/includes? % layer-pattern)
                                                         layer-names))]
                   (rest/create-layer-group geoserver-workspace
                                            name
                                            "SINGLE"
                                            name
                                            ""
                                            []
                                            matching-layers
                                            [])))))))

(defn get-existing-layer-groups
  [{:keys [geoserver-workspace] :as config-params}]
  (as-> (rest/get-layer-groups geoserver-workspace) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str % :key-fn keyword)
    (:layerGroups %)
    (:layerGroup %)
    (map :name %)
    (set %)))

;; FIXME: unused
(defn get-existing-layers
  [{:keys [geoserver-workspace] :as config-params}]
  (as-> (rest/get-layers geoserver-workspace) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str % :key-fn keyword)
    (:layers %)
    (:layer %)
    (map :name %)
    (set %)))

(defn get-existing-coverage-stores
  [{:keys [geoserver-workspace] :as config-params}]
  (as-> (rest/get-coverage-stores geoserver-workspace) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str % :key-fn keyword)
    (:coverageStores %)
    (:coverageStore %)
    (map :name %)
    (set %)))

(defn get-existing-data-stores
  [{:keys [geoserver-workspace] :as config-params}]
  (as-> (rest/get-data-stores geoserver-workspace) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str % :key-fn keyword)
    (:dataStores %)
    (:dataStore %)
    (map :name %)
    (set %)))

(defn get-existing-stores
  [config-params]
  (into (get-existing-coverage-stores config-params)
        (get-existing-data-stores     config-params)))

(defn workspace-exists?
  [{:keys [geoserver-workspace] :as config-params}]
  (as-> (rest/get-workspace geoserver-workspace) %
    (make-rest-request config-params %)
    (:status %)
    (success-code? %)))

(defn file-specs->rest-specs
  "Generates a sequence of REST request specifications as triplets of
  [http-method uri-suffix http-body]. Each file-spec may contribute
  one or more of these to the final sequence."
  [{:keys [geoserver-workspace] :as config-params} file-specs]
  (let [ws-exists?            (workspace-exists? config-params)
        existing-stores       (if ws-exists? (get-existing-stores config-params) #{})
        existing-layer-groups (if ws-exists? (get-existing-layer-groups config-params) #{})
        layer-specs           (->> file-specs
                                   (mapcat (partial file-spec->layer-specs
                                                    config-params
                                                    existing-stores))
                                   (remove nil?))
        layer-group-specs     (file-specs->layer-group-specs config-params
                                                             existing-layer-groups
                                                             file-specs)
        rest-specs            (concat layer-specs layer-group-specs)]
    (if ws-exists?
      rest-specs
      (cons (rest/create-workspace geoserver-workspace) rest-specs))))

(defn get-store-type
  "Returns a string describing the class of data or coverage store
  implied by the structure of the passed-in file-path."
  [file-path]
  (condp re-matches file-path
    #"^.*\.tiff?$" :geotiff
    #"^.*\.shp$"   :shapefile
    nil))

(defn file-path->store-name
  [file-path]
  (as-> file-path %
    (subs % 0 (str/last-index-of % \.))
    (str/replace % #"[^0-9a-zA-Z/\-_]" "-")
    (str/replace % #"-+" "-")
    (str/replace % "/" "_")))

(defn file-path->layer-name
  [file-path]
  (let [file-name (if (str/includes? file-path "/")
                    (second (re-find #"^.*/([^/]+)$" file-path))
                    file-path)]
    (subs file-name 0 (str/last-index-of file-name \.))))

(defn file-path->file-url
  [file-path data-dir]
  (str "file://" data-dir (if (str/ends-with? data-dir "/") "" "/") file-path))

(defn get-style
  [file-path store-type styles]
  (first
   (keep (fn [{:keys [layer-pattern raster-style vector-style]}]
           (when (str/includes? file-path layer-pattern)
             (case store-type
               :geotiff   raster-style
               :shapefile vector-style
               nil)))
         styles)))

(defn has-spatial-index?
  [file-path data-dir]
  (let [file-path-sans-extension (subs file-path 0 (str/last-index-of file-path \.))
        spatial-index-file-path  (str file-path-sans-extension ".qix")]
    (.exists (io/file data-dir spatial-index-file-path))))

(defn file-paths->file-specs
  [data-dir styles file-paths]
  (keep #(when-let [store-type (get-store-type %)]
           (array-map :store-type store-type
                      :store-name (file-path->store-name %)
                      :layer-name (file-path->layer-name %)
                      :file-url   (file-path->file-url % data-dir)
                      :style      (get-style % store-type styles)
                      :indexed?   (has-spatial-index? % data-dir)))
        file-paths))

(defn load-file-paths
  [data-dir]
  (let [data-dir (if (str/ends-with? data-dir "/")
                   data-dir
                   (str data-dir "/"))]
    (->> (io/file data-dir)
         (file-seq)
         (filter #(.isFile %))
         (map #(-> (.getPath %)
                   (str/replace-first data-dir "")))
         (sort))))

(defn update-geoserver!
  [{:keys [data-dir styles] :as config-params}]
  (let [file-specs          (->> (load-file-paths data-dir)
                                 (file-paths->file-specs data-dir styles))
        rest-response-codes (client/with-connection-pool {:insecure? true}
                              (mapv (comp :status (partial make-rest-request config-params))
                                    (file-specs->rest-specs config-params file-specs)))
        wms-response-codes  (client/with-connection-pool {:insecure? true}
                              (mapv (comp :status
                                          (partial create-feature-type-spatial-index config-params))
                                    (filter #(and (= :shapefile (:store-type %))
                                                  (not (:indexed? %)))
                                            file-specs)))
        http-response-codes (concat rest-response-codes wms-response-codes)]
    (println "\nFinished updating GeoServer.\nSuccessful requests:"
             (count (filter success-code? http-response-codes))
             "\nFailed requests:"
             (count (remove success-code? http-response-codes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def base64-encoder (Base64/getUrlEncoder))

(defn encode-str
  [s]
  (.encodeToString base64-encoder (.getBytes s)))

;; FIXME: Use clojure.spec to validate the config-params map
(defn read-config-params
  [config-file-path]
  (if config-file-path
    (let [config-params (edn/read-string (slurp config-file-path))]
      (if (and (map? config-params)
               (every? keyword? (keys config-params)))
        config-params
        (throw (ex-info "The config-file must contain an EDN map whose keys are keywords."
                        {"--config-file" config-file-path}))))
    {}))

;; FIXME: Use clojure.spec to validate the config-params map
(defn process-options
  [options]
  (let [config-file-params  (read-config-params (:config-file options))
        command-line-params (into {} (remove (comp str/blank? val) (dissoc options :config-file)))
        config-params       (merge config-file-params command-line-params)]
    (assoc config-params
           :geoserver-auth-code
           (str "Basic " (encode-str (str (:geoserver-username config-params)
                                          ":"
                                          (:geoserver-password config-params)))))))

(def cli-options
  [["-c" "--config-file EDN"         "Path to an EDN file containing a map of these parameters"]
   ["-d" "--data-dir DIR"            "Path to the directory containing your GIS files"]
   ["-g" "--geoserver-rest-uri URI"  "URI of your GeoServer's REST extensions"]
   ["-u" "--geoserver-username USER" "GeoServer admin username"]
   ["-p" "--geoserver-password PASS" "GeoServer admin password"]
   ["-w" "--geoserver-workspace WS"  "Workspace name to receive the new GeoServer layers"]])

(defn -main
  "Call this with the name of an EDN file containing the config-params
  map. The params will be read into a hash-map and passed on to the
  update-geoserver! function. So that we only have to calculate it
  once, the geoserver-auth-code is generated here from
  the :geoserver-username and :geoserver-password fields in the
  passed-in map and added to the in-memory hash-map under
  the :geoserver-auth-code entry."
  [& args]
  (println (str "geosync: Load a nested directory tree of GeoTIFFs and Shapefiles into "
                "a running GeoServer instance.\n"
                "Copyright 2020 Gary W. Johnson (gjohnson@sig-gis.com)\n"))
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    ;; {:options   The options map, keyed by :id, mapped to the parsed value
    ;;  :arguments A vector of unprocessed arguments
    ;;  :summary   A string containing a minimal options summary
    ;;  :errors    A vector of error message strings thrown during parsing; nil when no errors exist
    (if (or (seq errors) (empty? options))
      (do
        (when (seq errors)
          (run! println errors)
          (newline))
        (println (str "Usage:\n" summary)))
      (update-geoserver! (process-options options))))
  ;; Exit cleanly
  (shutdown-agents)
  (flush)
  (System/exit 0))
