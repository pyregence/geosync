(ns geosync.core
  (:import java.io.File)
  (:require [clj-http.client    :as client]
            [clojure.data.json  :as json]
            [clojure.java.io    :as io]
            [clojure.string     :as s]
            [geosync.rest-api   :as rest]
            [triangulum.logging :refer [log-str]]))

;;===========================================================
;;
;; Files -> WMS Requests
;;
;;===========================================================

(defn create-feature-type-spatial-index
  [{:keys [geoserver-rest-uri geoserver-workspace]} {:keys [store-name]}]
  (try
    (let [geoserver-wms-uri (as-> (s/replace geoserver-rest-uri "/rest" "/wms") %
                              (if (s/ends-with? % "/")
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
      (log-str "GetFeatureInfo " layer-name " -> " (select-keys response [:status :reason-phrase]))
      response)
    (catch Exception e
      (let [layer-name (str geoserver-workspace ":" store-name)]
        (log-str "GetFeatureInfo "
                 layer-name
                 " -> "
                 (select-keys (ex-data e) [:status :reason-phrase :body]))
        (ex-data e)))))

;;===========================================================
;;
;; Files -> REST Requests
;;
;;===========================================================

(def success-code? #{200 201 202 203 204 205 206 207 300 301 302 303 307})

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request
  [{:keys [geoserver-rest-uri geoserver-auth-code]} [http-method uri-suffix http-body]]
  (try
    (let [geoserver-rest-uri (if (s/ends-with? geoserver-rest-uri "/")
                               (subs geoserver-rest-uri 0 (dec (count geoserver-rest-uri)))
                               geoserver-rest-uri)
          response           (client/request {:url       (str geoserver-rest-uri uri-suffix)
                                              :method    http-method
                                              :insecure? true
                                              :headers   {"Content-Type"  "text/xml"
                                                          "Accept"        "application/json"
                                                          "Authorization" geoserver-auth-code}
                                              :body      http-body})]
      (log-str (format "%6s %s%n    -> %s"
                       http-method
                       uri-suffix
                       (select-keys response [:status :reason-phrase])))
      response)
    (catch Exception e
      (do (log-str (format "%6s %s%n    -> %s"
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
                 (when-let [matching-layers (seq (filter #(s/includes? % layer-pattern)
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
    (subs % 0 (s/last-index-of % \.))
    (s/replace % #"[^0-9a-zA-Z/\-_]" "-")
    (s/replace % #"-+" "-")
    (s/replace % "/" "_")))

(defn file-path->layer-name
  [file-path]
  (let [file-name (if (s/includes? file-path "/")
                    (second (re-find #"^.*/([^/]+)$" file-path))
                    file-path)]
    (subs file-name 0 (s/last-index-of file-name \.))))

(defn file-path->file-url
  [file-path data-dir]
  (str "file://" data-dir (if (s/ends-with? data-dir "/") "" "/") file-path))

(defn get-style
  [file-path store-type styles]
  (first
   (keep (fn [{:keys [layer-pattern raster-style vector-style]}]
           (when (s/includes? file-path layer-pattern)
             (case store-type
               :geotiff   raster-style
               :shapefile vector-style
               nil)))
         styles)))

(defn has-spatial-index?
  [file-path data-dir]
  (let [file-path-sans-extension (subs file-path 0 (s/last-index-of file-path \.))
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
  (let [data-dir (if (s/ends-with? data-dir "/")
                   data-dir
                   (str data-dir "/"))]
    (->> (io/file data-dir)
         (file-seq)
         (filter #(.isFile ^File %))
         (map #(-> (.getPath ^File %)
                   (s/replace-first data-dir "")))
         (sort))))

(defn add-directory-to-workspace!
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
    (log-str "\nFinished updating GeoServer.\nSuccessful requests:"
             (count (filter success-code? http-response-codes))
             "\nFailed requests:"
             (count (remove success-code? http-response-codes)))))
