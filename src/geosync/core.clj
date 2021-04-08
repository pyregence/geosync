(ns geosync.core
  (:import java.io.File)
  (:require [clj-http.client    :as client]
            [clojure.data.json  :as json]
            [clojure.java.io    :as io]
            [clojure.string     :as s]
            [geosync.rest-api   :as rest]
            [triangulum.logging :refer [log log-str]]
            [taoensso.tufte     :as tufte]))

;;===========================================================
;;
;; Files -> WMS Requests
;;
;;===========================================================

(defn create-feature-type-spatial-index
  [{:keys [geoserver-wms-uri geoserver-workspace]} {:keys [store-name]}]
  (try
    (let [layer-name (str geoserver-workspace ":" store-name)
          response   (client/request {:url       (str geoserver-wms-uri
                                                      "&LAYERS=" layer-name
                                                      "&QUERY_LAYERS=" layer-name)
                                      :method    "GET"
                                      :insecure? true})]
      (log-str "GetFeatureInfo " layer-name " -> " (select-keys response [:status :reason-phrase]))
      response)
    (catch Exception e
      (let [layer-name (str geoserver-workspace ":" store-name)]
        (log-str "GetFeatureInfo " layer-name " -> " (select-keys (ex-data e) [:status :reason-phrase :body]))
        (ex-data e)))))

(defn file-specs->wms-specs
  [file-specs]
  (filterv #(and (= :shapefile (:store-type %))
                 (not (:indexed? %)))
           file-specs))

;;===========================================================
;;
;; Files -> REST Requests
;;
;;===========================================================

(def success-code? #{200 201 202 203 204 205 206 207 300 301 302 303 307})

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request
  [{:keys [geoserver-rest-uri geoserver-rest-headers]} [http-method uri-suffix http-body]]
  (try
    (let [response (client/request {:url       (str geoserver-rest-uri uri-suffix)
                                    :method    http-method
                                    :headers   geoserver-rest-headers
                                    :body      http-body
                                    :insecure? true})]
      (log-str (format "%6s %s%n               -> %s"
                       http-method
                       uri-suffix
                       (select-keys response [:status :reason-phrase])))
      response)
    (catch Exception e
      (do (log-str (format "%6s %s%n               -> %s"
                           http-method
                           uri-suffix
                           (select-keys (ex-data e) [:status :reason-phrase :body])))
          (ex-data e)))))

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request-async
  [{:keys [geoserver-rest-uri geoserver-rest-headers]} [http-method uri-suffix http-body]]
  (let [result (promise)]
    (client/request {:url       (str geoserver-rest-uri uri-suffix)
                     :method    http-method
                     :headers   geoserver-rest-headers
                     :body      http-body
                     :insecure? true
                     :async?    true}
                    (fn [response]
                      (log-str (format "%6s %s%n               -> %s"
                                       http-method
                                       uri-suffix
                                       (select-keys response [:status :reason-phrase])))
                      (deliver result response))
                    (fn [error]
                      (log-str (format "%6s %s%n               -> %s"
                                       http-method
                                       uri-suffix
                                       (select-keys (ex-data error) [:status :reason-phrase :body])))
                      (deliver result (ex-data error))))
    result))

(defn file-specs->layer-group-specs
  [{:keys [geoserver-workspace layer-groups]} existing-layer-groups file-specs]
  (let [layer-names (mapv #(str geoserver-workspace ":" (:store-name %)) file-specs)]
    (into []
          (comp (remove #(contains? existing-layer-groups (:name %)))
                (keep (fn [{:keys [layer-pattern name]}]
                        (when-let [matching-layers (seq (filterv #(s/includes? % layer-pattern)
                                                                 layer-names))]
                          (rest/create-layer-group geoserver-workspace
                                                   name
                                                   "SINGLE"
                                                   name
                                                   ""
                                                   []
                                                   matching-layers
                                                   [])))))
          layer-groups)))

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

      :shapefile (doall
                  (concat
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
                     [(rest/update-layer-style geoserver-workspace store-name style :vector)])))

      (throw (ex-info "Unsupported store type detected."
                      {:store-type store-type :file-url file-url})))))

(defn file-specs->layer-specs
  [config-params existing-stores file-specs]
  (into []
        (comp (mapcat (partial file-spec->layer-specs
                               config-params
                               existing-stores))
              (remove nil?))
        file-specs))

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

(defn get-spec-type
  [[http-method uri-suffix _]]
  (cond (and (= http-method "PUT")    (s/includes?  uri-suffix "/coveragestores/")) :create-coverage-via-put
        (and (= http-method "PUT")    (s/includes?  uri-suffix "/layers/"))         :update-layer-style
        (and (= http-method "POST")   (s/ends-with? uri-suffix "/datastores"))      :create-data-store
        (and (= http-method "PUT")    (s/includes?  uri-suffix "/datastores/"))     :create-feature-type-via-put
        (and (= http-method "POST")   (s/ends-with? uri-suffix "/featuretypes"))    :create-feature-type-alias
        (and (= http-method "DELETE") (s/includes?  uri-suffix "/layers/"))         :delete-layer
        (and (= http-method "DELETE") (s/includes?  uri-suffix "/featuretypes/"))   :delete-feature-type))

(defn file-specs->rest-specs
  "Generates a sequence of REST request specifications as triplets of
  [http-method uri-suffix http-body]. Each file-spec may contribute
  one or more of these to the final sequence. Returns a map of these
  REST specs grouped by spec type."
  [{:keys [geoserver-workspace] :as config-params} file-specs]
  (let [ws-exists?            (workspace-exists? config-params)
        existing-stores       (if ws-exists? (get-existing-stores config-params) #{})
        existing-layer-groups (if ws-exists? (get-existing-layer-groups config-params) #{})
        layer-specs           (file-specs->layer-specs config-params existing-stores file-specs)
        layer-group-specs     (file-specs->layer-group-specs config-params existing-layer-groups file-specs)
        rest-specs            (-> (group-by get-spec-type layer-specs)
                                  (assoc :create-layer-group layer-group-specs))]
    (if ws-exists?
      rest-specs
      (assoc rest-specs :create-workspace [(rest/create-workspace geoserver-workspace)]))))

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
  (into []
        (keep #(when-let [store-type (get-store-type %)]
                 (array-map :store-type store-type
                            :store-name (file-path->store-name %)
                            :layer-name (file-path->layer-name %)
                            :file-url   (file-path->file-url % data-dir)
                            :style      (get-style % store-type styles)
                            :indexed?   (has-spatial-index? % data-dir))))
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

(defn make-parallel-rest-requests
  [config-params rest-specs]
  (->> rest-specs
       (mapv #(make-rest-request-async config-params %))
       (mapv (comp :status deref))))

(defn add-directory-to-workspace-aux!
  [{:keys [data-dir styles] :as config-params}]
  (tufte/profile
   {:id :add-directory-to-workspace!}
   (let [file-specs          (tufte/p :file-specs
                                      (->> (load-file-paths data-dir)
                                           (file-paths->file-specs data-dir styles)))
         rest-specs          (tufte/p :rest-specs
                                      (file-specs->rest-specs config-params file-specs))
         wms-specs           (tufte/p :wms-specs
                                      (file-specs->wms-specs file-specs))
         rest-response-codes (tufte/p :rest-requests
                                      (client/with-async-connection-pool {:insecure? true}
                                        ;; FIXME: Try :default-per-route
                                        (into []
                                              (mapcat (fn [spec-type]
                                                        (->> (get rest-specs spec-type)
                                                             (make-parallel-rest-requests config-params))))
                                              [:create-workspace
                                               :create-coverage-via-put
                                               :create-data-store
                                               :create-feature-type-via-put
                                               :create-feature-type-alias
                                               :delete-layer
                                               :delete-feature-type
                                               :update-layer-style
                                               :create-layer-group])))
         wms-response-codes  (tufte/p :wms-requests
                                      (client/with-connection-pool {:insecure? true}
                                        ;; FIXME: Try :threads :default-per-route
                                        (mapv #(:status (create-feature-type-spatial-index config-params %))
                                              wms-specs)))
         http-response-codes (into rest-response-codes wms-response-codes)
         num-success-codes   (count (filter success-code? http-response-codes))
         num-failure-codes   (- (count http-response-codes) num-success-codes)]
     (log-str "\nFinished updating GeoServer."
              "\nSuccessful requests: " num-success-codes
              "\nFailed requests: " num-failure-codes)
     (zero? num-failure-codes)))) ; Return true if successful

(defn add-directory-to-workspace!
  [config-params]
  (let [stats-accumulator (do
                            (tufte/remove-handler! :accumulating)
                            (tufte/add-accumulating-handler! {:handler-id :accumulating}))
        success?          (add-directory-to-workspace-aux! config-params)]
    (Thread/sleep 1000)
    (log (tufte/format-grouped-pstats @stats-accumulator
                                      {:format-pstats-opts {:columns [:n-calls :min :max
                                                                      :mean :mad :clock :total]}})
         :truncate? false)
    success?))

(defn remove-workspace!
  [{:keys [geoserver-workspace] :as config-params}]
  (let [response (make-rest-request config-params
                                    (rest/delete-workspace geoserver-workspace true))]
    (success-code? (:status response)))) ; Return true if successful
