(ns geosync.core
  (:import java.io.File
           java.net.SocketTimeoutException
           java.util.concurrent.TimeoutException
           java.util.Properties)
  (:require [clj-http.client     :as client]
            [clojure.data.json   :as json]
            [clojure.java.io     :as io]
            [clojure.string      :as s]
            [geosync.rest-api    :as rest]
            [geosync.utils       :refer [nil-on-error url-path]]
            [triangulum.logging  :refer [log log-str]]
            [triangulum.database :refer [call-sql]]
            [taoensso.tufte      :as tufte]))

;;===========================================================
;;
;; Files -> WMS Requests
;;
;;===========================================================

(def timeout-ms (* 20 60 1000)) ; 20 minutes

(defn timeout? [e]
  (or (instance? TimeoutException e)
      (instance? SocketTimeoutException e)))

(defn create-feature-type-spatial-index
  [{:keys [geoserver-wms-uri geoserver-workspace]} {:keys [store-name]}]
  (try
    (let [layer-name (str geoserver-workspace ":" store-name)
          response   (client/request {:url                (str geoserver-wms-uri
                                                               "&LAYERS=" layer-name
                                                               "&QUERY_LAYERS=" layer-name)
                                      :method             "GET"
                                      :insecure?          true
                                      :socket-timeout     timeout-ms
                                      :connection-timeout timeout-ms})]
      (log-str (format "GetFeatureInfo %s%n               -> %s"
                       layer-name
                       (select-keys response [:status :reason-phrase])))
      response)
    (catch Exception e
      (let [layer-name (str geoserver-workspace ":" store-name)]
        (log-str (format "GetFeatureInfo %s%n               -> %s"
                         layer-name
                         (if (timeout? e)
                           (str "Timeout Error: Your request took longer than " (quot timeout-ms 1000) " seconds.")
                           (select-keys (ex-data e) [:status :reason-phrase :body]))))
        (ex-data e)))))

(defn create-feature-type-spatial-index-async
  [{:keys [geoserver-wms-uri geoserver-workspace]} {:keys [store-name]}]
  (let [layer-name (str geoserver-workspace ":" store-name)
        result     (promise)]
    (client/request {:url                (str geoserver-wms-uri "&LAYERS=" layer-name "&QUERY_LAYERS=" layer-name)
                     :method             "GET"
                     :insecure?          true
                     :async?             true
                     :socket-timeout     timeout-ms
                     :connection-timeout timeout-ms}
                    (fn [response]
                      (log-str (format "GetFeatureInfo %s%n               -> %s"
                                       layer-name
                                       (select-keys response [:status :reason-phrase])))
                      (deliver result response))
                    (fn [error]
                      (log-str (format "GetFeatureInfo %s%n               -> %s"
                                       layer-name
                                       (if (timeout? error)
                                         (str "Timeout Error: Your request took longer than " (quot timeout-ms 1000) " seconds.")
                                         (select-keys (ex-data error) [:status :reason-phrase :body]))))
                      (deliver result (ex-data error))))
    result))

(defn file-specs->wms-specs
  [file-specs]
  (filterv #(and (= :shapefile (:store-type %))
                 (not (:indexed? %)))
           file-specs))

(defn file-specs->gwc-specs
  [file-specs]
  (filterv #(= :imagemosaic (:store-type %)) file-specs))

;;===========================================================
;;
;; Files -> REST Requests
;;
;;===========================================================

(def success-code? #{200 201 202 203 204 205 206 207 300 301 302 303 307})

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request
  [{:keys [geoserver-rest-uri geoserver-rest-headers]} [http-method uri-suffix http-body content-type]]
  (try
    (let [response (client/request {:url                (url-path geoserver-rest-uri uri-suffix)
                                    :method             http-method
                                    :headers            (if content-type
                                                          (assoc geoserver-rest-headers "Content-Type" content-type)
                                                          geoserver-rest-headers)
                                    :body               http-body
                                    :insecure?          true
                                    :socket-timeout     timeout-ms
                                    :connection-timeout timeout-ms})]
      (log-str (format "%6s %s%n               -> %s"
                       http-method
                       uri-suffix
                       (select-keys response [:status :reason-phrase])))
      response)
    (catch Exception e
      (log-str (format "%6s %s%n               -> %s"
                       http-method
                       uri-suffix
                       (if (timeout? e)
                         (str "Timeout Error: Your request took longer than " (quot timeout-ms 1000) " seconds.")
                         (select-keys (ex-data e) [:status :reason-phrase :body]))))
      (ex-data e))))

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request-async
  [{:keys [geoserver-rest-uri geoserver-rest-headers]} [http-method uri-suffix http-body content-type]]
  (let [result (promise)]
    (client/request {:url                (url-path geoserver-rest-uri uri-suffix)
                     :method             http-method
                     :headers            (if content-type
                                           (assoc geoserver-rest-headers "Content-Type" content-type)
                                           geoserver-rest-headers)
                     :body               http-body
                     :insecure?          true
                     :async?             true
                     :socket-timeout     timeout-ms
                     :connection-timeout timeout-ms}
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
                                       (if (timeout? error)
                                         (str "Timeout Error: Your request took longer than " (quot timeout-ms 1000) " seconds.")
                                         (select-keys (ex-data error) [:status :reason-phrase :body]))))
                      (deliver result (ex-data error))))
    result))

(defn file-specs->layer-group-specs
  [{:keys [geoserver-workspace layer-groups]} existing-stores existing-layer-groups file-specs]
  (let [store-names     (map :store-name file-specs)
        old-layer-names (->> store-names
                             (filter #(contains? existing-stores %))
                             (mapv #(str geoserver-workspace ":" %)))
        new-layer-names (->> store-names
                             (remove #(contains? existing-stores %))
                             (mapv #(str geoserver-workspace ":" %)))]
    (into []
          (keep (fn [{:keys [layer-pattern name]}]
                  (when-let [new-matching-layers (seq (filterv #(s/includes? % layer-pattern)
                                                               new-layer-names))]
                    (if (contains? existing-layer-groups name)
                      (let [old-matching-layers (seq (filterv #(s/includes? % layer-pattern)
                                                              old-layer-names))]
                        (rest/delete-layer-group geoserver-workspace name)
                        (rest/create-layer-group geoserver-workspace
                                                 name
                                                 "SINGLE"
                                                 name
                                                 ""
                                                 []
                                                 (sort (concat old-matching-layers new-matching-layers))
                                                 []))
                      (rest/create-layer-group geoserver-workspace
                                               name
                                               "SINGLE"
                                               name
                                               ""
                                               []
                                               (sort new-matching-layers)
                                               [])))))
          layer-groups)))

(defn update-properties-file!
  [file-path attribute value]
  (let [props (with-open [reader (io/reader file-path)]
                (doto (Properties.) (.load reader)))]
    (with-open [writer (io/writer file-path)]
      (doto ^Properties props
        (.setProperty attribute value)
        (.store writer nil)))))

(defn clean-image-mosaic-folder
  "GeoServer likes to add extra config files to an ImageMosaic directory.
   This function deletes them (and keeps the files we care about)."
  [data-dir]
  (doseq [^File file (file-seq (io/file data-dir))
          :when (let [file-name (.getName file)]
                  (not (or (.isDirectory file)
                           (s/ends-with? file-name ".tif")
                           (#{"datastore.properties" "timeregex.properties" "indexer.properties"} file-name))))]
    (io/delete-file file)))

(defn file-spec->layer-specs
  "Returns a sequence of one or more REST request specifications as
  tuples of [http-method uri-suffix http-body content-type] depending
  on the structure of the passed-in file-spec or nil if the store-type
  is unsupported."
  [{:keys [geoserver-workspace]}
   existing-stores
   {:keys [store-type store-name layer-name file-url style]}]
  (when-not (contains? existing-stores store-name)
    (case store-type
      :geotiff     [(rest/create-coverage-via-put geoserver-workspace store-name file-url)
                    (when style
                      (rest/update-layer-style geoserver-workspace store-name style :raster))]

      :shapefile   (doall
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

      :imagemosaic (do (update-properties-file! (str file-url "/datastore.properties") "schema" geoserver-workspace)
                       (update-properties-file! (str file-url "/indexer.properties") "Name" store-name)
                       (clean-image-mosaic-folder (s/replace file-url "file://" ""))
                       [(rest/create-coverage-store-image-mosaic geoserver-workspace store-name file-url)
                        (rest/update-coverage-store-image-mosaic geoserver-workspace store-name file-url)
                        (rest/create-coverage-image-mosaic geoserver-workspace store-name)
                        (when style
                          (rest/update-layer-style geoserver-workspace store-name style :raster))])

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

(defn get-existing-styles
  [{:keys [geoserver-workspace] :as config-params}]
  (let [response (make-rest-request config-params (rest/get-styles geoserver-workspace))
        success? (success-code? (:status response))]
    (if success?
      (as-> (:body response) %
        (json/read-str % :key-fn keyword)
        (:styles %)
        (:style %)
        (map :name %)
        (set %))
      (hash-set))))

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

(defn get-existing-gwc-layer
  [{:keys [geoserver-workspace] :as config-params} store-name]
  (as-> (rest/get-cached-layer geoserver-workspace store-name) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str % :key-fn keyword)
    (:GeoServerLayer %)))

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

(defn get-existing-workspaces
  [config-params]
  (as-> (rest/get-workspaces) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str % :key-fn keyword)
    (:workspaces %)
    (:workspace %)))

(defn workspace-exists?
  [{:keys [geoserver-workspace] :as config-params}]
  (as-> (rest/get-workspace geoserver-workspace) %
    (make-rest-request config-params %)
    (:status %)
    (success-code? %)))

(defn get-spec-type
  [[http-method uri-suffix]]
  (let [uri-without-query-params (first (s/split uri-suffix #"\?"))]
    (nil-on-error
     (case http-method
       "POST"   (condp #(s/ends-with? %2 %1) uri-without-query-params
                  "external.imagemosaic" :update-coverage-store-image-mosaic
                  "/coverages"           :create-coverage
                  "/datastores"          :create-data-store
                  "/featuretypes"        :create-feature-type-alias
                  "/styles"              :create-style)
       "PUT"    (condp #(s/includes? %2 %1) uri-suffix
                  "external.imagemosaic" :create-coverage-store-image-mosaic
                  "external.geotiff"     :create-coverage-via-put
                  "external.shp"         :create-feature-type-via-put
                  "/coveragestores/"     :update-coverage-store
                  "/gwc/rest/layers/"    :update-cached-layer
                  "/layers/"             :update-layer-style
                  "/styles"              :update-style)
       "DELETE" (condp #(s/includes? %2 %1) uri-suffix
                  "/layers/"             :delete-layer
                  "/featuretypes/"       :delete-feature-type)))))

(defn get-style-name
  [file-path]
  (-> file-path
      (io/file)
      (.getName)
      (s/split #"\.")
      (first)))

(defn file-path->style-spec
  [{:keys [geoserver-workspace overwrite-styles]} file-path existing-styles]
  (let [style-name (get-style-name file-path)
        exists?    (contains? existing-styles style-name)]
    (cond
      (not exists?)                  (rest/create-style geoserver-workspace style-name file-path)
      (and exists? overwrite-styles) (rest/update-style geoserver-workspace style-name file-path)
      :else                          nil)))

(defn file-paths->style-specs
  [config-params style-file-paths existing-styles]
  (keep #(file-path->style-spec config-params % existing-styles) style-file-paths))

(defn file-specs->rest-specs
  "Generates a sequence of REST request specifications as tuples of
  [http-method uri-suffix http-body content-type]. Each file-spec may
  contribute one or more of these to the final sequence. Returns a map
  of these REST specs grouped by spec type."
  [{:keys [geoserver-workspace] :as config-params} gis-file-specs style-file-paths]
  (let [ws-exists?            (workspace-exists? config-params)
        existing-stores       (if ws-exists? (get-existing-stores config-params) #{})
        existing-layer-groups (if ws-exists? (get-existing-layer-groups config-params) #{})
        existing-styles       (if ws-exists? (get-existing-styles config-params) #{})
        layer-specs           (file-specs->layer-specs config-params existing-stores gis-file-specs)
        style-specs           (file-paths->style-specs config-params style-file-paths existing-styles)
        layer-group-specs     (file-specs->layer-group-specs config-params existing-stores existing-layer-groups gis-file-specs)
        rest-specs            (-> (group-by get-spec-type (concat layer-specs style-specs))
                                  (assoc :create-layer-group layer-group-specs))]
    (if ws-exists?
      rest-specs
      (do
        (when (contains? rest-specs :create-coverage-store-image-mosaic)
          (call-sql "create_new_schema" geoserver-workspace))
        (assoc rest-specs :create-workspace [(rest/create-workspace geoserver-workspace)])))))

(defn get-store-type
  "Returns a string describing the class of data or coverage store
  implied by the structure of the passed-in file-path."
  [file-path]
  (condp re-matches file-path
    #"^.*\.tiff?$"               :geotiff
    #"^.*\.shp$"                 :shapefile
    #"^.*datastore\.properties$" :imagemosaic
    nil))

(defn clean-file-path
  [file-path translate-bad-chars?]
  (let [pruned-file-path (if (s/ends-with? file-path "/datastore.properties")
                           (s/replace file-path "/datastore.properties" "")
                           (subs file-path 0 (s/last-index-of file-path \.)))]
    (if translate-bad-chars?
      (-> pruned-file-path
          (s/replace #"[^0-9a-zA-Z/\-_]" "-")
          (s/replace #"-+" "-"))
      pruned-file-path)))

;; FIXME: This will behave incorrectly if datastore.properties is in the toplevel data-dir directory.
(defn file-path->store-name
  [file-path]
  (-> file-path
      (clean-file-path true)
      (s/replace "/" "_")))

;; FIXME: This will behave incorrectly if datastore.properties is in the toplevel data-dir directory.
(defn file-path->layer-name
  [file-path]
  (let [cleaned-file-path (clean-file-path file-path false)]
    (if (s/includes? cleaned-file-path "/")
      (second (re-find #"^.*/([^/]+)$" cleaned-file-path))
      cleaned-file-path)))

(defn file-path->file-url
  [file-path data-dir]
  (let [cleaned-file-path (if (s/ends-with? file-path "/datastore.properties")
                            (s/replace file-path "/datastore.properties" "")
                            file-path)]
    (str "file://" data-dir (when-not (s/ends-with? data-dir "/") "/") cleaned-file-path)))

(defn get-style
  [file-path store-type styles]
  (let [store-name (file-path->store-name file-path)]
    (first
     (keep (fn [{:keys [layer-pattern raster-style vector-style]}]
             (when (s/includes? store-name layer-pattern)
               (case store-type
                 :geotiff     raster-style
                 :shapefile   vector-style
                 :imagemosaic raster-style
                 nil)))
           styles))))

(defn has-spatial-index?
  [file-path data-dir]
  (let [file-path-sans-extension (subs file-path 0 (s/last-index-of file-path \.))
        spatial-index-file-path  (str file-path-sans-extension ".qix")]
    (.exists (io/file data-dir spatial-index-file-path))))

(defn file-paths->file-specs
  [data-dir styles file-paths]
  (mapv #(let [store-type (get-store-type %)]
           (array-map :store-type store-type
                      :store-name (file-path->store-name %)
                      :layer-name (file-path->layer-name %)
                      :file-url   (file-path->file-url % data-dir)
                      :style      (get-style % store-type styles)
                      :indexed?   (has-spatial-index? % data-dir)))
        file-paths))

(defn gis-file-seq
  [^File node]
  (lazy-seq
   (if (.isDirectory node)
     (let [children (seq (.listFiles node))]
       (if-let [imagemosaic-properties (->> children
                                            (filter #(= (.getName ^File %)
                                                        "datastore.properties"))
                                            (first))]
         [imagemosaic-properties]
         (mapcat gis-file-seq children)))
     (when (get-store-type (.getName node))
       [node]))))

(defn to-dir
  [dir]
  (if (s/ends-with? dir "/")
    dir
    (str dir "/")))

(defn load-style-file-paths
  [style-dir]
  (some->> (io/file style-dir)
           (file-seq)
           (filter #(s/ends-with? (.getName %) ".css"))
           (map #(.getPath ^File %))))

(defn load-gis-file-paths
  [data-dir]
  (let [data-dir (to-dir data-dir)]
    (->> (io/file data-dir)
         (gis-file-seq)
         (map #(-> (.getPath ^File %)
                   (s/replace-first data-dir "")))
         (sort))))

(defn make-parallel-rest-requests
  [config-params rest-specs]
  (->> rest-specs
       (mapv #(make-rest-request-async config-params %))
       (mapv (comp :status deref))))

(defn make-parallel-wms-requests
  [config-params wms-specs]
  (->> wms-specs
       (mapv #(create-feature-type-spatial-index-async config-params %))
       (mapv (comp :status deref))))

(defn update-gwc-time-param-filters
  [{:keys [geoserver-workspace] :as config-params} {:keys [store-name]}]
  (let [{:keys [gridSubsets]} (get-existing-gwc-layer config-params store-name)]
    (rest/update-cached-layer geoserver-workspace
                              store-name
                              #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$"
                              gridSubsets)))

(defn make-parallel-gwc-requests
  [config-params gwc-specs]
  (->> gwc-specs
       (mapv #(update-gwc-time-param-filters config-params %))
       (mapv #(make-rest-request-async config-params %))
       (mapv (comp :status deref))))

(defn add-directory-to-workspace-aux!
  [{:keys [data-dir style-dir styles geoserver-workspace] :as config-params}]
  (tufte/profile
   {:id :add-directory-to-workspace!}
   (let [style-file-paths    (tufte/p :style-file-paths
                                      (load-style-file-paths style-dir))
         gis-file-specs      (tufte/p :gis-file-specs
                                      (->> (load-gis-file-paths data-dir)
                                           (file-paths->file-specs data-dir styles)))
         rest-specs          (tufte/p :rest-specs
                                      (file-specs->rest-specs config-params gis-file-specs style-file-paths))
         wms-specs           (tufte/p :wms-specs
                                      (file-specs->wms-specs gis-file-specs))
         gwc-specs           (tufte/p :gwc-specs
                                      (file-specs->gwc-specs gis-file-specs))
         rest-response-codes (tufte/p :rest-requests
                                      (client/with-async-connection-pool {:insecure? true}
                                        (into []
                                              (mapcat (fn [spec-type]
                                                        (->> (get rest-specs spec-type)
                                                             (make-parallel-rest-requests config-params))))
                                              [:create-workspace
                                               :create-style
                                               :create-coverage-store-image-mosaic
                                               :update-style
                                               :update-coverage-store
                                               :update-coverage-store-image-mosaic
                                               :create-coverage
                                               :create-coverage-via-put
                                               :create-data-store
                                               :create-feature-type-via-put
                                               :create-feature-type-alias
                                               :delete-layer
                                               :delete-feature-type
                                               :update-layer-style
                                               :create-layer-group])))
         wms-response-codes  (tufte/p :wms-requests
                                      (client/with-async-connection-pool {:insecure? true}
                                        (make-parallel-wms-requests config-params wms-specs)))
         gwc-response-codes  (tufte/p :gwc-requests
                                      (client/with-async-connection-pool {:insecure? true}
                                        (make-parallel-gwc-requests config-params gwc-specs)))
         http-response-codes (into [] (concat rest-response-codes wms-response-codes gwc-response-codes))
         num-success-codes   (count (filter success-code? http-response-codes))
         num-failure-codes   (- (count http-response-codes) num-success-codes)]
     (call-sql "clear_connection" geoserver-workspace)
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
  (let [workspaces (->> (get-existing-workspaces config-params)
                        (map :name)
                        (filter (fn [w] (re-matches (re-pattern geoserver-workspace) w))))]
    (log (str (count workspaces) " workspaces are queued to be removed."))
    (reduce (fn [acc cur]
              (call-sql "drop_existing_schema" cur)
              (->> (rest/delete-workspace cur true)
                   (make-rest-request config-params)
                   (:status)
                   (success-code?)
                   (and acc)))
            true
            workspaces)))
