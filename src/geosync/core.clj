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
  [{:keys [geoserver-rest-uri geoserver-rest-headers]} [http-method uri-suffix http-body content-type accept]]
  (try
    (let [headers  (cond-> geoserver-rest-headers
                     content-type (assoc "Content-Type" content-type)
                     accept       (assoc "Accept" accept))
          response (client/request {:url                (url-path geoserver-rest-uri uri-suffix)
                                    :method             http-method
                                    :headers            headers
                                    :body               http-body
                                    :insecure?          true
                                    :socket-timeout     timeout-ms
                                    :connection-timeout timeout-ms})]
      (log (format "%6s %s%n               -> %s"
                   http-method
                   uri-suffix
                   (select-keys response [:status :reason-phrase]))
           {:truncate? false})
      response)
    (catch Exception e
      (log (format "%6s %s%n               -> %s"
                   http-method
                   uri-suffix
                   (if (timeout? e)
                     (str "Timeout Error: Your request took longer than " (quot timeout-ms 1000) " seconds.")
                     (select-keys (ex-data e) [:status :reason-phrase :body])))
           {:truncate? false})
      (ex-data e))))

;; FIXME: Use an SSL keystore and remove insecure? param
(defn make-rest-request-async
  [{:keys [geoserver-rest-uri geoserver-rest-headers]} [http-method uri-suffix http-body content-type accept]]
  (let [result  (promise)
        headers (cond-> geoserver-rest-headers
                  content-type (assoc "Content-Type" content-type)
                  accept       (assoc "Accept" accept))]
    (client/request {:url                (url-path geoserver-rest-uri uri-suffix)
                     :method             http-method
                     :headers            headers
                     :body               http-body
                     :insecure?          true
                     :async?             true
                     :socket-timeout     timeout-ms
                     :connection-timeout timeout-ms}
                    (fn [response]
                      (log (format "%6s %s%n               -> %s"
                                   http-method
                                   uri-suffix
                                   (select-keys response [:status :reason-phrase]))
                           {:truncate? false})
                      (deliver result response))
                    (fn [error]
                      (log (format "%6s %s%n               -> %s"
                                   http-method
                                   uri-suffix
                                   (if (timeout? error)
                                     (str "Timeout Error: Your request took longer than " (quot timeout-ms 1000) " seconds.")
                                     (select-keys (ex-data error) [:status :reason-phrase :body])))
                           {:truncate? false})
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

(defn get-matching-style
  [layer-name style existing-styles autostyle-layers]
  (let [layer-name (s/lower-case layer-name)]
    (cond
      style            style
      autostyle-layers (first (filter #(s/ends-with? layer-name (s/lower-case (last (s/split % #":")))) existing-styles))
      :else            nil)))

(defn file-spec->layer-specs
  "Returns a sequence of one or more REST request specifications as
  tuples of [http-method uri-suffix http-body content-type] depending
  on the structure of the passed-in file-spec or nil if the store-type
  is unsupported."
  [{:keys [geoserver-workspace autostyle-layers]}
   existing-stores
   existing-styles
   {:keys [store-type store-name layer-name file-url style]}]
  (let [matching-style (get-matching-style layer-name style existing-styles autostyle-layers)]
    (when-not (contains? existing-stores store-name)
      (case store-type
        :geotiff     [(rest/create-coverage-via-put geoserver-workspace store-name file-url)
                      (when matching-style
                        (rest/update-layer-style geoserver-workspace store-name matching-style :raster))]

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
                       (when matching-style
                         [(rest/update-layer-style geoserver-workspace store-name matching-style :vector)])))

        :imagemosaic (do (update-properties-file! (str file-url "/datastore.properties") "schema" geoserver-workspace)
                         (update-properties-file! (str file-url "/indexer.properties") "Name" store-name)
                         (clean-image-mosaic-folder (s/replace file-url "file://" ""))
                         [(rest/create-coverage-store-image-mosaic geoserver-workspace store-name file-url)
                          (rest/update-coverage-store-image-mosaic geoserver-workspace store-name file-url)
                          (rest/create-coverage-image-mosaic geoserver-workspace store-name)
                          (when matching-style
                            (rest/update-layer-style geoserver-workspace store-name matching-style :raster))])

        (throw (ex-info "Unsupported store type detected."
                        {:store-type store-type :file-url file-url}))))))

(defn file-specs->layer-specs
  [config-params existing-stores existing-styles file-specs]
  (into []
        (comp (mapcat (partial file-spec->layer-specs
                               config-params
                               existing-stores
                               existing-styles))
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
                  "/styles"              :create-style
                  "/security/acl/layers" :add-layer-rules
                  "/geofence/rules"      :add-geofence-rule
                  "/geofence/adminrules" :add-geofence-admin-rule)
       "PUT"    (condp #(s/includes? %2 %1) uri-suffix
                  "external.imagemosaic" :create-coverage-store-image-mosaic
                  "external.geotiff"     :create-coverage-via-put
                  "external.shp"         :create-feature-type-via-put
                  "/coveragestores/"     :update-coverage-store
                  "/gwc/rest/layers/"    :update-cached-layer
                  "/layers/"             :update-layer-style
                  "/styles"              :update-style)
       "DELETE" (condp #(s/includes? %2 %1) uri-suffix
                  "/layers/"              :delete-layer
                  "/featuretypes/"        :delete-feature-type
                  "/security/acl/layers/" :delete-layer-rule
                  "/geofence/rules/"      :delete-geofence-rule
                  "/geofence/adminrules/" :delete-geofence-admin-rule)))))

(defn get-style-name
  "Returns the style name, given a target workspace and a file path.
   We are prepending the workspace name to the final style name because
   GeoServer does not support styles with the same name across workspaces."
  [workspace-name file-path]
  (let [style-name (-> file-path
                       (io/file)
                       (.getName)
                       (s/split #"\.")
                       (first))]
    (cond
      (s/blank? workspace-name) style-name
      :else                     (str workspace-name ":" style-name))))

(defn file-path->style-spec
  [{:keys [geoserver-workspace overwrite-styles]} file-path existing-styles]
  (let [style-name (get-style-name geoserver-workspace file-path)
        exists?    (contains? existing-styles style-name)]
    (cond
      (not exists?)                  (rest/create-style geoserver-workspace style-name file-path)
      (and exists? overwrite-styles) (rest/update-style geoserver-workspace style-name file-path)
      :else                          nil)))

(defn file-paths->style-specs
  [config-params existing-styles style-file-paths]
  (keep #(file-path->style-spec config-params % existing-styles) style-file-paths))

;;; Data Security Layer Rules

(defn get-existing-layer-rules
  "Gets existing layer rules and returns them in a set of maps of the format:
   #{{:layer-rule \"*.*.r\", :role \"ROLE_ANONYMOUS\"
     {:layer-rule \"*.*.w\", :role \"GROUP_ADMIN,ADMIN\"}}"
  [config-params]
  (as-> (rest/get-layer-rules) %
    (make-rest-request config-params %)
    (:body %)
    (json/read-str %)
    (for [[layer-rule role] %]
      {:layer-rule layer-rule :role role})
    (set %)))

(defn get-matching-layer-rules
  "Matches the given `geoserver-workspace` to the `:workspace-regex` value provided
   in the `:layer-rules` section of the config file and returns the rules associated
   with that entry in the map. Note that each `:layer-rule` should have the string
   \"geoserver-workspace\" in it in order to be replaced with the actual workspace.
   Also note that each `:workspace-regex` should be unique from every other `:workspace-regex`
   in the `:layer-rules` entry; thus it's safe to call `first` on the matching regex."
  [geoserver-workspace layer-rules]
  (some->> layer-rules
    (filter #(re-matches (re-pattern (:workspace-regex %)) geoserver-workspace))
    (first)
    (:associated-rules)
    (map (fn [rule]
           (update rule :layer-rule #(s/replace % #"geoserver-workspace" geoserver-workspace))))))

(defn layer-rules->layer-rules-specs
  "Determines any new layer rules that need to be added. Doesn't add any
   layer rules that already exist on the GeoServer.
   Example `final-layer-rules` format:
   [{:layer-rule \"fire-risk-forecast_nve_20231213_00.*.r\" :role \"NVE\"}
    {:layer-rule \"fire-risk-forecast_nve_20231213_00.*.w\" :role \"NVE\"}]"
  [{:keys [geoserver-workspace layer-rules]} existing-layer-rules]
  (let [matching-layer-rules (get-matching-layer-rules geoserver-workspace layer-rules)
        final-layer-rules    (remove existing-layer-rules matching-layer-rules)]
    (when (seq final-layer-rules)
      [(rest/add-layer-rules final-layer-rules)])))

;;; GeoFence Rules

(defn get-existing-geofence-rules
  "Gets existing GeoFence data and admin rules."
  [config-params]
  {:existing-data-rules  (as-> (rest/get-geofence-rules) %
                           (make-rest-request config-params %)
                           (:body %)
                           (json/read-str % :key-fn keyword)
                           (:rules %))
   :existing-admin-rules (as-> (rest/get-geofence-admin-rules) %
                           (make-rest-request config-params %)
                           (:body %)
                           (json/read-str % :key-fn keyword)
                           (:rules %))})

(defn get-matching-geofence-rules
  "Returns a map with `:matching-data-rules` and/or `:matching-admin-rules` for the matching workspace
   regex, injecting the workspace name into each rule. Returns nil if no matches are found.
   Note that each `:workspace-regex` should be unique from every other `:workspace-regex`
   in the `:layer-rules` entry; thus it's safe to call `first` on the matching regex."
  [workspace geofence-rules]
  (when-let [matching-geofence-rules (->> geofence-rules
                                          (filter #(re-matches (re-pattern (:workspace-regex %)) workspace))
                                          (first))]
    (let [{:keys [data-rules admin-rules]} matching-geofence-rules]
      (cond-> {}
        data-rules  (assoc :matching-data-rules  (mapv #(assoc % :workspace workspace) data-rules))
        admin-rules (assoc :matching-admin-rules (mapv #(assoc % :workspace workspace) admin-rules))))))

(defn geofence-rules->geofence-rules-specs
  "Determines any new GeoFence data and admin rules that need to be added.
   We delete all existing rules that match on workspace and add all new ones that match."
  [{:keys [geoserver-workspace geofence-rules]} {:keys [existing-data-rules existing-admin-rules]}]
  (let [{:keys [matching-data-rules matching-admin-rules]} (get-matching-geofence-rules geoserver-workspace geofence-rules)
        ;; Find all existing rules for this workspace to delete
        data-rules-to-delete  (filter #(= (:workspace %) geoserver-workspace) existing-data-rules)
        admin-rules-to-delete (filter #(= (:workspace %) geoserver-workspace) existing-admin-rules)]

    (concat
     ;; Delete all existing rules for this workspace
     (mapv rest/delete-geofence-rule (map :id data-rules-to-delete))
     (mapv rest/delete-geofence-admin-rule (map :id admin-rules-to-delete))

     ;; Add all rules from config
     (mapv rest/add-geofence-rule matching-data-rules)
     (mapv rest/add-geofence-admin-rule matching-admin-rules))))

;;; Core

(defn file-specs->rest-specs
  "Generates a sequence of REST request specifications as tuples of
  [http-method uri-suffix http-body content-type]. Each file-spec may
  contribute one or more of these to the final sequence. Returns a map
  of these REST specs grouped by spec type."
  [{:keys [geoserver-workspace] :as config-params} gis-file-specs style-file-paths]
  (let [ws-exists?              (workspace-exists? config-params)
        layer-rules?            (some? (:layer-rules config-params))
        geofence-rules?         (some? (:geofence-rules config-params))
        existing-stores         (if ws-exists? (get-existing-stores config-params) #{})
        existing-layer-groups   (if ws-exists? (get-existing-layer-groups config-params) #{})
        existing-styles         (if ws-exists? (get-existing-styles config-params) #{})
        existing-layer-rules    (if layer-rules? (get-existing-layer-rules config-params) #{})
        layer-rule-specs        (if layer-rules? (layer-rules->layer-rules-specs config-params existing-layer-rules) nil)
        existing-geofence-rules (if geofence-rules? (get-existing-geofence-rules config-params) #{})
        geofence-rule-specs     (if geofence-rules? (geofence-rules->geofence-rules-specs config-params existing-geofence-rules) nil)
        style-specs             (file-paths->style-specs config-params existing-styles style-file-paths)
        all-styles              (concat existing-styles (map #(get-style-name geoserver-workspace %) style-file-paths))
        layer-specs             (file-specs->layer-specs config-params existing-stores all-styles gis-file-specs)
        layer-group-specs       (file-specs->layer-group-specs config-params existing-stores existing-layer-groups gis-file-specs)
        rest-specs              (-> (group-by get-spec-type (concat layer-specs style-specs layer-rule-specs geofence-rule-specs))
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
           (filter #(s/ends-with? (.getName ^File %) ".css"))
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

;;; Add workspace

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
                                               :create-layer-group
                                               :add-layer-rules
                                               :delete-layer-rule
                                               :delete-geofence-rule
                                               :delete-geofence-admin-rule
                                               :add-geofence-rule
                                               :add-geofence-admin-rule])))
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

;;; Remove workspace

(defn remove-workspace!
  [{:keys [geoserver-workspace] :as config-params}]
  (let [workspaces            (->> (get-existing-workspaces config-params)
                                   (map :name)
                                   (filter (fn [w] (re-matches (re-pattern geoserver-workspace) w))))
        layer-rules?          (some? (:layer-rules config-params))
        geofence-rules?       (some? (:geofence-rules config-params))]
    (log (str (count workspaces) " workspaces are queued to be removed."))
    (reduce (fn [acc current-workspace]
              (call-sql "drop_existing_schema" current-workspace)
              (let [delete-workspace-success?      (->> (rest/delete-workspace current-workspace true)
                                                        (make-rest-request config-params)
                                                        (:status)
                                                        (success-code?))
                    existing-layer-rules           (when layer-rules?
                                                     (get-existing-layer-rules config-params))
                    layer-rules-to-delete          (->> existing-layer-rules
                                                        (map :layer-rule)
                                                        (filter #(let [[rule-workspace _ _] (s/split % #"\.")]
                                                                   (= rule-workspace current-workspace))))
                    delete-layer-rule-success?     (->> layer-rules-to-delete
                                                        (map #(->> (rest/delete-layer-rule %)
                                                                   (make-rest-request config-params)
                                                                   (:status)))
                                                        (every? success-code?))
                    existing-geofence-rules        (when geofence-rules?
                                                     (get-existing-geofence-rules config-params))
                    geofence-data-rules-to-delete  (when geofence-rules?
                                                     (->> (:existing-data-rules existing-geofence-rules)
                                                          (filter #(= (:workspace %) current-workspace))
                                                          (map :id)))
                    geofence-admin-rules-to-delete (when geofence-rules?
                                                     (->> (:existing-admin-rules existing-geofence-rules)
                                                          (filter #(= (:workspace %) current-workspace))
                                                          (map :id)))
                    delete-geofence-data-success?  (->> geofence-data-rules-to-delete
                                                        (map #(->> (rest/delete-geofence-rule %)
                                                                   (make-rest-request config-params)
                                                                   (:status)))
                                                        (every? success-code?))
                    delete-geofence-admin-success? (->> geofence-admin-rules-to-delete
                                                        (map #(->> (rest/delete-geofence-admin-rule %)
                                                                   (make-rest-request config-params)
                                                                   (:status)))
                                                        (every? success-code?))]


                (when (and layer-rules? delete-layer-rule-success?)
                  (log (str (count layer-rules-to-delete) " layer rules were removed.")))
                (when (and geofence-rules? delete-geofence-data-success?)
                  (log (str (count geofence-data-rules-to-delete) " GeoFence data rules were deleted.")))
                (when (and geofence-rules? delete-geofence-admin-success?)
                  (log (str (count geofence-admin-rules-to-delete) " GeoFence admin rules were deleted.")))
                (and acc delete-workspace-success? delete-layer-rule-success?)))
            true
            workspaces)))
