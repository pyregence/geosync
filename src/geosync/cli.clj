(ns geosync.cli
  (:gen-class)
  (:import java.util.Base64)
  (:require [clojure.edn        :as edn]
            [clojure.java.io    :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string     :as s]
            [clojure.tools.cli  :refer [parse-opts]]
            [geosync.core       :refer [add-directory-to-workspace!
                                        remove-workspace!]]
            [geosync.server     :refer [start-server!]]
            [geosync.utils      :refer [hostname?
                                        nil-on-error
                                        non-empty-string?
                                        port?
                                        readable-directory?
                                        throw-message
                                        url?
                                        writable-directory?]]))

;;===========================================================
;; Argument Validation
;;===========================================================

;; Valid Config Keys

(spec/def ::geoserver-rest-uri  url?)
(spec/def ::geoserver-username  non-empty-string?)
(spec/def ::geoserver-password  non-empty-string?)
(spec/def ::geoserver-workspace non-empty-string?)
(spec/def ::geosync-server-host hostname?)
(spec/def ::geosync-server-port port?)
(spec/def ::data-dir            readable-directory?)
(spec/def ::style-dir           readable-directory?)
(spec/def ::overwrite-styles    boolean?)
(spec/def ::autostyle-layers    boolean?)
(spec/def ::layer-pattern       non-empty-string?)
(spec/def ::name                non-empty-string?)
(spec/def ::layer-group         (spec/keys :req-un [::layer-pattern ::name]))
(spec/def ::layer-groups        (spec/coll-of ::layer-group :kind vector? :distinct true))
(spec/def ::style-vector        (spec/coll-of non-empty-string? :kind vector? :distinct true))
(spec/def ::raster-style        (spec/or :style non-empty-string? :styles ::style-vector))
(spec/def ::vector-style        (spec/or :style non-empty-string? :styles ::style-vector))
(spec/def ::style               (spec/keys :req-un [::layer-pattern (or ::raster-style ::vector-style)]))
(spec/def ::styles              (spec/coll-of ::style :kind vector? :distinct true))
(spec/def ::action-run-time     #{:before :after})
(spec/def ::action              #{"add" "remove"})
(spec/def ::action-hook-params  (spec/map-of keyword? any?))
(spec/def ::action-hook         (spec/tuple ::action-run-time ::action url? ::action-hook-params))
(spec/def ::action-hooks        (spec/coll-of ::action-hook :kind vector? :distinct true))
(spec/def ::dir                 readable-directory?)
(spec/def ::folder-name->regex  (spec/map-of string? string?))
(spec/def ::file-watcher        (spec/keys :req-un [::dir
                                                    ::folder-name->regex]))
(spec/def ::workspace-regex     re-pattern)
(spec/def ::associated-rule     (spec/keys :req-un [::layer-rule ::role]))
(spec/def ::associated-rules    (spec/coll-of ::associated-rule :kind vector? :distinct true))
(spec/def ::one-layer-rule      (spec/keys :req-un [::workspace-regex ::associated-rules]))
(spec/def ::layer-rules         (spec/coll-of ::one-layer-rule :kind vector? :distinct true))
(spec/def ::geosync-config-file (spec/keys :opt-un [::geoserver-rest-uri
                                                    ::geoserver-username
                                                    ::geoserver-password
                                                    ::geoserver-workspace
                                                    ::geosync-server-host
                                                    ::geosync-server-port
                                                    ::action
                                                    ::data-dir
                                                    ::style-dir
                                                    ::overwrite-styles
                                                    ::autostyle-layers
                                                    ::layer-groups
                                                    ::styles
                                                    ::action-hooks
                                                    ::file-watcher
                                                    ::layer-rules]))

;; Key Combination Rules

(spec/def ::geoserver-auth      (spec/keys :req-un [::geoserver-rest-uri
                                                    ::geoserver-username
                                                    ::geoserver-password]))
(spec/def ::server-mode         (fn [{:keys [geosync-server-host geosync-server-port data-dir style-dir]}]
                                  (and geosync-server-host
                                       geosync-server-port
                                       (nil? data-dir)
                                       (nil? style-dir))))
(spec/def ::cli-register-mode   (fn [{:keys [geoserver-workspace action data-dir style-dir overwrite-styles]}]
                                  (and geoserver-workspace
                                       (= action "add")
                                       (or data-dir style-dir)
                                       ;; overwrite-styles and no style-dir is the only invalid combination
                                       (not (and overwrite-styles (nil? style-dir))))))
(spec/def ::cli-deregister-mode (fn [{:keys [geoserver-workspace action data-dir style-dir overwrite-styles]}]
                                  (and geoserver-workspace
                                       (= action "remove")
                                       (nil? data-dir)
                                       (nil? style-dir)
                                       (nil? overwrite-styles))))
(spec/def ::operation-mode      (spec/or :server-mode         ::server-mode
                                         :cli-register-mode   ::cli-register-mode
                                         :cli-deregister-mode ::cli-deregister-mode))

(spec/def ::geosync-config      (spec/and ::geosync-config-file
                                          ::geoserver-auth
                                          ::operation-mode))

;;===========================================================
;; Argument Processing
;;===========================================================

(defn encode-str
  [s]
  (.encodeToString (Base64/getEncoder) (.getBytes ^String s)))

(defn all-required-keys? [config-params]
  (and (every? config-params [:geoserver-rest-uri :geoserver-username :geoserver-password])
       (or (every? config-params [:geoserver-workspace :action])
           (every? config-params [:geosync-server-host :geosync-server-port]))))

(defn read-config-params
  [config-file-path]
  (if (not config-file-path)
    {}
    (let [config-params (nil-on-error (edn/read-string (slurp config-file-path)))]
      (cond (nil? config-params)
            (throw-message "The provided --config-file does not contain valid EDN.\n")

            (not (map? config-params))
            (throw-message "The provided --config-file does not contain an EDN map.\n")

            (not (spec/valid? ::geosync-config-file config-params))
            (throw-message (str "The provided --config-file contains an invalid EDN config map:\n"
                                (spec/explain-str ::geosync-config-file config-params)))

            :else
            config-params))))

(defn add-derived-params
  [{:keys [geoserver-rest-uri geoserver-username geoserver-password] :as config-params}]
  (let [geoserver-rest-uri (if (s/ends-with? geoserver-rest-uri "/")
                             (subs geoserver-rest-uri 0 (dec (count geoserver-rest-uri)))
                             geoserver-rest-uri)]
    (assoc config-params
           :geoserver-rest-uri     geoserver-rest-uri
           :geoserver-rest-headers {"Content-Type"  "text/xml"
                                    "Accept"        "application/json"
                                    "Authorization" (str "Basic "
                                                         (encode-str (str geoserver-username
                                                                          ":"
                                                                          geoserver-password)))}
           :geoserver-wms-uri      (-> geoserver-rest-uri
                                       (s/replace "/rest" "/wms")
                                       (str "?SERVICE=WMS"
                                            "&VERSION=1.3.0"
                                            "&REQUEST=GetFeatureInfo"
                                            "&INFO_FORMAT=application/json"
                                            "&FEATURE_COUNT=1"
                                            "&TILED=true"
                                            "&I=0"
                                            "&J=0"
                                            "&WIDTH=1"
                                            "&HEIGHT=1"
                                            "&CRS=EPSG:4326"
                                            "&BBOX=-180.0,-90.0,180.0,90.0")))))

(defn add-file-watcher-params
  [{:keys [file-watcher] :as config-params}]
  (if file-watcher
    (update-in config-params
               [:file-watcher :folder-name->regex]
               #(reduce-kv (fn [acc k v] (assoc acc k (re-pattern v))) {} %))
    config-params))

(defn process-options
  [options]
  (let [config-file-params  (read-config-params (:config-file options))
        command-line-params (dissoc options :config-file)
        config-params       (merge config-file-params command-line-params)]
    (cond (not (all-required-keys? config-params))
          (throw-message (str "These parameters are always required (but may be included in --config-file):\n"
                              "  --geoserver-rest-uri --geoserver-username --geoserver-password\n"
                              "For command-line mode, please include:\n"
                              "  --geoserver-workspace --action\n"
                              "For server mode, please include:\n"
                              "  --geosync-server-host --geosync-server-port\n"))

          (not (spec/valid? ::geosync-config config-params))
          (throw-message (str "Some input parameters are invalid:\n"
                              (spec/explain-str ::geosync-config config-params)))

          :else
          (-> config-params
              add-derived-params
              add-file-watcher-params))))

;;===========================================================
;; User Interface
;;===========================================================

(def cli-options
  [["-c" "--config-file EDN" "Path to an EDN file containing a map of these parameters"
    :validate [#(.exists  (io/file %)) "The provided --config-file does not exist."
               #(.canRead (io/file %)) "The provided --config-file is not readable."]]

   ["-g" "--geoserver-rest-uri URI" "URI of your GeoServer's REST extensions"
    :validate [url? "The provided --geoserver-rest-uri is not a valid URI."]]

   ["-u" "--geoserver-username USER" "GeoServer admin username"]

   ["-p" "--geoserver-password PASS" "GeoServer admin password"]

   ["-w" "--geoserver-workspace WS" "Workspace name to receive the new GeoServer layers"]

   ["-d" "--data-dir DIR" "Path to the directory containing your GIS files"
    :validate [#(.exists  (io/file %)) "The provided --data-dir does not exist."
               #(.canRead (io/file %)) "The provided --data-dir is not readable."]]

   ["-s" "--style-dir DIR" "Path to the directory containing your style files"
    :validate [#(.exists  (io/file %)) "The provided --style-dir does not exist."
               #(.canRead (io/file %)) "The provided --style-dir is not readable."]]

   ["-a" "--action ACTION" "GeoServer action: either \"add\" or \"remove\". Required in CLI mode."]

   ["-O" "--overwrite-styles" "If true, already existing styles will have their definitions overwritten"]

   ["-A", "--autostyle-layers" "If true, Geosync will match layers with existing styles based on the style and layer names"]

   ["-h" "--geosync-server-host HOST" "Hostname to advertise in server responses"
    :validate [hostname? "The provided --geosync-server-host is invalid."]]

   ["-P" "--geosync-server-port PORT" "Server port to listen on for incoming requests"
    :parse-fn #(Integer/parseInt %)
    :validate [port? "The provided --geosync-server-port must be an integer between 0 and 65536."]]

   ["-o" "--log-dir PATH" "Path to log files"
    :validate [writable-directory? "Directory does not exist or is not writable."]]])

(def program-banner
  (str "geosync: Load directory trees of GIS and style files into a running GeoServer instance.\n"
       "Copyright © 2020-2023 Spatial Informatics Group, LLC.\n"))

(defn -main
  [& args]
  (println program-banner)
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)
        ;; {:options   The options map, keyed by :id, mapped to the parsed value
        ;;  :arguments A vector of unprocessed arguments
        ;;  :summary   A string containing a minimal options summary
        ;;  :errors    A vector of error message strings thrown during parsing; nil when no errors exist
        config-params (try
                        (process-options options)
                        (catch Exception e
                          (ex-message e)))]
    (cond (seq errors)
          (do
            (run! println errors)
            (println (str "\nUsage:\n" summary)))

          (or (empty? options) (seq arguments))
          (println (str "Usage:\n" summary))

          (string? config-params)
          (do
            (println config-params)
            (println (str "Usage:\n" summary)))

          ;; GeoSync is running in server mode
          (and (:geosync-server-host config-params)
               (:geosync-server-port config-params))
          (start-server! config-params)

          ;; GeoSync is running in CLI mode with an action of "add"
          (= (:action config-params) "add")
          (do
            (add-directory-to-workspace! config-params)
            (shutdown-agents)
            (flush))

          ;; GeoSync is running in CLI mode with an action of "remove"
          (= (:action config-params) "remove")
          (do
            (remove-workspace! config-params)
            (shutdown-agents)
            (flush))

          :else
          (do
            (println "Something went wrong when running GeoSync.")
            (shutdown-agents)
            (flush)))))
