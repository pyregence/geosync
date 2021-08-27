(ns geosync.cli
  (:import java.util.Base64)
  (:require [clojure.edn        :as edn]
            [clojure.java.io    :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string     :as s]
            [clojure.tools.cli  :refer [parse-opts]]
            [geosync.core       :refer [add-directory-to-workspace!]]
            [geosync.server     :refer [start-server!] :as server]
            [geosync.utils      :refer [nil-on-error
                                        throw-message
                                        non-empty-string?
                                        url?
                                        readable-directory?
                                        hostname?
                                        port?]]))

;;===========================================================
;; Argument Validation
;;===========================================================

(spec/def ::geoserver-rest-uri  url?)
(spec/def ::geoserver-username  non-empty-string?)
(spec/def ::geoserver-password  non-empty-string?)
(spec/def ::geoserver-workspace non-empty-string?)
(spec/def ::data-dir            readable-directory?)
(spec/def ::geosync-server-host hostname?)
(spec/def ::geosync-server-port port?)
(spec/def ::layer-pattern       non-empty-string?)
(spec/def ::raster-style        non-empty-string?)
(spec/def ::vector-style        non-empty-string?)
(spec/def ::style               (spec/keys :req-un [::layer-pattern (or ::raster-style ::vector-style)]))
(spec/def ::styles              (spec/coll-of ::style :kind vector? :distinct true))
(spec/def ::name                non-empty-string?)
(spec/def ::layer-group         (spec/keys :req-un [::layer-pattern ::name]))
(spec/def ::layer-groups        (spec/coll-of ::layer-group :kind vector? :distinct true))
(spec/def ::action-run-time     #{:before :after})
(spec/def ::action-hook-params  (spec/map-of keyword? any?))
(spec/def ::action-hook         (spec/tuple ::action-run-time ::server/action url? ::action-hook-params))
(spec/def ::action-hooks        (spec/coll-of ::action-hook :kind vector? :distinct true))
(spec/def ::geosync-config      (spec/keys :req-un [::geoserver-rest-uri
                                                    ::geoserver-username
                                                    ::geoserver-password
                                                    (or
                                                     (and ::geoserver-workspace
                                                          ::data-dir)
                                                     (and ::geosync-server-host
                                                          ::geosync-server-port))]
                                           :opt-un [::styles
                                                    ::layer-groups
                                                    ::action-hooks]))
(spec/def ::geosync-config-file (spec/keys :opt-un [::geoserver-rest-uri
                                                    ::geoserver-username
                                                    ::geoserver-password
                                                    ::geoserver-workspace
                                                    ::data-dir
                                                    ::geosync-server-host
                                                    ::geosync-server-port
                                                    ::styles
                                                    ::layer-groups
                                                    ::action-hooks]))

;;===========================================================
;; Argument Processing
;;===========================================================

(defn encode-str
  [s]
  (.encodeToString (Base64/getUrlEncoder) (.getBytes ^String s)))

(defn all-required-keys? [config-params]
  (and (every? config-params [:geoserver-rest-uri :geoserver-username :geoserver-password])
       (or (every? config-params [:geoserver-workspace :data-dir])
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

(defn process-options
  [options]
  (let [config-file-params  (read-config-params (:config-file options))
        command-line-params (dissoc options :config-file)
        config-params       (merge config-file-params command-line-params)]
    (cond (not (all-required-keys? config-params))
          (throw-message (str "These parameters are always required (but may be included in --config-file):\n"
                              "  --geoserver-rest-uri --geoserver-username --geoserver-password\n"
                              "For command-line mode, please include:\n"
                              "  --geoserver-workspace --data-dir\n"
                              "For server mode, please include:\n"
                              "  --geosync-server-host --geosync-server-port\n"))

          (not (spec/valid? ::geosync-config config-params))
          (throw-message (str "Some input parameters are invalid:\n"
                              (spec/explain-str ::geosync-config config-params)))

          :else
          (add-derived-params config-params))))

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
   ["-h" "--geosync-server-host HOST" "Hostname to advertise in server responses"
    :validate [hostname? "The provided --geosync-server-host is invalid."]]
   ["-P" "--geosync-server-port PORT" "Server port to listen on for incoming requests"
    :parse-fn #(Integer/parseInt %)
    :validate [port? "The provided --geosync-server-port must be an integer between 0 and 65536."]]])

(def program-banner
  (str "geosync: Load a nested directory tree of GeoTIFFs and Shapefiles into a running GeoServer instance.\n"
       "Copyright © 2020-2021 Spatial Informatics Group, LLC.\n"))

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

          (and (:geosync-server-host config-params)
               (:geosync-server-port config-params))
          (start-server! config-params)

          :else
          (do
            (add-directory-to-workspace! config-params)
            (shutdown-agents)
            (flush)))))
