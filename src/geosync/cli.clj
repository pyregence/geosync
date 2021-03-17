(ns geosync.cli
  (:import java.net.URL
           java.util.Base64)
  (:require [clojure.edn        :as edn]
            [clojure.java.io    :as io]
            [clojure.spec.alpha :as spec]
            [clojure.tools.cli  :refer [parse-opts]]
            [geosync.core       :refer [update-geoserver!]]))

(defn non-empty-string?
  [x]
  (and (string? x)
       (pos? (count x))))

(defn url?
  [x]
  (and (non-empty-string? x)
       (try
         (URL. x)
         (catch Exception _ false))))

(defn readable-directory?
  [x]
  (when-let [directory (try
                         (io/file x)
                         (catch Exception _ nil))]
    (and (.exists directory)
         (.canRead directory)
         (.isDirectory directory))))

(spec/def ::geoserver-rest-uri  url?)
(spec/def ::geoserver-username  non-empty-string?)
(spec/def ::geoserver-password  non-empty-string?)
(spec/def ::geoserver-workspace non-empty-string?)
(spec/def ::data-dir            readable-directory?)
(spec/def ::layer-pattern       non-empty-string?)
(spec/def ::raster-style        non-empty-string?)
(spec/def ::vector-style        non-empty-string?)
(spec/def ::style               (spec/keys :req-un [::layer-pattern (or ::raster-style ::vector-style)]))
(spec/def ::styles              (spec/coll-of ::style :kind vector? :distinct true))
(spec/def ::name                non-empty-string?)
(spec/def ::layer-group         (spec/keys :req-un [::layer-pattern ::name]))
(spec/def ::layer-groups        (spec/coll-of ::layer-group :kind vector? :distinct true))
(spec/def ::geosync-config      (spec/keys :req-un [::geoserver-rest-uri
                                                    ::geoserver-username
                                                    ::geoserver-password
                                                    ::geoserver-workspace
                                                    ::data-dir]
                                           :opt-un [::styles
                                                    ::layer-groups]))
(spec/def ::geosync-config-opt  (spec/keys :opt-un [::geoserver-rest-uri
                                                    ::geoserver-username
                                                    ::geoserver-password
                                                    ::geoserver-workspace
                                                    ::data-dir
                                                    ::styles
                                                    ::layer-groups]))

(defn throw-message
  [msg]
  (throw (ex-info msg {})))

(defn encode-str
  [s]
  (.encodeToString (Base64/getUrlEncoder) (.getBytes s)))

(defn read-config-params
  [config-file-path]
  (if (not config-file-path)
    {}
    (let [config-params (try
                          (edn/read-string (slurp config-file-path))
                          (catch Exception _ nil))]
      (cond (nil? config-params)
            (throw-message "The provided --config-file does not contain valid EDN.")

            (not (map? config-params))
            (throw-message "The provided --config-file does not contain an EDN map.")

            (not (spec/valid? ::geosync-config-opt config-params))
            (throw-message (str "The provided --config-file contains an invalid EDN config map:\n"
                                (spec/explain-str ::geosync-config-opt config-params)))

            :else
            config-params))))

(defn process-options
  [options]
  (let [config-file-params  (read-config-params (:config-file options))
        command-line-params (dissoc options :config-file)
        config-params       (merge config-file-params command-line-params)]
    (if (spec/valid? ::geosync-config config-params)
      (assoc config-params
             :geoserver-auth-code
             (str "Basic " (encode-str (format "%s:%s")
                                       (:geoserver-username config-params)
                                       (:geoserver-password config-params))))
      (throw-message (str "Some input parameters are invalid:\n"
                          (spec/explain-str ::geosync-config config-params))))))

(def cli-options
  [["-c" "--config-file EDN"         "Path to an EDN file containing a map of these parameters"
    :validate [#(.exists  (io/file %)) "The provided --config-file does not exist."
               #(.canRead (io/file %)) "The provided --config-file is not readable."]]
   ["-d" "--data-dir DIR"            "Path to the directory containing your GIS files"
    :validate [#(.exists  (io/file %)) "The provided --data-dir does not exist."
               #(.canRead (io/file %)) "The provided --data-dir is not readable."]]
   ["-g" "--geoserver-rest-uri URI"  "URI of your GeoServer's REST extensions"
    :validate [url? "The provided --geoserver-rest-uri is not a valid URI."]]
   ["-u" "--geoserver-username USER" "GeoServer admin username"]
   ["-p" "--geoserver-password PASS" "GeoServer admin password"]
   ["-w" "--geoserver-workspace WS"  "Workspace name to receive the new GeoServer layers"]])

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
        options-map (try
                      (process-options options)
                      (catch Exception e
                        (ex-message e)))]
    (cond (seq errors)
          (do
            (run! println errors)
            (println (str "\nUsage:\n" summary)))

          (or (empty? options) (seq arguments))
          (println (str "Usage:\n" summary))

          (string? options-map)
          (do
            (println options-map)
            (println (str "\nUsage:\n" summary)))

          :else
          (update-geoserver! options-map)))
  ;; Exit cleanly
  (shutdown-agents)
  (flush)
  (System/exit 0))
