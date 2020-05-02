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
;;; geosync is a simple command-line application that traverses a
;;; directory of raster and vector GIS files (e.g., GeoTIFFs,
;;; Shapefiles) and generates the necessary REST commands to add
;;; layers for each file to a running GeoServer instance.

(ns geosync.core
  (:require [clojure.edn       :as edn]
            [clojure.java.io   :as io]
            [clojure.string    :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-http.client   :as client]
            [geosync.rest-api  :as rest])
  (:import java.util.Base64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Files -> REST Requests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-rest-request [{:keys [geoserver-rest-uri geoserver-auth-code]} [http-method uri-suffix http-body]]
  (try
    (client/request {:url     (str geoserver-rest-uri uri-suffix)
                     :method  http-method
                     :headers {"Content-Type"  "text/xml"
                               "Accept"        "text/xml"
                               "Authorization" geoserver-auth-code}
                     :body    http-body})
    (catch Exception e (println "REST Exception:" http-method uri-suffix "->" (.getMessage e)))))

(defn get-store-type
  "Returns a string describing the class of data or coverage store
  implied by the structure of the passed-in file-path."
  [file-path]
  (condp re-matches file-path
    #"^.*\.tiff?$" "GeoTIFF"
    #"^.*\.shp$"   "Shapefile"
    nil))

;; FIXME: Link in geosync.rest-api
(defn file-path->rest-specs
  "Returns a vector of one or more REST request specifications as
  triplets of [http-method uri-suffix http-body] depending on the
  structure of the passed-in file-path."
  [config-params file-path]
  (let [store-type (get-store-type file-path)]
    nil
    #_(cond Layer
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
                                             Workspace " (" URI ")"))))))

(defn file-paths->rest-specs
  "Generates a sequence of REST request specifications as triplets of
  [http-method uri-suffix http-body]. Each file path may contribute
  one or more of these to the final sequence."
  [config-params file-paths]
  (->> file-paths
       (keep (partial file-path->rest-specs config-params))
       (apply concat)))

(defn load-file-paths [data-dir]
  (->> (io/file data-dir)
       (file-seq)
       (filter #(.isFile %))
       (map #(.getPath %))))

(defn update-geoserver! [{:keys [data-dir] :as config-params}]
  (let [http-responses       (->> (load-file-paths data-dir)
                                  (file-paths->rest-specs config-params)
                                  (mapv (partial make-rest-request config-params))) ; FIXME: use client/with-connection-pool for speed
        successful-responses (remove nil? http-responses)]
    (println "\nFinished updating GeoServer.\nSuccessful requests:"
             (count successful-responses)
             "\nFailed requests:"
             (- (count http-responses) (count successful-responses)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def base64-encoder (Base64/getUrlEncoder))

(defn encode-str [s]
  (.encodeToString base64-encoder (.getBytes s)))

(defn read-config-params [config-file-path]
  (if config-file-path
    (let [config-params (edn/read-string (slurp config-file-path))]
      ;; FIXME: Use clojure.spec to validate the config-params map
      (if (and (map? config-params)
               (every? keyword? (keys config-params)))
        config-params
        (throw (ex-info "The config-file must contain an EDN map whose keys are keywords."
                        {"--config-file" config-file-path}))))
    {}))

(def cli-options
  [["-c" "--config-file EDN"         "Path to an EDN file containing a map of configuration parameters"]
   ["-d" "--data-dir DIR"            "Path to the directory containing your GIS files"]
   ["-g" "--geoserver-rest-uri URI"  "URI of your GeoServer's REST extensions"]
   ["-u" "--geoserver-username USER" "GeoServer admin username"]
   ["-p" "--geoserver-password PASS" "GeoServer admin password"]])

(defn -main
  "Call this with the name of an EDN file containing the config-params
  map. The params will be read into a hash-map and passed on to the
  update-geoserver! function. So that we only have to calculate it
  once, the geoserver-auth-code is generated here from
  the :geoserver-username and :geoserver-password fields in the
  passed-in map and added to the in-memory hash-map under
  the :geoserver-auth-code entry."
  [& args]
  (println (str "geosync: Load a nested directory tree of GeoTIFFs and Shapefiles into a running GeoServer instance.\n"
                "Copyright 2020 Gary W. Johnson (gjohnson@sig-gis.com)\n"))
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    ;; {:options     The options map, keyed by :id, mapped to the parsed value
    ;;  :arguments   A vector of unprocessed arguments
    ;;  :summary     A string containing a minimal options summary
    ;;  :errors      A possible vector of error message strings generated during parsing; nil when no errors exist
    (if (or (seq errors) (empty? options))
      ;; FIXME: Use clojure.spec to validate the options map
      (do
        (when (seq errors)
          (run! println errors)
          (newline))
        (println (str "Usage:\n" summary)))
      (let [config-file-params  (read-config-params (:config-file options))
            command-line-params (into {} (remove (comp str/blank? val) (dissoc options :config-file)))
            config-params       (merge config-file-params command-line-params)]
        ;; FIXME: Use clojure.spec to validate the config-params map
        (update-geoserver!
         (assoc config-params
                :geoserver-auth-code
                (str "Basic " (encode-str (str (:geoserver-username config-params)
                                               ":"
                                               (:geoserver-password config-params)))))))))
  ;; Exit cleanly
  (shutdown-agents)
  (flush)
  (System/exit 0))
