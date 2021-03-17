(ns geosync.utils
  (:require [clojure.edn        :as edn]
            [clojure.string     :as str]
            [clojure.java.shell :refer [with-sh-dir sh]]
            [hiccup2.core       :refer [html]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XML Generation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro xml [& args]
  `(str (html {:mode :xml} ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GDAL Interaction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dms->dd [dms]
  (let [[d m s dir] (->> (re-find #"^([ \d]+)d([ \d]+)'([ \.0123456789]+)\"(\w)$" dms)
                         (rest)
                         (map edn/read-string))
        unsigned-dd (+ d (/ m 60.0) (/ s 3600.0))]
    (if (#{'S 'W} dir)
      (- unsigned-dd)
      unsigned-dd)))

(defn radians->degrees [rads]
  (/ (* rads 180.0) Math/PI))

;; FIXME: Consider using "gdalinfo -json file.tif" and parsing it with
;;        #(json/read-str % :key-fn keyword)
(defn run-gdal-info [data-dir file-path]
  (try
    (let [result (with-sh-dir data-dir (sh "gdalinfo" file-path))]
      (if (str/blank? (:out result))
        (throw (ex-info "gdalinfo failed"
                        {:data-dir data-dir :file-path file-path :error (:err result)}))
        (:out result)))
    (catch Exception e
      (throw (ex-info "gdalinfo failed"
                      {:data-dir data-dir :file-path file-path :error (.getMessage e)})))))

(defn run-gdal-srs-info [data-dir file-path]
  (try
    (let [result (with-sh-dir data-dir (sh "gdalsrsinfo" "-o" "epsg" file-path))]
      (if (str/blank? (:out result))
        (throw (ex-info "gdalsrsinfo failed"
                        {:data-dir data-dir :file-path file-path :error (:err result)}))
        (str/trim (:out result))))
    (catch Exception e
      (throw (ex-info "gdalsrsinfo failed"
                      {:data-dir data-dir :file-path file-path :error (.getMessage e)})))))

(defn extract-path [file-url]
  (rest (re-find #"^file://(.+)/([^/]+)$" file-url)))

(defn extract-georeferences [file-url]
  (let [[data-dir file-path] (extract-path file-url)
        gdal-info            (run-gdal-info data-dir file-path)
        proj-code            (run-gdal-srs-info data-dir file-path)

        cols-rows-regex      #"(?s)Size is (\d+), (\d+)"
        pixel-size-regex     #"(?s)Pixel Size = \(([\-\.0123456789]+),([\-\.0123456789]+)\)"
        origin-regex         #"(?s)Origin = \(([\-\.0123456789]+),([\-\.0123456789]+)\)"

        color-interp-regex   #"(?s)ColorInterp=(\w+)"
        native-crs-regex     #"(?s)Coordinate System is:\s*\n(.+?)\n[A-Z]"

        upper-left-regex     #"(?s)Upper Left\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        lower-left-regex     #"(?s)Lower Left\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        upper-right-regex    #"(?s)Upper Right\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        lower-right-regex    #"(?s)Lower Right\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"

        [cols rows]                (rest (re-find cols-rows-regex  gdal-info))
        [pixel-width pixel-height] (rest (re-find pixel-size-regex gdal-info))
        [x-origin y-origin]        (rest (re-find origin-regex     gdal-info))

        [native-ulx native-uly latlon-ulx latlon-uly] (rest (re-find upper-left-regex gdal-info))
        [native-llx native-lly latlon-llx latlon-lly] (rest (re-find lower-left-regex gdal-info))
        [native-urx native-ury latlon-urx latlon-ury] (rest (re-find upper-right-regex gdal-info))
        [native-lrx native-lry latlon-lrx latlon-lry] (rest (re-find lower-right-regex gdal-info))

        [native-ulx native-uly] (map edn/read-string [native-ulx native-uly])
        [native-llx native-lly] (map edn/read-string [native-llx native-lly])
        [native-urx native-ury] (map edn/read-string [native-urx native-ury])
        [native-lrx native-lry] (map edn/read-string [native-lrx native-lry])]

    {:proj-code    proj-code
     :cols-rows    (str cols " " rows)
     :pixel-width  pixel-width
     :pixel-height pixel-height
     :x-origin     x-origin
     :y-origin     y-origin
     :color-interp (second (re-find color-interp-regex gdal-info))
     :native-crs   (second (re-find native-crs-regex gdal-info))
     :native-min-x (str (min native-ulx native-llx))
     :native-max-x (str (max native-urx native-lrx))
     :native-min-y (str (min native-lly native-lry))
     :native-max-y (str (max native-uly native-ury))
     :latlon-min-x (str (apply min (map dms->dd [latlon-ulx latlon-llx])))
     :latlon-max-x (str (apply max (map dms->dd [latlon-urx latlon-lrx])))
     :latlon-min-y (str (apply min (map dms->dd [latlon-lly latlon-lry])))
     :latlon-max-y (str (apply max (map dms->dd [latlon-uly latlon-ury])))
     :shear-x      (str (radians->degrees (Math/asin (/ (- native-llx native-ulx)
                                                        (- native-uly native-lly)))))
     :shear-y      (str (radians->degrees (Math/asin (/ (- native-ury native-uly)
                                                        (- native-urx native-ulx)))))}))
