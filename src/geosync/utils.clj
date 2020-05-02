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
;;; This file provides a number of utility functions required by the
;;; rest of the application.

(ns geosync.utils
  (:require [clojure.java.shell :refer [with-sh-dir sh]]
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

;; FIXME: Make sure these functions are application generic.

(defn extract-path
  [uri]
  (second (re-find #"^file:/raid/geodata/(.*)$" uri)))

(defn run-gdal-info
  [geoserver-data-dir uri]
  (let [result (with-sh-dir geoserver-data-dir
                 (:out (sh "gdalinfo" (extract-path uri))))]
    (if (.isEmpty result)
      (throw (Exception. (str "gdalinfo failed for file: "
                              geoserver-data-dir
                              (if-not (.endsWith geoserver-data-dir "/") "/")
                              (extract-path uri))))
      result)))

(defn dms->dd
  [dms]
  (let [[d m s dir] (map read-string (rest (re-find #"^([ \d]+)d([ \d]+)'([ \.0123456789]+)\"(\w)$" dms)))
        unsigned-dd (+ d (/ m 60.0) (/ s 3600.0))]
    (if (#{'S 'W} dir)
      (- unsigned-dd)
      unsigned-dd)))

(defn radians->degrees
  [rads]
  (/ (* rads 180.0) Math/PI))

(defn extract-georeferences
  [geoserver-data-dir uri]
  (let [gdal-info (run-gdal-info geoserver-data-dir uri)
        cols-rows-regex    #"(?s)Size is (\d+), (\d+)"
        pixel-size-regex   #"(?s)Pixel Size = \(([\-\.0123456789]+),([\-\.0123456789]+)\)"
        origin-regex       #"(?s)Origin = \(([\-\.0123456789]+),([\-\.0123456789]+)\)"

        color-interp-regex #"(?s)ColorInterp=(\w+)"
        native-crs-regex   #"(?s)Coordinate System is:\s*\n(.+)Origin"

        upper-left-regex   #"(?s)Upper Left\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        lower-left-regex   #"(?s)Lower Left\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        upper-right-regex  #"(?s)Upper Right\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"
        lower-right-regex  #"(?s)Lower Right\s+\(\s*([\-\.0123456789]+),\s*([\-\.0123456789]+)\)\s+\(\s*([^,]+),\s*([^\)]+)\)"

        [cols rows]                (rest (re-find cols-rows-regex  gdal-info))
        [pixel-width pixel-height] (rest (re-find pixel-size-regex gdal-info))
        [x-origin y-origin]        (rest (re-find origin-regex     gdal-info))

        [ul-native-x ul-native-y ul-latlon-x ul-latlon-y] (rest (re-find upper-left-regex gdal-info))
        [ll-native-x ll-native-y ll-latlon-x ll-latlon-y] (rest (re-find lower-left-regex gdal-info))
        [ur-native-x ur-native-y ur-latlon-x ur-latlon-y] (rest (re-find upper-right-regex gdal-info))
        [lr-native-x lr-native-y lr-latlon-x lr-latlon-y] (rest (re-find lower-right-regex gdal-info))

        [ul-native-x ul-native-y] (map read-string [ul-native-x ul-native-y])
        [ll-native-x ll-native-y] (map read-string [ll-native-x ll-native-y])
        [ur-native-x ur-native-y] (map read-string [ur-native-x ur-native-y])
        [lr-native-x lr-native-y] (map read-string [lr-native-x lr-native-y])]

    {:cols-rows    (str cols " " rows)
     :pixel-width  pixel-width
     :pixel-height pixel-height
     :x-origin     x-origin
     :y-origin     y-origin
     :color-interp (second (re-find color-interp-regex gdal-info))
     :native-crs   (second (re-find native-crs-regex gdal-info))
     :native-min-x (str (min ul-native-x ll-native-x))
     :native-max-x (str (max ur-native-x lr-native-x))
     :native-min-y (str (min ll-native-y lr-native-y))
     :native-max-y (str (max ul-native-y ur-native-y))
     :latlon-min-x (str (apply min (map dms->dd [ul-latlon-x ll-latlon-x])))
     :latlon-max-x (str (apply max (map dms->dd [ur-latlon-x lr-latlon-x])))
     :latlon-min-y (str (apply min (map dms->dd [ll-latlon-y lr-latlon-y])))
     :latlon-max-y (str (apply max (map dms->dd [ul-latlon-y ur-latlon-y])))
     :shear-x      (str (radians->degrees (Math/asin (/ (- ll-native-x ul-native-x) (- ul-native-y ll-native-y)))))
     :shear-y      (str (radians->degrees (Math/asin (/ (- ur-native-y ul-native-y) (- ur-native-x ul-native-x)))))}))
