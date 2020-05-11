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
;;; This file implements most of the commonly used operations in the
;;; GeoServer REST API as of version 2.17.0.

(ns geosync.rest-api
  (:require [clojure.string :as str]
            [geosync.utils  :refer [xml extract-georeferences]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Workspaces (http://docs.geoserver.org/latest/en/api/#1.0.0/workspaces.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-workspaces []
  ["GET"
   "/workspaces"
   nil])

(defn get-workspace [workspace]
  ["GET"
   (str "/workspaces/" workspace)
   nil])

(defn create-workspace [workspace]
  ["POST"
   "/workspaces"
   (xml
    [:workspace
     [:name workspace]])])

(defn update-workspace [workspace new-workspace]
  ["PUT"
   (str "/workspaces/" workspace)
   (xml
    [:workspace
     [:name new-workspace]])])

(defn delete-workspace [workspace]
  ["DELETE"
   (str "/workspaces/" workspace)
   nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Namespaces (http://docs.geoserver.org/latest/en/api/#1.0.0/namespaces.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-namespaces []
  ["GET"
   "/namespaces"
   nil])

(defn get-namespace [workspace]
  ["GET"
   (str "/namespaces/" workspace)
   nil])

(defn create-namespace [uri-prefix workspace]
  ["POST"
   "/namespaces"
   (xml
    [:namespace
     [:prefix workspace]
     [:uri (str uri-prefix workspace)]])])

(defn update-namespace [uri-prefix workspace new-workspace]
  ["PUT"
   (str "/namespaces/" workspace)
   (xml
    [:namespace
     [:prefix new-workspace]
     [:uri (str uri-prefix new-workspace)]])])

(defn delete-namespace [workspace]
  ["DELETE"
   (str "/namespaces/" workspace)
   nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Data Stores (http://docs.geoserver.org/latest/en/api/#1.0.0/datastores.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-data-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/datastores")
   nil])

(defn get-data-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

;; NOTE: Only Shapefile stores are currently supported.
;; NOTE: file-url should look like file:/path/to/nyc.shp
(defn create-data-store [workspace store file-url]
  ["POST"
   (str "/workspaces/" workspace "/datastores")
   (xml
    [:dataStore
     [:workspace
      [:name workspace]]
     [:name store]
     [:type "Shapefile"]
     [:enabled true]
     [:connectionParameters
      [:url file-url]]])])

;; NOTE: Only Shapefile stores are currently supported.
;; NOTE: file-url should look like file:/path/to/nyc.shp
(defn update-data-store [workspace store new-store file-url enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store)
   (xml
    [:dataStore
     [:workspace
      [:name workspace]]
     [:name new-store]
     [:type "Shapefile"]
     [:enabled enabled?]
     [:connectionParameters
      [:url file-url]]])])

(defn delete-data-store [workspace store]
  ["DELETE"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coverage Stores (http://docs.geoserver.org/latest/en/api/#1.0.0/coveragestores.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-coverage-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/coveragestores")
   nil])

(defn get-coverage-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/coveragestores/" store)
   nil])

;; NOTE: file-url should look like file:/path/to/nyc.tiff
(defn create-coverage-store [workspace store file-url]
  ["POST"
   (str "/workspaces/" workspace "/coveragestores")
   (xml
    [:coverageStore
     [:workspace
      [:name workspace]]
     [:name store]
     [:type "GeoTIFF"]
     [:enabled true]
     [:url file-url]])])

;; NOTE: file-url should look like file:/path/to/nyc.tiff
(defn update-coverage-store [workspace store new-store file-url enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/coveragestores/" store)
   (xml
    [:coverageStore
     [:workspace
      [:name workspace]]
     [:name new-store]
     [:type "GeoTIFF"]
     [:enabled enabled?]
     [:url file-url]])])

(defn delete-coverage-store [workspace store]
  ["DELETE"
   (str "/workspaces/" workspace "/coveragestores/" store)
   nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Feature Types (http://docs.geoserver.org/latest/en/api/#1.0.0/featuretypes.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-feature-types
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/featuretypes")
    nil])
  ([workspace store]
   ["GET"
    (str "/workspaces/" workspace "/datastores/" store "/featuretypes")
    nil]))

(defn get-feature-type
  ([workspace feature-type]
   ["GET"
    (str "/workspaces/" workspace "/featuretypes/" feature-type)
    nil])
  ([workspace store feature-type]
   ["GET"
    (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
    nil]))

;; NOTE: Only Shapefile feature types are currently supported.
(defn create-feature-type-via-put [workspace store file-url]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/external.shp")
   file-url])

(defn create-feature-type [workspace store feature-type title abstract description keywords crs srs max-features num-decimals]
  ["POST"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes")
   (xml
    [:featureType
     [:name feature-type]
     [:nativeName feature-type]
     [:title title]
     [:abstract abstract]
     [:description description]
     [:keywords
      (map (fn [k] [:string k]) keywords)]
     [:nativeCRS crs]
     [:srs srs]
     [:maxFeatures max-features]
     [:numDecimals num-decimals]
     [:enabled true]])])

(defn update-feature-type [workspace store feature-type new-feature-type title abstract description keywords crs srs max-features num-decimals enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
   (xml
    [:featureType
     [:name new-feature-type]
     [:nativeName new-feature-type]
     [:title title]
     [:abstract abstract]
     [:description description]
     [:keywords
      (map (fn [k] [:string k]) keywords)]
     [:nativeCRS crs]
     [:srs srs]
     [:maxFeatures max-features]
     [:numDecimals num-decimals]
     [:enabled enabled?]])])

(defn delete-feature-type [workspace store feature-type]
  ["DELETE"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
   nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coverages (http://docs.geoserver.org/latest/en/api/#1.0.0/coverages.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-coverages
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/coverages")
    nil])
  ([workspace store]
   ["GET"
    (str "/workspaces/" workspace "/coveragestores/" store "/coverages")
    nil]))

(defn get-coverage
  ([workspace coverage]
   ["GET"
    (str "/workspaces/" workspace "/coverages/" coverage)
    nil])
  ([workspace store coverage]
   ["GET"
    (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
    nil]))

;; FIXME: Throws 500 Server Error
;; - ((:proj-code gdal-info) should be EPSG:3310)
(defn create-coverage [workspace store coverage title abstract description keywords interpolation-method file-url]
  (let [gdal-info (extract-georeferences file-url)]
    ["POST"
     (str "/workspaces/" workspace "/coveragestores/" store "/coverages")
     (xml
      [:coverage
       [:store
        [:name (str workspace ":" store)]]
       [:name coverage]
       [:nativeName coverage]
       [:nativeCoverageName coverage]
       [:title title]
       [:abstract abstract]
       [:description description]
       [:keywords
        (map (fn [k] [:string k]) keywords)]
       [:nativeCRS (:native-crs gdal-info)]
       [:srs (:proj-code gdal-info)]
       [:nativeBoundingBox
        [:crs (:proj-code gdal-info)]
        [:minx (:native-min-x gdal-info)]
        [:maxx (:native-max-x gdal-info)]
        [:miny (:native-min-y gdal-info)]
        [:maxy (:native-max-y gdal-info)]]
       [:latLonBoundingBox
        [:crs "EPSG:4326"]
        [:minx (:latlon-min-x gdal-info)]
        [:maxx (:latlon-max-x gdal-info)]
        [:miny (:latlon-min-y gdal-info)]
        [:maxy (:latlon-max-y gdal-info)]]
       [:projectionPolicy "REPROJECT_TO_DECLARED"]
       [:nativeFormat "GEOTIFF"]
       [:grid {:dimension "2"}
        [:crs (:proj-code gdal-info)]
        [:range
         [:low "0 0"]
         [:high (:cols-rows gdal-info)]]
        [:transform
         [:scaleX     (:pixel-width  gdal-info)]
         [:scaleY     (:pixel-height gdal-info)]
         [:shearX     (:shear-x      gdal-info)]
         [:shearY     (:shear-y      gdal-info)]
         [:translateX (:x-origin     gdal-info)]
         [:translateY (:y-origin     gdal-info)]]]
       [:supportedFormats
        [:string "GIF"]
        [:string "PNG"]
        [:string "JPEG"]
        [:string "TIFF"]
        [:string "GEOTIFF"]]
       [:interpolationMethods
        [:string "nearest neighbor"]
        [:string "bilinear"]
        [:string "bicubic"]]
       [:defaultInterpolationMethod interpolation-method]
       [:dimensions
        [:coverageDimension
         [:name (str/upper-case (str (:color-interp gdal-info) "_INDEX"))]
         [:description "GridSampleDimension[-Infinity,Infinity]"]]]
       [:requestSRS
        [:string "EPSG:4326"]
        (if (not= (:proj-code gdal-info) "EPSG:4326")
          [:string (:proj-code gdal-info)])]
       [:responseSRS
        [:string "EPSG:4326"]
        (if (not= (:proj-code gdal-info) "EPSG:4326")
          [:string (:proj-code gdal-info)])]
       [:enabled true]])]))

(defn update-coverage [workspace store coverage new-coverage title abstract description keywords interpolation-method file-url enabled?]
  (let [gdal-info (extract-georeferences file-url)]
    ["PUT"
     (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
     (xml
      [:coverage
       [:store
        [:name (str workspace ":" store)]]
       [:name new-coverage]
       [:nativeName new-coverage]
       [:nativeCoverageName new-coverage]
       [:title title]
       [:abstract abstract]
       [:description description]
       [:keywords
        (map (fn [k] [:string k]) keywords)]
       [:nativeCRS (:native-crs gdal-info)]
       [:srs (:proj-code gdal-info)]
       [:nativeBoundingBox
        [:crs (:proj-code gdal-info)]
        [:minx (:native-min-x gdal-info)]
        [:maxx (:native-max-x gdal-info)]
        [:miny (:native-min-y gdal-info)]
        [:maxy (:native-max-y gdal-info)]]
       [:latLonBoundingBox
        [:crs "EPSG:4326"]
        [:minx (:latlon-min-x gdal-info)]
        [:maxx (:latlon-max-x gdal-info)]
        [:miny (:latlon-min-y gdal-info)]
        [:maxy (:latlon-max-y gdal-info)]]
       [:grid {:dimension "2"}
        [:crs (:proj-code gdal-info)]
        [:range
         [:low "0 0"]
         [:high (:cols-rows gdal-info)]]
        [:transform
         [:scaleX     (:pixel-width  gdal-info)]
         [:scaleY     (:pixel-height gdal-info)]
         [:shearX     (:shear-x      gdal-info)]
         [:shearY     (:shear-y      gdal-info)]
         [:translateX (:x-origin     gdal-info)]
         [:translateY (:y-origin     gdal-info)]]]
       [:defaultInterpolationMethod interpolation-method]
       [:dimensions
        [:coverageDimension
         [:name (str/upper-case (str (:color-interp gdal-info) "_INDEX"))]
         [:description "GridSampleDimension[-Infinity,Infinity]"]]]
       [:requestSRS
        [:string "EPSG:4326"]
        (if (not= (:proj-code gdal-info) "EPSG:4326")
          [:string (:proj-code gdal-info)])]
       [:responseSRS
        [:string "EPSG:4326"]
        (if (not= (:proj-code gdal-info) "EPSG:4326")
          [:string (:proj-code gdal-info)])]
       [:enabled enabled?]])]))

(defn delete-coverage [workspace store coverage]
  ["DELETE"
   (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
   nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Layers (https://docs.geoserver.org/latest/en/api/#1.0.0/layers.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-layers
  ([]
   ["GET"
    "/layers"
    nil])
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/layers")
    nil]))

(defn get-layer
  ([layer]
   ["GET"
    (str "/layers/" layer)
    nil])
  ([workspace layer]
   ["GET"
    (str "/workspaces/" workspace "/layers/" layer)
    nil]))

;; NOTE: Not part of the API. Use create-coverage or create-feature-type instead.
(defn create-layer [])

(defn update-layer
  ([layer new-layer path layer-type default-style styles resource-type resource-name opaque?]
   ["PUT"
    (str "/layers/" layer)
    (xml
     [:layer
      [:name new-layer]
      [:path path]
      [:type layer-type]
      [:defaultStyle
       [:name default-style]]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]
      [:resource {:class resource-type}
       [:name resource-name]]
      [:opaque opaque?]])])
  ([workspace layer new-layer path layer-type default-style styles resource-type resource-name opaque?]
   ["PUT"
    (str "/workspaces/" workspace "/layers/" layer)
    (xml
     [:layer
      [:name new-layer]
      [:path path]
      [:type layer-type]
      [:defaultStyle
       [:name default-style]]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]
      [:resource {:class resource-type}
       [:name resource-name]]
      [:opaque opaque?]])]))

(defn delete-layer
  ([layer]
   ["DELETE"
    (str "/layers/" layer)
    nil])
  ([workspace layer]
   ["DELETE"
    (str "/workspaces/" workspace "/layers/" layer)
    nil]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Layer Groups (http://docs.geoserver.org/latest/en/api/#1.0.0/layergroups.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-layer-groups
  ([]
   ["GET"
    "/layergroups"
    nil])
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/layergroups")
    nil]))

(defn get-layer-group
  ([layer-group]
   ["GET"
    (str "/layergroups/" layer-group)
    nil])
  ([workspace layer-group]
   ["GET"
    (str "/workspaces/" workspace "/layergroups/" layer-group)
    nil]))

(defn create-layer-group
  ([layer-group mode title abstract description keywords layers styles]
   ["POST"
    "/layergroups"
    (xml
     [:layerGroup
      [:name layer-group]
      [:mode mode]
      [:title title]
      [:abstract abstract]
      [:description description]
      [:keywords
       (map (fn [k] [:string k]) keywords)]
      [:publishables
       (map (fn [l] [:published [:name l]]) layers)]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]])])
  ([workspace layer-group mode title abstract description keywords layers styles]
   ["POST"
    (str "/workspaces/" workspace "/layergroups")
    (xml
     [:layerGroup
      [:name layer-group]
      [:mode mode]
      [:title title]
      [:abstract abstract]
      [:description description]
      [:keywords
       (map (fn [k] [:string k]) keywords)]
      [:publishables
       (map (fn [l] [:published [:name l]]) layers)]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]])]))

(defn update-layer-group
  ([layer-group new-layer-group mode title abstract description keywords layers styles]
   ["PUT"
    (str "/layergroups/" layer-group)
    (xml
     [:layerGroup
      [:name new-layer-group]
      [:mode mode]
      [:title title]
      [:abstract abstract]
      [:description description]
      [:keywords
       (map (fn [k] [:string k]) keywords)]
      [:publishables
       (map (fn [l] [:published [:name l]]) layers)]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]])])
  ([workspace layer-group new-layer-group mode title abstract description keywords layers styles]
   ["PUT"
    (str "/workspaces/" workspace "/layergroups/" layer-group)
    (xml
     [:layerGroup
      [:name new-layer-group]
      [:mode mode]
      [:title title]
      [:abstract abstract]
      [:description description]
      [:keywords
       (map (fn [k] [:string k]) keywords)]
      [:publishables
       (map (fn [l] [:published [:name l]]) layers)]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]])]))

(defn delete-layer-group
  ([layer-group]
   ["DELETE"
    (str "/layergroups/" layer-group)
    nil])
  ([workspace layer-group]
   ["DELETE"
    (str "/workspaces/" workspace "/layergroups/" layer-group)
    nil]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Styles (http://docs.geoserver.org/latest/en/api/#1.0.0/styles.yaml)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-styles
  ([]
   ["GET"
    "/styles"
    nil])
  ([workspace]
   ["GET"
    (str "/workspaces/" workspace "/styles")
    nil]))

(defn get-style
  ([style]
   ["GET"
    (str "/styles/" style)
    nil])
  ([workspace style]
   ["GET"
    (str "/workspaces/" workspace "/styles/" style)
    nil]))

(defn create-style
  ([style file-url]
   ["POST"
    "/styles"
    (xml
     [:style
      [:name style]
      [:filename file-url]])])
  ([workspace style file-url]
   ["POST"
    (str "/workspaces/" workspace "/styles")
    (xml
     [:style
      [:name style]
      [:filename file-url]])]))

(defn update-style
  ([style new-style file-url]
   ["PUT"
    (str "/styles/" style)
    (xml
     [:style
      [:name new-style]
      [:filename file-url]])])
  ([workspace style new-style file-url]
   ["PUT"
    (str "/workspaces/" workspace "/styles/" style)
    (xml
     [:style
      [:name new-style]
      [:filename file-url]])]))

(defn delete-style
  ([style]
   ["DELETE"
    (str "/styles/" style)
    nil])
  ([workspace style]
   ["DELETE"
    (str "/workspaces/" workspace "/styles/" style)
    nil]))
