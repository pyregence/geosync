;;; This file implements most of the commonly used operations in the
;;; GeoServer REST API as of version 2.17.0.

(ns geosync.rest-api
  (:require [clojure.string :as s]
            [geosync.utils  :refer [xml extract-georeferences]]))

;;=================================================================================
;;
;; Workspaces (http://docs.geoserver.org/latest/en/api/#1.0.0/workspaces.yaml)
;;
;;=================================================================================

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

;; FIXME: Add params that can be changed.
(defn update-workspace [workspace]
  ["PUT"
   (str "/workspaces/" workspace)
   (xml
    [:workspace
     [:name workspace]])])

(defn delete-workspace [workspace recurse?]
  ["DELETE"
   (str "/workspaces/" workspace "?recurse=" recurse?)
   nil])

;;=================================================================================
;;
;; Namespaces (http://docs.geoserver.org/latest/en/api/#1.0.0/namespaces.yaml)
;;
;;=================================================================================

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

;; FIXME: Add params that can be changed.
(defn update-namespace [uri-prefix workspace]
  ["PUT"
   (str "/namespaces/" workspace)
   (xml
    [:namespace
     [:prefix workspace]
     [:uri (str uri-prefix workspace)]])])

(defn delete-namespace [workspace]
  ["DELETE"
   (str "/namespaces/" workspace)
   nil])

;;=================================================================================
;;
;; Data Stores (http://docs.geoserver.org/latest/en/api/#1.0.0/datastores.yaml)
;;
;;=================================================================================

(defn get-data-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/datastores")
   nil])

(defn get-data-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

;; NOTE: Only Shapefile stores are currently supported.
;; NOTE: file-url should look like file:///path/to/nyc.shp
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
      [:entry {:key "url"} file-url]
      [:entry {:key "fstype"} "shape"]
      [:entry {:key "filetype"} "shapefile"]
      [:entry {:key "charset"} "ISO-8859-1"]
      [:entry {:key "create spatial index"} true]
      [:entry {:key "enable spatial index"} true]
      [:entry {:key "memory mapped buffer"} true]
      [:entry {:key "cache and reuse memory maps"} true]]])])

;; NOTE: Only Shapefile feature types are currently supported.
;; NOTE: file-url should look like file:///path/to/nyc.shp
(defn create-data-store-via-put [workspace store file-url]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/external.shp?configure=none")
   file-url])

;; FIXME: Add these connectionParameters
;; <connectionParameters>
;;   <entry key="memory mapped buffer">false</entry>
;;   <entry key="create spatial index">true</entry>
;;   <entry key="charset">ISO-8859-1</entry>
;;   <entry key="cache and reuse memory maps">false</entry>
;;   <entry key="url">file:/c:/data/states/</entry>
;;   <entry key="namespace">http://www.openplans.org/topp</entry>
;; </connectionParameters>
;; NOTE: Only Shapefile stores are currently supported.
;; NOTE: file-url should look like file:///path/to/nyc.shp
(defn update-data-store [workspace store file-url enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store)
   (xml
    [:dataStore
     [:workspace
      [:name workspace]]
     [:name store]
     [:type "Shapefile"]
     [:enabled enabled?]
     [:connectionParameters
      [:entry {:key "url"} file-url]
      [:entry {:key "fstype"} "shape"]
      [:entry {:key "filetype"} "shapefile"]
      [:entry {:key "charset"} "ISO-8859-1"]
      [:entry {:key "create spatial index"} true]
      [:entry {:key "enable spatial index"} true]
      [:entry {:key "memory mapped buffer"} true]
      [:entry {:key "cache and reuse memory maps"} true]]])])

(defn delete-data-store [workspace store]
  ["DELETE"
   (str "/workspaces/" workspace "/datastores/" store)
   nil])

;;=================================================================================
;;
;; Coverage Stores (http://docs.geoserver.org/latest/en/api/#1.0.0/coveragestores.yaml)
;;
;;=================================================================================

(defn get-coverage-stores [workspace]
  ["GET"
   (str "/workspaces/" workspace "/coveragestores")
   nil])

(defn get-coverage-store [workspace store]
  ["GET"
   (str "/workspaces/" workspace "/coveragestores/" store)
   nil])

;; NOTE: file-url should look like file:///path/to/nyc.tiff
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

(defn create-coverage-store-image-mosaic [workspace store file-url]
  ["PUT"
   (str "/workspaces/" workspace "/coveragestores/" store "/external.imagemosaic?configure=none")
   file-url
   "text/plain"])

;; NOTE: file-url should look like file:///path/to/nyc.tiff
(defn update-coverage-store [workspace store {:keys [type enabled? file-url]}]
  ["PUT"
   (str "/workspaces/" workspace "/coveragestores/" store)
   (xml
    [:coverageStore
     (when type [:type type])
     (when enabled? [:enabled enabled?])
     (when file-url [:url file-url])])])

(defn update-coverage-store-image-mosaic [workspace store file-url]
  ["POST"
   (str "/workspaces/" workspace "/coveragestores/" store "/external.imagemosaic")
   file-url
   "text/plain"])

(defn delete-coverage-store [workspace store]
  ["DELETE"
   (str "/workspaces/" workspace "/coveragestores/" store)
   nil])

;;=================================================================================
;;
;; Feature Types (http://docs.geoserver.org/latest/en/api/#1.0.0/featuretypes.yaml)
;;
;;=================================================================================

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

(defn create-feature-type [workspace store feature-type title abstract description
                           keywords crs srs max-features num-decimals]
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

;; NOTE: Only Shapefile feature types are currently supported.
;; NOTE: file-url should look like file:///path/to/nyc.shp
(defn create-feature-type-via-put [workspace store file-url]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/external.shp")
   file-url])

(defn create-feature-type-alias [workspace store old-feature-type new-feature-type]
  ["POST"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes")
   (xml
    [:featureType
     [:store {:class "dataStore"}
      [:name (str workspace ":" store)]]
     [:name new-feature-type]
     [:nativeName old-feature-type]])])

(defn update-feature-type [workspace store feature-type native-name title abstract
                           description keywords crs srs max-features num-decimals enabled?]
  ["PUT"
   (str "/workspaces/" workspace "/datastores/" store "/featuretypes/" feature-type)
   (xml
    [:featureType
     [:name feature-type]
     [:nativeName native-name]
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

;;=================================================================================
;;
;; Coverages (http://docs.geoserver.org/latest/en/api/#1.0.0/coverages.yaml)
;;
;;=================================================================================

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

(defn create-coverage [workspace store coverage layer-name]
  ["POST"
   (str "/workspaces/" workspace "/coveragestores/" store "/coverages")
   (xml
    [:coverage
     [:name coverage]
     [:nativeName layer-name]])])

(defn create-coverage-image-mosaic [workspace store]
  ["POST"
   (str "/workspaces/" workspace "/coveragestores/" store "/coverages")
   (xml
    [:coverage
     [:name store]
     [:nativeName store]
     [:title store]
     [:metadata
      [:entry {:key "time"}
       [:dimensionInfo
        [:enabled true]
        [:presentation "LIST"]
        [:units "ISO8601"]
        [:defaultValue
         [:strategy "MINIMUM"]]
        [:nearestMatchEnabled false]
        [:rawNearestMatchEnabled false]]]]])])

;; FIXME: GeoSync coverages load incorrectly:
;; - Dimensions tab throws errors (the coverageName "foo" is not supported)
;; - Missing keywords: {coverage}, WCS, GeoTIFF
;; - SRS handling should be: Reproject native to declared
;; - Rescale Pixels should be true
;; - Suggested tile size should be 512,512
;; - Data type should be Unsigned 16 bits
;; - Request SRS list and Response SRS list should not include EPSG:4326
;; - Native Format should be "GeoTIFF"
;; - Move GEOTIFF to top of Selected Formats list
;; - Default Style should be "fire-area"
;; - Grid subset bounds should be set statically(?)
#_(defn create-coverage [workspace store coverage title abstract description
                         keywords proj-code interpolation-method file-url]
    (let [gdal-info (extract-georeferences file-url)
          proj-code (or proj-code (:proj-code gdal-info))]
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
         [:srs proj-code]
         [:nativeBoundingBox
          [:crs proj-code]
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
          [:crs proj-code]
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
           [:name (s/upper-case (str (:color-interp gdal-info) "_INDEX"))]
           [:description "GridSampleDimension[-Infinity,Infinity]"]]]
         [:requestSRS
          [:string "EPSG:4326"]
          (if (not= proj-code "EPSG:4326")
            [:string proj-code])]
         [:responseSRS
          [:string "EPSG:4326"]
          (if (not= proj-code "EPSG:4326")
            [:string proj-code])]
         [:enabled true]])]))

;; NOTE: Only GeoTIFF coverages are currently supported.
(defn create-coverage-via-put [workspace store file-url]
  ["PUT"
   (str "/workspaces/" workspace "/coveragestores/" store "/external.geotiff?coverageName=" store)
   file-url])

(defn update-coverage [workspace store coverage native-name title abstract description
                       keywords proj-code interpolation-method file-url enabled?]
  (let [gdal-info (extract-georeferences file-url)
        proj-code (or proj-code (:proj-code gdal-info))]
    ["PUT"
     (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
     (xml
      [:coverage
       [:store
        [:name (str workspace ":" store)]]
       [:name coverage]
       [:nativeName native-name]
       [:nativeCoverageName native-name]
       [:title title]
       [:abstract abstract]
       [:description description]
       [:keywords
        (map (fn [k] [:string k]) keywords)]
       [:nativeCRS (:native-crs gdal-info)]
       [:srs proj-code]
       [:nativeBoundingBox
        [:crs proj-code]
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
        [:crs proj-code]
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
         [:name (s/upper-case (str (:color-interp gdal-info) "_INDEX"))]
         [:description "GridSampleDimension[-Infinity,Infinity]"]]]
       [:requestSRS
        [:string "EPSG:4326"]
        (if (not= proj-code "EPSG:4326")
          [:string proj-code])]
       [:responseSRS
        [:string "EPSG:4326"]
        (if (not= proj-code "EPSG:4326")
          [:string proj-code])]
       [:enabled enabled?]])]))

(defn delete-coverage [workspace store coverage]
  ["DELETE"
   (str "/workspaces/" workspace "/coveragestores/" store "/coverages/" coverage)
   nil])

;;=================================================================================
;;
;; Layers (https://docs.geoserver.org/latest/en/api/#1.0.0/layers.yaml)
;;
;;=================================================================================

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
  ([layer path layer-type default-style styles resource-type resource-name opaque?]
   ["PUT"
    (str "/layers/" layer)
    (xml
     [:layer
      [:name layer]
      [:path path]
      [:type layer-type]
      [:defaultStyle
       [:name default-style]]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]
      [:resource {:class resource-type}
       [:name resource-name]]
      [:opaque opaque?]])])
  ([workspace layer path layer-type default-style styles resource-type resource-name opaque?]
   ["PUT"
    (str "/workspaces/" workspace "/layers/" layer)
    (xml
     [:layer
      [:name layer]
      [:path path]
      [:type layer-type]
      [:defaultStyle
       [:name default-style]]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]
      [:resource {:class resource-type}
       [:name resource-name]]
      [:opaque opaque?]])]))

(defn update-layer-style
  ([layer styles layer-type]
   ["PUT"
    (str "/layers/" layer)
    (xml
     [:layer
      [:name layer]
      [:type (case layer-type
               :vector "VECTOR"
               :raster "RASTER"
               nil)]
      [:defaultStyle
       [:name (if (coll? styles)
                (first styles)
                styles)]]
      (when (coll? styles)
        [:styles
         (map (fn [s] [:style [:name s]]) styles)])])])

  ([workspace layer styles layer-type]
   ["PUT"
    (str "/workspaces/" workspace "/layers/" layer)
    (xml
     [:layer
      [:name layer]
      [:type (case layer-type
               :vector "VECTOR"
               :raster "RASTER"
               nil)]
      [:defaultStyle
       [:name (if (coll? styles)
                (first styles)
                styles)]]
      (when (coll? styles)
        [:styles
         (map (fn [s] [:style [:name s]]) styles)])])]))

(defn delete-layer
  ([layer]
   ["DELETE"
    (str "/layers/" layer)
    nil])
  ([workspace layer]
   ["DELETE"
    (str "/workspaces/" workspace "/layers/" layer)
    nil]))

;;=================================================================================
;;
;; Layer Groups (http://docs.geoserver.org/latest/en/api/#1.0.0/layergroups.yaml)
;;
;;=================================================================================

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
  ([layer-group mode title abstract keywords layers styles]
   ["POST"
    "/layergroups"
    (xml
     [:layerGroup
      [:name layer-group]
      [:mode mode]
      [:title title]
      [:abstractTxt abstract]
      [:keywords
       (map (fn [k] [:string k]) keywords)]
      [:publishables
       (map (fn [l] [:published {:type "layer"} [:name l]]) layers)]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]])])
  ([workspace layer-group mode title abstract keywords layers styles]
   ["POST"
    (str "/workspaces/" workspace "/layergroups")
    (xml
     [:layerGroup
      [:workspace
       [:name workspace]]
      [:name layer-group]
      [:mode mode]
      [:title title]
      [:abstractTxt abstract]
      [:keywords
       (map (fn [k] [:string k]) keywords)]
      [:publishables
       (map (fn [l] [:published {:type "layer"} [:name l]]) layers)]
      [:styles
       (map (fn [s] [:style [:name s]]) styles)]])]))

(defn update-layer-group
  ([layer-group mode title abstract description keywords layers styles]
   ["PUT"
    (str "/layergroups/" layer-group)
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
   ["PUT"
    (str "/workspaces/" workspace "/layergroups/" layer-group)
    (xml
     [:layerGroup
      [:workspace
       [:name workspace]]
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

(defn delete-layer-group
  ([layer-group]
   ["DELETE"
    (str "/layergroups/" layer-group)
    nil])
  ([workspace layer-group]
   ["DELETE"
    (str "/workspaces/" workspace "/layergroups/" layer-group)
    nil]))

;;=================================================================================
;;
;; Styles (http://docs.geoserver.org/latest/en/api/#1.0.0/styles.yaml)
;;
;;=================================================================================

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
  ([style file-url]
   ["PUT"
    (str "/styles/" style)
    (xml
     [:style
      [:name style]
      [:filename file-url]])])
  ([workspace style file-url]
   ["PUT"
    (str "/workspaces/" workspace "/styles/" style)
    (xml
     [:style
      [:name style]
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
