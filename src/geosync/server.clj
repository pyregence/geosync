(ns geosync.server
  (:import  java.text.SimpleDateFormat
            java.util.Date)
  (:require [clojure.core.async     :refer [>! chan go alts!]]
            [clojure.data.json      :as json]
            [clojure.spec.alpha     :as spec]
            [geosync.action-hooks   :refer [run-action-hooks!]]
            [geosync.core           :refer [add-directory-to-workspace!
                                            remove-workspace!]]
            [geosync.file-watcher   :as file-watcher]
            [geosync.simple-sockets :as sockets]
            [geosync.utils          :refer [nil-on-error
                                            camel->kebab
                                            kebab->camel
                                            hostname?
                                            port?
                                            non-empty-string?
                                            readable-directory?]]
            [triangulum.logging     :refer [log-str set-log-path!]]))

;;===========================================================
;; Request Validation
;;===========================================================

(spec/def ::response-host                  hostname?)
(spec/def ::response-port                  port?)
(spec/def ::action                         #{"add" "remove"})
(spec/def ::geoserver-workspace            non-empty-string?)
(spec/def ::data-dir                       readable-directory?)
(spec/def ::prioritize                     boolean?)
(spec/def ::geosync-server-request         (spec/and (spec/keys :req-un [::response-host
                                                                         ::response-port
                                                                         ::action
                                                                         ::geoserver-workspace]
                                                                :opt-un [::data-dir
                                                                         ::prioritize])
                                                     (fn [{:keys [action data-dir]}]
                                                       (or (and (= action "add") (string? data-dir))
                                                           (and (= action "remove") (nil? data-dir))))))
(spec/def ::geosync-server-request-minimal (spec/keys :req-un [::response-host
                                                               ::response-port]))

;;===========================================================
;; Server and Handler Functions
;;===========================================================

(defonce job-queue-size (atom 0))

(defonce stand-by-queue-size (atom 0))

(defonce job-queue (chan 1000
                         (map (fn [x]
                                (swap! job-queue-size inc)
                                (delay (swap! job-queue-size dec) x)))))

(defonce stand-by-queue (chan 1000
                              (map (fn [x]
                                     (swap! stand-by-queue-size inc)
                                     (delay (swap! stand-by-queue-size dec) x)))))

(defmulti process-request! (fn [_ {:keys [action]}] (keyword action)))

(defmethod process-request! :add
  [config-params _]
  (if (add-directory-to-workspace! config-params)
    [0 "GeoSync: Workspace updated."]
    [1 "GeoSync: Errors encountered during layer registration."]))

(defmethod process-request! :remove
  [config-params _]
  (if (remove-workspace! config-params)
    [0 "GeoSync: Workspace(s) removed."]
    [1 "GeoSync: Errors encountered during workspace removal."]))

(defn process-requests!
  [{:keys [geosync-server-host geosync-server-port action-hooks] :as config-params}]
  (go
    (loop [{:keys
            [response-host
             response-port
             geoserver-workspace
             data-dir] :as request} @(first (alts! [job-queue stand-by-queue]
                                                   :priority true))]
      (log-str "Processing Request: " request)
      (let [config-params           (-> config-params
                                        (dissoc :geosync-server-host :geosync-server-port)
                                        (assoc :geoserver-workspace geoserver-workspace :data-dir data-dir))
            [status-num status-msg] (try
                                      (let [_      (run-action-hooks! action-hooks request :before)
                                            status (process-request! config-params request)
                                            _      (run-action-hooks! action-hooks request :after)]
                                        status)
                                      (catch Exception e
                                        [1 (str "GeoSync: Error updating GeoServer: " (ex-message e))]))]
        (log-str "-> " status-msg)
        (sockets/send-to-server! response-host
                                 response-port
                                 (json/write-str (merge request
                                                        {:status        status-num
                                                         :message       status-msg
                                                         :response-host geosync-server-host
                                                         :response-port geosync-server-port})
                                                 :key-fn (comp kebab->camel name))))
      (recur @(first (alts! [job-queue stand-by-queue]
                            :priority true))))))

(defn handler
  [geosync-server-host geosync-server-port request-msg]
  (go
    (log-str "Received Request: " request-msg)
    (if-let [request (nil-on-error (json/read-str request-msg :key-fn (comp keyword camel->kebab)))]
      (let [[status status-msg] (try
                                  (if (spec/valid? ::geosync-server-request request)
                                    (let [queue-size (if (:prioritize request)
                                                       (do (>! job-queue request)
                                                           @job-queue-size)
                                                       (do (>! stand-by-queue request)
                                                           (+ @job-queue-size @stand-by-queue-size)))]
                                      [2 (format "GeoSync: You are number %d in the queue." queue-size)])
                                    [1 (str "GeoSync: Invalid request: " (spec/explain-str ::geosync-server-request request))])
                                  (catch AssertionError _
                                    [1 "GeoSync: Job queue limit exceeded! Dropping request!"])
                                  (catch Exception e
                                    [1 (str "GeoSync: Request validation error: " (ex-message e))]))]
        (log-str "-> " status-msg)
        (when (spec/valid? ::geosync-server-request-minimal request)
          (sockets/send-to-server! (:response-host request)
                                   (:response-port request)
                                   (json/write-str (merge request
                                                          {:status        status
                                                           :message       status-msg
                                                           :timestamp     (.format (SimpleDateFormat. "MM/dd HH:mm:ss") (Date.))
                                                           :response-host geosync-server-host
                                                           :response-port geosync-server-port})
                                                   :key-fn (comp kebab->camel name)))))
      (log-str "-> Invalid JSON"))))

(defonce watcher (atom nil))

(defn stop-server!
  []
  (sockets/stop-server!)
  (file-watcher/stop! @watcher)
  (reset! watcher nil))

(defn start-server!
  [{:keys [geosync-server-host geosync-server-port log-dir] :as config-params}]
  (when log-dir (set-log-path! log-dir))
  (log-str "Running server on port " geosync-server-port ".")
  (reset! watcher (file-watcher/start! config-params stand-by-queue))
  (sockets/start-server! geosync-server-port (partial handler geosync-server-host geosync-server-port))
  (process-requests! config-params))
