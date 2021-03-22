(ns geosync.server
  (:require [clojure.core.async     :refer [<! >! chan go]]
            [clojure.data.json      :as json]
            [clojure.spec.alpha     :as spec]
            [geosync.core           :refer [add-directory-to-workspace!]]
            [geosync.simple-sockets :as sockets]
            [geosync.utils          :refer [nil-on-error
                                            camel->kebab
                                            kebab->camel
                                            val->int
                                            hostname?
                                            port?
                                            non-empty-string?
                                            readable-directory?]]
            [triangulum.logging     :refer [log-str]]))

;;===========================================================
;; Request Validation
;;===========================================================

(spec/def ::response-host                  hostname?)
(spec/def ::response-port                  port?)
(spec/def ::geoserver-workspace            non-empty-string?)
(spec/def ::data-dir                       readable-directory?)
(spec/def ::geosync-server-request         (spec/keys :req-un [::response-host
                                                               ::response-port
                                                               ::geoserver-workspace
                                                               ::data-dir]))
(spec/def ::geosync-server-request-minimal (spec/keys :req-un [::response-host
                                                               ::response-port]))

;;===========================================================
;; Server and Handler Functions
;;===========================================================

(defonce job-queue (chan 100))

(defn process-requests!
  [{:keys [geosync-server-host geosync-server-port] :as config-params}]
  (go (loop [{:keys [response-host response-port geoserver-workspace data-dir] :as request} (<! job-queue)]
        (try
          (log-str "Processing Request: " request)
          ;; FIXME: Catch server errors from add-directory-to-workspace! and report them in the response message
          (add-directory-to-workspace! (-> config-params
                                           (dissoc :geosync-server-host
                                                   :geosync-server-port)
                                           (assoc :geoserver-workspace geoserver-workspace
                                                  :data-dir            data-dir)))
          (sockets/send-to-server! response-host
                                   (val->int response-port)
                                   (json/write-str (merge request
                                                          {:status        0 ; Return 1 for errors
                                                           :message       "OK" ; Return error message if any
                                                           :response-host geosync-server-host
                                                           :response-port geosync-server-port})
                                                   :key-fn (comp kebab->camel name)))
          (catch Exception e
            (log-str "Request Processing Exception: " (ex-message e))))
        (recur (<! job-queue)))))

(defn handler
  [geosync-server-host geosync-server-port request-msg]
  (go
    (log-str "Received Request: " request-msg)
    (if-let [request (nil-on-error (json/read-str request-msg :key-fn (comp keyword camel->kebab)))]
      (let [[status status-msg] (try
                                  (if (spec/valid? ::geosync-server-request request)
                                    (do
                                      (>! job-queue request)
                                      [2 "Added to Job Queue"])
                                    [1 (spec/explain-str ::geosync-server-request request)])
                                  (catch AssertionError _
                                    [1 "Job Queue Limit Exceeded! Dropping Request!"])
                                  (catch Exception e
                                    [1 (str "Validation Error: " (ex-message e))]))]
        (log-str "  -> " status-msg)
        (when (spec/valid? ::geosync-server-request-minimal request)
          (sockets/send-to-server! (:response-host request)
                                   (val->int (:response-port request))
                                   (json/write-str (merge request
                                                          {:status        status
                                                           :message       status-msg
                                                           :response-host geosync-server-host
                                                           :response-port geosync-server-port})
                                                   :key-fn (comp kebab->camel name)))))
      (log-str "  -> Invalid JSON"))))

(defn stop-server!
  []
  (sockets/stop-server!))

(defn start-server!
  [{:keys [geosync-server-host geosync-server-port] :as config-params}]
  (log-str "Running server on port " geosync-server-port ".")
  (sockets/start-server! geosync-server-port (partial handler geosync-server-host geosync-server-port))
  (process-requests! config-params))
