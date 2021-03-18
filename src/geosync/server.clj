(ns geosync.server
  (:require [clojure.core.async     :refer [<! >! chan go timeout]]
            [clojure.data.json      :as json]
            [clojure.spec.alpha     :as spec]
            [geosync.simple-sockets :as sockets]
            [geosync.utils          :refer [camel->kebab kebab->camel val->int hostname? port?]]
            [triangulum.logging     :refer [log-str]]))

;;===========================================================
;; Request Validation
;;===========================================================

(spec/def ::response-host hostname?)
(spec/def ::response-port port?)
(spec/def ::geosync-server-request (spec/keys :req-un [::response-host ::response-port]))

;;===========================================================
;; Server and Handler Functions
;;===========================================================

(defonce job-queue (chan 100))

(defn process-requests!
  [{:keys [geosync-server-host geosync-server-port geoserver-rest-uri geoserver-username geoserver-password]}]
  (go (loop [{:keys [response-host response-port] :as request} (<! job-queue)]
        (try
          (log-str "Request: " request)
          (<! (timeout 2000))
          ;; FIXME: Update GeoServer in response to the request using these config-params:
          ;;        geoserver-rest-uri geoserver-username geoserver-password
          (sockets/send-to-server! response-host
                                   (val->int response-port)
                                   (json/write-str {:status        0    ; Return 1 for errors
                                                    :message       "OK" ; Return error message if any
                                                    :response-host geosync-server-host
                                                    :response-port geosync-server-port}
                                                   :key-fn (comp kebab->camel name)))
          (catch Exception e
            (log-str "Request Processing Exception: " (ex-message e))))
        (recur (<! job-queue)))))

(defn handler
  [msg]
  (go
    (if-let [request (try
                       (json/read-str msg :key-fn (comp keyword camel->kebab))
                       (catch Exception _ nil))]
      (try
        (if (spec/valid? ::geosync-server-request request)
          (>! job-queue request)
          (log-str "Malformed Request: " msg))
        (catch AssertionError _
          (log-str "Job Queue Limit Exceeded! Dropping Request: " msg))
        (catch Exception e
          (log-str "Request Validation Error: " (ex-message e))))
      (log-str "Malformed Request: " msg))))

(defn stop-server!
  []
  (sockets/stop-server!))

(defn start-server!
  [{:keys [geosync-server-port] :as config-params}]
  (log-str "Running server on port " geosync-server-port ".")
  (sockets/start-server! geosync-server-port handler)
  (process-requests! config-params))
