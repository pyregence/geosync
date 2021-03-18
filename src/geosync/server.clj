(ns geosync.server
  (:require [clojure.core.async     :refer [<! >! chan go timeout]]
            [clojure.data.json      :as json]
            [clojure.string         :as s]
            [geosync.simple-sockets :as sockets]
            [triangulum.logging     :refer [log-str]]))

;;=============================================================================
;; Type Conversion Functions
;;=============================================================================

;; TODO: Remove when code is in triangulum
(defn camel->kebab
  "Converts camelString to kebab-string."
  [camel-string]
  (as-> camel-string text
    (s/split text #"(?<=[a-z])(?=[A-Z])")
    (map s/lower-case text)
    (s/join "-" text)))

;; TODO: Remove when code is in triangulum
(defn kebab->camel
  "Converts kebab-string to camelString."
  [kebab-string]
  (let [words (-> kebab-string
                  (s/lower-case)
                  (s/replace #"^[^a-z_$]|[^\w-]" "")
                  (s/split #"-"))]
    (->> (map s/capitalize (rest words))
         (cons (first words))
         (s/join ""))))

;; TODO: Remove when code is in triangulum
(defn val->int
  ([val]
   (val->int val (int -1)))
  ([val default]
   (cond
     (instance? Integer val) val
     (number? val)           (int val)
     :else                   (try
                               (Integer/parseInt val)
                               (catch Exception _ (int default))))))

;;=============================================================================
;; Server and Handler Functions
;;=============================================================================

(defonce job-queue (chan 100))

(defn process-requests!
  [{:keys [geosync-server-host geosync-server-port geoserver-rest-uri geoserver-username geoserver-password]}]
  (go (loop [{:keys [response-host response-port] :as request} (<! job-queue)]
        (log-str "Request: " request)
        ;; FIXME: Update GeoServer in response to the request using these config-params:
        ;; geoserver-rest-uri geoserver-username geoserver-password
        (<! (timeout 2000))
        (sockets/send-to-server! response-host
                                 (val->int response-port)
                                 (json/write-str {:status        0    ; Return 1 for errors
                                                  :message       "OK" ; Return error message if any
                                                  :response-host geosync-server-host
                                                  :response-port geosync-server-port}
                                                 :key-fn (comp kebab->camel name)))
        (recur (<! job-queue)))))

(defn handler
  [msg]
  (go
    (let [request (json/read-str msg :key-fn (comp keyword camel->kebab))]
      (>! job-queue request))))

(defn stop-server!
  []
  (sockets/stop-server!))

(defn start-server!
  [{:keys [geosync-server-port] :as config-params}]
  (log-str "Running server on port " geosync-server-port ".")
  (sockets/start-server! geosync-server-port handler)
  (process-requests! config-params))
