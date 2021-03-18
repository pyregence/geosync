(ns geosync.server
  (:require [clojure.core.async     :refer [<! >! chan go timeout]]
            [clojure.data.json      :as json]
            [clojure.string         :as s]
            [geosync.simple-sockets :as sockets]
            [triangulum.logging     :refer [log-str]]))

;; FIXME: use <! >! chan go timeout json/ s/

;; FIXME: stub
(defn process-requests! [config-params]
  nil)

;; FIXME: stub
(defn handler [msg]
  nil)

(defn start-server! [{:keys [geosync-server-port] :as config-params}]
  (log-str "Running server on port " geosync-server-port ".")
  (sockets/start-server! geosync-server-port handler)
  (process-requests! config-params))
