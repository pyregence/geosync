(ns geosync.action-hooks
  (:require [clj-http.client :as client]
            [triangulum.logging :refer [log-str]]
            [geosync.utils :refer [camel->kebab]]
            [clojure.data.json :as json]))

(defn- replace-symbol [request v]
  (if (symbol? v)
    (get request (keyword (camel->kebab (name v))))
    v))

(defn- replace-symbols
  [request v]
  (if (vector? v)
    (json/write-str (mapv (partial replace-symbol request) v))
    v))

(defn process-query-params
  [query-params request]
  (reduce-kv (fn [acc k v]
               (assoc acc (name k) (replace-symbols request v)))
             {}
             query-params))

(defn run-action-hooks!
  [action-hooks request selected-run-time]
  (doseq [[_ _ url query-params] (filter (fn [[run-time action]]
                                           (and (= run-time selected-run-time)
                                                (= action (:action request))))
                                         action-hooks)]
    (try
      (let [response (client/get url {:query-params (process-query-params query-params request)})]
        (log-str (:body response)))
      (catch Exception e
        (log-str (ex-message e))))))
