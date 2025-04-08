(ns geosync.action-hooks
  (:require [clj-http.client    :as client]
            [geosync.utils      :refer [camel->kebab]]
            [triangulum.logging :refer [log-str]]))

(defn- replace-symbols
  [request v]
  (cond (symbol? v) (-> v name camel->kebab keyword request)
        (list? v)   (into () (map (partial replace-symbols request)) v)
        (vector? v) (into [] (map (partial replace-symbols request)) v)
        (map? v)    (into {} (map (fn [[k v]] [k (replace-symbols request v)])) v)
        (set? v)    (into #{} (map (partial replace-symbols request)) v)
        :else       v))

(defn- process-query-params
  [query-params request]
  (reduce-kv (fn [acc k v]
               (assoc acc k (replace-symbols request v)))
             {}
             query-params))

(defn run-action-hooks!
  [action-hooks request selected-run-time]
  (doseq [[_ _ url query-params header-params]
          (filter (fn [[run-time action]]
                    (and (= run-time selected-run-time)
                         (= action (:action request))))
                  action-hooks)]
    (try
      (let [{:keys [auth-token]} header-params
            headers  (cond-> {"Accept"       "application/edn"
                              "Content-Type" "application/edn"}
                       auth-token (assoc "Authorization" (str "Bearer " auth-token)))
            response (client/get url {:query-params (process-query-params query-params request)
                                      :headers      headers})]
        (log-str (:body response)))
      (catch Exception e
        (log-str (:body (ex-message e)))))))
