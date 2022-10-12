(ns geosync.file-watcher
  (:require [clojure.core.async   :refer [<! >! >!! go-loop timeout]]
            [clojure.string       :as s]
            [nextjournal.beholder :as beholder]
            [triangulum.logging   :refer [log-str]]))

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defonce event-in-progress (atom {}))

(def request-minimum
  {:response-host "some.host"
   :response-port 1234})

(defn- count-down [{:keys [job-queue] :as _config} workspace request-args]
  (go-loop [seconds (get @event-in-progress workspace)]
    (if (> @seconds 0)
      (do (<! (timeout 1000))
          (swap! seconds dec)
          (recur (get @event-in-progress workspace)))
      (do (swap! event-in-progress dissoc workspace)
          (when request-args
            (>! job-queue (merge request-minimum request-args)))))))

(defn- reset-timer [workspace]
  (swap! event-in-progress assoc workspace (atom 10)))

(defn- start-counter
  ([config workspace]
   (start-counter config workspace nil))

  ([config workspace request-args]
   (reset-timer workspace)
   (count-down config workspace request-args)))

;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

;;FIXME some files are created and then deleted during the registration process.
;;The deletion triggers the deregistration of the workspace. These are some
;;known files that should be ignored.
(def files-to-ignore #{"sample_image.dat" ".properties"})

(defn- parse-workspace
  "Based on the :dir and :folder-name->regex keys from the config file
   and the path of the current file, a GeoServer workspace string is returned."
  [{:keys [dir folder-name->regex] :as _config} path]
  (let [folder-regex (re-pattern (format "(?<=%s/)[\\w-]+" dir))
        folder-name  (re-find folder-regex path)]
    (when-let [regex (get folder-name->regex folder-name)]
      (some-> (re-find regex path)
              (s/replace "_" "-")
              (s/replace "/" "_")))))

(defn- process-event
  [{:keys [job-queue] :as config} event-type path]
  (when (not-any? #(s/includes? path %) files-to-ignore)
    ;; If no workspace is found then no action will be taken.
    ;; Any folder that you wish an action to be taken for **must**
    ;; be included as a key in the folder-name->regex config map.
    (when-let [workspace (parse-workspace config path)]
      (log-str "A " event-type " event on " path " has been detected.")
      (case event-type
        (:create :modify) (if (get @event-in-progress workspace)
                            (reset-timer workspace)
                            (start-counter config workspace {:action              "add"
                                                             :geoserver-workspace workspace
                                                             :data-dir            path}))
        :delete           (if (get @event-in-progress workspace)
                            (reset-timer workspace)
                            (do (>!! job-queue (assoc request-minimum
                                                      :action              "remove"
                                                      :geoserver-workspace workspace))
                                (start-counter config workspace)))
        nil))))

(defn- handler [config]
  (fn [{:keys [type path]}]
    (let [path-str (.toString ^java.nio.file.Path path)]
      (process-event config type path-str))))

(defn start! [{:keys [file-watcher] :as _config} job-queue]
  (when file-watcher
    (beholder/watch (handler (assoc file-watcher :job-queue job-queue)) (:dir file-watcher))))

(def stop! beholder/stop)
