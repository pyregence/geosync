(ns geosync.file-watcher
  (:require [clojure.core.async :refer [<! >! >!! go-loop timeout]]
            [nextjournal.beholder :as beholder]
            [triangulum.logging :refer [log-str]]
            [clojure.string :as s]))

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defonce event-in-progress (atom {}))

(def request-minimum
  {:response-host "some.host"
   :response-port 1234})

(defn count-down [{:keys [job-queue]} workspace request-args]
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
   (swap! event-in-progress assoc workspace (atom 10))
   (count-down config workspace request-args)))

;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

;;FIXME some files are created and then deleted during the registration process.
;;The deletion triggers the deregistration of the workspace. These are some
;;known files that should be ignored.
(def files-to-ignore #{"sample_image.dat" ".properties"})

(defn parse-workspace
  [{:keys [dir workspace-regex]} path]
  (let [folder-regex (re-pattern (format "(?<=%s/)[\\w-]+" dir))
        folder-name  (re-find folder-regex path)]
    (when-let [regex (get workspace-regex folder-name)]
      (some-> (re-find regex path)
              (s/replace "/" "_")
              (s/replace "_dev" ""))))) ;TODO remove when dev folders no longer needed

(defn process-event
  [{:keys [job-queue] :as config} event-type path]
  (when (not (some #(s/includes? path %) files-to-ignore))
    (when-let [workspace (parse-workspace config path)]
      (log-str event-type ":" path)
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

(defn- handler [{:keys [dev] :as config}]
  (fn [{:keys [type path]}]
    (let [path-str (.toString path)]
      (cond ;TODO remove when dev folders are no longer needed
        (and dev (s/includes? path-str "_dev"))
        (process-event config type path-str)

        (and (not dev) (not (s/includes? path-str "_dev")))
        (process-event config type path-str)

        :else nil))))

(defn start! [{:keys [file-watcher]} job-queue]
  (when file-watcher
    (beholder/watch (handler (assoc file-watcher :job-queue job-queue)) (:dir file-watcher))))
