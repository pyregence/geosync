(ns geosync.file-watcher
  (:require [clojure.core.async :refer [<! >! >!! go-loop timeout]]
            [clojure.string     :as s]
            [triangulum.logging :refer [log-str]])
  (:import java.nio.file.Paths
           (io.methvin.watcher DirectoryChangeEvent
                               DirectoryChangeEvent$EventType
                               DirectoryChangeListener
                               DirectoryWatcher)
           io.methvin.watcher.hashing.FileHasher))

;;-----------------------------------------------------------------------------
;; From https://github.com/nextjournal/beholder/blob/main/src/nextjournal/beholder.clj
;;-----------------------------------------------------------------------------

(defn- fn->listener ^DirectoryChangeListener [f]
  (reify
    DirectoryChangeListener
    (onEvent [this e]
      (let [path (.path ^DirectoryChangeEvent e)]
        (condp = (. ^DirectoryChangeEvent e eventType)
          DirectoryChangeEvent$EventType/CREATE   (f {:type :create :path path})
          DirectoryChangeEvent$EventType/MODIFY   (f {:type :modify :path path})
          DirectoryChangeEvent$EventType/DELETE   (f {:type :delete :path path})
          DirectoryChangeEvent$EventType/OVERFLOW (f {:type :overflow :path path}))))))

(defn- to-path [& args]
  (Paths/get ^String (first args) (into-array String (rest args))))

(defn- create
  "Creates a watcher taking a callback function `cb` that will be invoked
  whenever a file in one of the `paths` chages.
  Not meant to be called directly but use `watch` or `watch-blocking` instead."
  [cb paths]
  (-> (DirectoryWatcher/builder)
      (.paths (map to-path paths))
      (.listener (fn->listener cb))
      ;; (.fileHashing false) ;; Uncomment this (and comment out the line below) if the file watcher is slow to initialize.
                              ;; This will turn off file hashing (which is used to prevent duplicate events).
      (.fileHasher FileHasher/LAST_MODIFIED_TIME)
      (.build)))

(defn watch
  "Creates a directory watcher that will invoke the callback function `cb` whenever
  a file event in one of the `paths` occurs. Watching will happen asynchronously.
  Returns a directory watcher that can be passed to `stop` to stop the watch."
  [cb & paths]
  (doto (create cb paths)
    (.watchAsync)))

(defn watch-blocking
  "Blocking version of `watch`."
  [cb & paths]
  (doto (create cb paths)
    (.watch)))

(defn stop
  "Stops the watch for a given `watcher`."
  [^DirectoryWatcher watcher]
  (.close watcher))

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

(defonce watcher (atom nil))

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
      (let [init-workspace       (re-find regex path)
            [forecast timestamp] (s/split init-workspace #"/")
            cleaned-forecast     (s/replace forecast "_" "-")]
        (s/join "_" [cleaned-forecast timestamp])))))

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

(defn- make-handler [config]
  (fn [{:keys [type path]}]
    (let [path-str (.toString ^java.nio.file.Path path)]
      (process-event config type path-str))))

(defn start! [{:keys [file-watcher] :as _config} job-queue]
  (future
    (log-str "Initializing file watcher...")
    (reset! watcher
           (watch (make-handler (assoc file-watcher :job-queue job-queue))
                  (:dir file-watcher)))
    (log-str "File watcher has been initialized.")))

(defn stop! []
  (stop @watcher)
  (reset! watcher nil)
  (log-str "File watcher has been stopped."))
