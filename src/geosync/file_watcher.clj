(ns geosync.file-watcher
  (:require [clojure.core.async :refer [<! >! >!! go-loop timeout]]
            [clojure.java.io    :as io]
            [clojure.string     :as s]
            [triangulum.logging :refer [log-str]])
  (:import java.io.File
           (java.nio.file Path
                          Files
                          FileSystems
                          LinkOption
                          StandardWatchEventKinds
                          WatchEvent
                          WatchKey
                          WatchService)))

;;-----------------------------------------------------------------------------
;; Custom file watcher adapted from https://docs.oracle.com/javase/tutorial/essential/io/examples/WatchDir.java
;; and https://docs.oracle.com/javase/tutorial/essential/io/notification.html
;;-----------------------------------------------------------------------------

(defn create-watcher
  "Creates a new WatchService object."
  []
  (.newWatchService (FileSystems/getDefault)))

(defn register-directories
  "Registers the given directory and all of its sub-directories with the provided
   WatchService object. Returns a hash map of watch keys to paths in order
   to keep track of the paths that the watcher is watching."
  [^WatchService watcher root-dir]
  (let [sub-dirs    (->> (io/file root-dir)
                         (file-seq)
                         (filter #(.isDirectory ^File %))
                         (map #(.toPath ^File %)))
        event-kinds (into-array [StandardWatchEventKinds/ENTRY_CREATE
                                 StandardWatchEventKinds/ENTRY_DELETE
                                 StandardWatchEventKinds/ENTRY_MODIFY])]
    (reduce (fn [acc ^Path dir]
              (let [watch-key (.register dir watcher event-kinds)]
                (assoc acc watch-key dir)))
            {}
            sub-dirs)))

(def nofollow-links (into-array [LinkOption/NOFOLLOW_LINKS]))

(defn directory? [path]
  (Files/isDirectory path nofollow-links))

(defn process-events!
  "Processes all events for keys queued to the watcher. The handler function
   will be invoked whenever a file in one the paths added to watch-keys by
   register-directories is created, deleted, or modified.
   Note that this must occur in a child thread."
  [handler ^WatchService watcher watch-keys]
  (when (seq watch-keys)
    (let [^WatchKey watch-key (.take watcher)
          ^Path     watch-dir (get watch-keys watch-key)]
      (recur handler
             watcher
             (try (reduce (fn [_ ^WatchEvent event]
                            (let [kind               (.kind event)
                                  path               (->> event
                                                          ^Path (.context)
                                                          (.resolve watch-dir))
                                  updated-watch-keys (cond
                                                       (and (= kind StandardWatchEventKinds/ENTRY_CREATE)
                                                            (directory? path))
                                                       (merge watch-keys (register-directories watcher path))

                                                       (and (= kind StandardWatchEventKinds/ENTRY_DELETE)
                                                            (directory? path))
                                                       (dissoc watch-keys watch-key)

                                                       :else
                                                       watch-keys)]
                              (handler {:path path
                                        :type (condp = kind
                                                StandardWatchEventKinds/ENTRY_CREATE :create
                                                StandardWatchEventKinds/ENTRY_MODIFY :modify
                                                StandardWatchEventKinds/ENTRY_DELETE :delete
                                                StandardWatchEventKinds/OVERFLOW     :overflow
                                                nil)})
                              (if (.reset watch-key)
                                updated-watch-keys
                                (dissoc updated-watch-keys watch-key))))
                          watch-keys
                          (.pollEvents watch-key))
               (catch Exception e
                 (log-str "Exception: " e)))))))

(defn run-file-watcher!
  "The entry point for the file watcher. Takes a handler function and a dir.
   The handler function is invoked on any create, modify, or delete event
   on any one of the sub-dirs to the provided dir. The handler function should
   take one argument, a map of {:path path :type type} where the path is the path
   of the file in question and the type is the kind of event observed."
  [handler dir]
  (try
    (let [_            (log-str "Initializing file watcher...")
          watcher      (create-watcher)
          watch-keys   (register-directories watcher dir)
          _            (log-str "Done registering directories for " dir)
          watch-thread (future (process-events! handler watcher watch-keys))
          _            (log-str "File watcher has been initialized.")]
      watch-thread)
    (catch Exception e
      (log-str "Exception: " e))))

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
    (when-let [init-workspace (some-> folder-name
                                      (folder-name->regex)
                                      (re-find path))]
      (let [[forecast & sub-dirs] (s/split init-workspace #"/")
            cleaned-forecast      (s/replace forecast "_" "-")]
        {:data-dir  (.getPath (io/file dir init-workspace))
         :workspace (s/join "_" (cons cleaned-forecast sub-dirs))}))))

(defn- process-event
  [{:keys [job-queue] :as config} event-type path]
  (try
    (when (not-any? #(s/includes? path %) files-to-ignore)
      ;; If no workspace is found then no action will be taken.
      ;; Any folder that you wish an action to be taken for **must**
      ;; be included as a key in the folder-name->regex config map.
      (when-let [{:keys [workspace data-dir]} (parse-workspace config path)]
        (log-str "A " event-type " event on " path " has been detected.")
        (case event-type
          (:create :modify) (if (get @event-in-progress workspace)
                              (reset-timer workspace)
                              (start-counter config workspace {:action              "add"
                                                               :geoserver-workspace workspace
                                                               :data-dir            data-dir}))
          :delete           (if (get @event-in-progress workspace)
                              (reset-timer workspace)
                              (do (>!! job-queue (assoc request-minimum
                                                        :action              "remove"
                                                        :geoserver-workspace workspace))
                                  (start-counter config workspace)))
          nil)))
    (catch Exception e
      (log-str "Exception: " e))))

(defn- make-handler [config]
  (fn [{:keys [type path]}]
    (let [path-str (.toString ^java.nio.file.Path path)]
      (process-event config type path-str))))

(defn start! [{:keys [file-watcher] :as _config} job-queue]
  (let [handler-fn (make-handler (assoc file-watcher :job-queue job-queue))]
    (reset! watcher (run-file-watcher! handler-fn (:dir file-watcher)))))

(defn stop! []
  (future-cancel @watcher)
  (reset! watcher nil)
  (log-str "File watcher has been stopped."))
