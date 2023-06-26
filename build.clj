(ns build
  (:require [clojure.string          :as str]
            [clojure.tools.build.api :as b]))

(defn get-calendar-commit-version []
  (let [date   (b/git-process {:git-args "show -s --format=%cs HEAD"})
        commit (b/git-process {:git-args "rev-parse --short HEAD"})]
    (-> date
        (str/replace "-" ".")
        (str "-" commit))))

(def build-folder "target")
(def jar-content (str build-folder "/classes"))
(def basis (b/create-basis {:project "deps.edn"}))

(def app-name "geosync")
(def version (get-calendar-commit-version))
(def uberjar-file-name (format "%s/%s-%s.jar" build-folder app-name version))

(defn clean [_]
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn uberjar [_]
  ;; clean up old files
  (clean nil)

  ;; copy resources to jar-content folder
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir jar-content})

  (b/compile-clj {:src-dirs  ["src"]
                  :class-dir jar-content
                  :basis     basis})

  ;; package jar-content into a jar
  (b/uber {:class-dir jar-content
           :uber-file uberjar-file-name
           :basis     basis
           :main      'geosync.cli})

  (println (format "Uberjar file created: \"%s\"" uberjar-file-name)))
