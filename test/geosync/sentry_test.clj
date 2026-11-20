(ns geosync.sentry-test
  (:require [clojure.spec.alpha :as spec]
            [clojure.test       :refer [deftest is]]
            [geosync.cli        :as cli]
            [geosync.sentry     :as sentry]))

(deftest report-exception-returns-original-throwable
  (let [err (ex-info "sync boom" {})]
    (is (identical? err (sentry/report-exception! err {:geosync.action "add"})))))

(deftest geosync-config-spec-accepts-sentry
  (is (true? (spec/valid? ::cli/geosync-config-file
                          {:geoserver-rest-uri "http://localhost:8080/geoserver/rest"
                           :geoserver-username "admin"
                           :geoserver-password "geoserver"
                           :sentry             {:dsn         "https://key@sentry.example/1"
                                                :environment "production"
                                                :instance    "shasta"}}))))
