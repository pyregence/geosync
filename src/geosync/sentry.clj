(ns geosync.sentry
  (:require [geosync.utils :refer [nil-on-error]])
  (:import [io.sentry IScope ScopeCallback Sentry Sentry$OptionsConfiguration
            SentryLevel SentryOptions]
           [java.util List]))

(defonce ^:private initialized? (atom false))

(defn- scope-callback ^ScopeCallback [tags ^List fingerprint]
  (reify ScopeCallback
    (run [_ scope]
      (let [^IScope s scope]
        (when fingerprint (.setFingerprint s fingerprint))
        (doseq [[k v] tags :when (some? v)]
          (.setTag s (name k) (str v)))))))

(defn init!
  "Initializes Sentry once per JVM when configured via :sentry or SENTRY_DSN."
  [config-params]
  (when (compare-and-set! initialized? false true)
    (let [cfg      (:sentry config-params)
          dsn      (or (not-empty (System/getenv "SENTRY_DSN")) (not-empty (:dsn cfg)))
          env      (or (not-empty (System/getenv "SENTRY_ENVIRONMENT")) (not-empty (:environment cfg)) "development")
          instance (or (not-empty (System/getenv "GEOSYNC_INSTANCE")) (not-empty (:instance cfg)))]
      (when-not (and dsn
                     (nil-on-error
                      (Sentry/init
                       (reify Sentry$OptionsConfiguration
                         (configure [_ options]
                           (let [^SentryOptions opts options]
                             (.setDsn opts ^String dsn)
                             (.setEnvironment opts ^String env)
                             ;; Keep OS hostname in server_name so multi-VM prod hosts stay distinguishable.
                             (when instance (.setTag opts "geosync.instance" ^String instance))
                             ;; Never send user emails, IP addresses, or cookies to Sentry.
                             (.setSendDefaultPii opts false)
                             (doto (.getDataCollection opts)
                               (.forceDataCollection)
                               (.setUserInfo Boolean/FALSE))
                             (.addInAppInclude opts "geosync")
                             (.addInAppInclude opts "triangulum")))))
                      true))
        (reset! initialized? false)))))

(defn close!
  "Flushes pending events and closes the Sentry SDK on CLI exit."
  []
  (nil-on-error
   (when (Sentry/isEnabled)
     (Sentry/close)
     (reset! initialized? false))))

(defn report-exception!
  "Reports throwable to Sentry when enabled; always returns throwable unchanged."
  [^Throwable throwable tags]
  (nil-on-error
   (when (Sentry/isEnabled)
     (Sentry/captureException throwable (scope-callback tags nil))))
  throwable)

(defn report-failure!
  "Reports a non-exception GeoSync failure when Sentry is enabled."
  [failure-key ^String message tags]
  (nil-on-error
   (when (Sentry/isEnabled)
     (Sentry/captureMessage message
                            SentryLevel/ERROR
                            (scope-callback (assoc tags :geosync.failure (name failure-key))
                                            ["geosync" (name failure-key)])))))
