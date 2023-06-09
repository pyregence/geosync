(ns geosync.simple-sockets
  (:import (java.io BufferedReader)
           (java.net ConnectException Socket ServerSocket))
  (:require [clojure.java.io    :as io]
            [clojure.string     :as s]
            [geosync.utils      :refer [nil-on-error]]
            [triangulum.logging :refer [log log-str]]))

;;=================================
;; Client Socket
;;=================================

(defn send-to-server! [host port message]
  (try
    (with-open [socket (Socket. ^String host ^Integer port)]
      (doto (io/writer socket)
        (.write (-> message
                    (s/trim-newline)
                    (str "\n")))
        (.flush))
      (.shutdownOutput socket)
      true)
    (catch ConnectException _
      (log-str "Connection to " host ":" port " failed!"))))

;;=================================
;; Server Socket
;;=================================

(defonce ^:private global-server-thread (atom nil))
(defonce ^:private global-server-socket (atom nil))

(defn- read-socket! [^Socket socket]
  (.readLine ^BufferedReader (io/reader socket)))

(defn- accept-connections! [^ServerSocket server-socket handler]
  (while @global-server-thread
    (when-let [^Socket socket (nil-on-error (.accept server-socket))]
      (try
        (->> (read-socket! socket)
             (handler))
        (catch Exception e
          (log-str "Server error: " e))
        (finally (.close socket))))))

(defn stop-server! []
  (if @global-server-thread
    (do
      (reset! global-server-thread nil)
      (when @global-server-socket
        (.close ^ServerSocket @global-server-socket)
        (reset! global-server-socket nil))
      (log "Server stopped."))
    (log "Server is not running.")))

(defn start-server! [port handler]
  (if @global-server-thread
    (log "Server is already running.")
    (reset! global-server-thread
            (future
              (try
                (with-open [server-socket (ServerSocket. port)]
                  (reset! global-server-socket server-socket)
                  (accept-connections! server-socket handler))
                (catch Exception e
                  (log-str "Error creating server socket: " e)
                  (stop-server!)))))))
