(use-modules ((gnu packages) #:select (specifications->manifest)))

(specifications->manifest
 (list "clojure-tools@1.11" ;; to build jar and run clojure.
       "openjdk@21:jdk"     ;; to build jar and run clojure.
       "git"                ;; to build jar.
       "nss-certs"          ;; to build jar it needs certs.
       "coreutils"          ;; to build the jar it needs mkdir.
       ))
