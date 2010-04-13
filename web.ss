#lang scheme 

(require "dispatch.ss"
         web-server/servlet
         web-server/servlet-env)

(define (serve-app)
  (serve/servlet app-dispatch
                 #:servlet-path "/"
                 #:launch-browser? #f
                 #:port 8080
                 #:listen-ip #f))


