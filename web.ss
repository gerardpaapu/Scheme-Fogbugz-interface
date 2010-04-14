#lang scheme 

(require "dispatch.ss"
         web-server/servlet
         web-server/servlet-env)

(provide serve-app)

(define (serve-app)
  (serve/servlet app-dispatch
                 #:servlet-regexp #rx""
                 #:extra-files-paths '("static")
                 #:launch-browser? #f
                 #:port 8000
                 #:listen-ip #f))


