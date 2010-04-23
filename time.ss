#lang scheme 
(require  srfi/19)

(provide (all-defined-out))

(define (current-date*)
  (current-date 0))

(define (minutes n)
  (make-time time-duration 0 (* n 60)))

(define (quick-interval n)
  (let ([now (current-date*)])
    (values (subtract-duration/date now (minutes (+ 1 n)))
            (subtract-duration/date now (minutes 1)))))

(define (subtract-duration/date date duration)
  (let* ([time* (date->time-utc date)]
         [total (subtract-duration time* duration)])
    (time-utc->date total 0)))

(define (time-stamp t)
  (date->string t "~4"))
