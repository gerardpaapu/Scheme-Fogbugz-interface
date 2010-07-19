#lang scheme
(require "structs.ss"
         (prefix-in time: "time.ss")
         net/url
         srfi/19
         ;(planet bzlib/xml:1:1)
         (only-in "compatibility.ss" read-sxml)
         (planet lizorkin/sxml:2:1/sxml))

;;; Customization Parameters
;;; ========================

(provide base-url)
(define base-url (make-parameter "http://fogbugz.phosphor.co.nz/api.asp"))

;;; API Calls
;;; =========
;;;
;;; all normal calls to the api should go through fb-command
;;; which uses the requires an auth-token.
;;;
;;; fb-command* is a little more raw, and isn't logon aware at all

(define (fb-command* params)
  (if (not (base-url))
      (error "base-url not set")
      (let* ([params* (remove (lambda (p)
                                (eq? (cdr p) #f))
                              params)]
             [response (call/input-url (struct-copy url (string->url (base-url))
                                                    [query params*])
                                       get-pure-port
                                       read-sxml)])
        (cond [(error-response? response) => (lambda (err)
                                               (raise err #t))]
              [else response]))))

(define (fb-command token name [params '()])
  (fb-command* (list* `[cmd . ,name]
                      `[token . ,token]
                      params)))

(define (error-response? response)
  (let ([err ((sxpath "/response/error") response)])
    (and (not (null? err))
         (make-exn:fogbugz-error (first ((sxpath "text()") err))
                                 (current-continuation-marks)
                                 (string->number (first ((sxpath "@code/text()") err)))))))

;;; Logging On and Off
;;; ==================

(provide logon logoff)

(define (logon email pw)
  (let* ([response (fb-command* `([cmd . "logon"]
                                  [email . ,email]
                                  [password . ,pw]))]
         [tokens ((sxpath "/response/token/text()") response)])
    (and (not (null? tokens)) 
         (first tokens))))

(define (logoff token)
  (fb-command token "logoff"))

;;; Searching and Listing Cases
;;; ===========================

(provide list-cases search list-filters set-current-filter)

(define default-columns '("ixBug" "sTitle" "hrsCurrEst" "sProject" "ixStatus"))

(define (list-cases token)
  (set-current-filter token my-cases)
  (map case-xml->dict
       ((sxpath "/response/cases/case")
        (fb-command token "search" `([cols . ,(string-join default-columns ",")])))))

(define (search token text
                #:max     [max #f]
                #:columns [columns default-columns])
  (let ([response (fb-command token "search"
                              `([q . ,text]
                                [max . ,max]
                                [cols . ,(and columns (string-join columns ","))]))])
    (map case-xml->dict
         ((sxpath "/response/cases/case") response))))

(define (list-filters token)
  (map (lambda (n)
         (cons (first ((sxpath "@sfilter/text()") n))
               ((sxpath "text()") n)))
       
       ((sxpath "/response/filters/filter")
        (fb-command token "listFilters"))))

(define (set-current-filter token id)
  ;; id *must* be an sFilter attribute returned by list-filters
  (fb-command token
              "setCurrentFilter"
              `([sFilter . ,id])))

(define my-cases "ez")

;;; Time Tracking
;;; =============

(provide start-work stop-work working-on list-intervals new-interval set-estimate resolve-bug close-bug quick-interval)

(define (start-work token case)
  (fb-command token "startWork" `([ixBug . ,case])))

(define (stop-work token)
  (fb-command token "stopWork"))

(define (list-intervals token
                        #:person [person #f]
                        #:bug [bug #f]
                        #:start [start #f]
                        #:end [end #f])
  (fb-command token "listIntervals"
              `([ixPerson . ,person]
                [ixBug . ,bug]
                [dtStart . ,(and start (date->string start))]
                [dtEnd . ,(and end (date->string end))]
                [cols . ,(string-join default-columns ",")])))

(define (new-interval token bug start stop)
  (fb-command token "newInterval"
              `([ixBug . ,bug]
                [dtStart . ,(time:time-stamp start)]
                [dtEnd . ,(time:time-stamp stop)])))

(define (set-estimate token bug n)
  (fb-command token "edit"
              `([ixBug . ,bug]
                [hrsCurrEst . ,(number->string n)])))

(define (close-bug token bug)
  (fb-command token "close" `([ixBug . ,bug])))

(define (resolve-bug token bug)
  (fb-command token "resolve"
              `([ixBug . ,bug]
                [ixStatus . "2"])))

(define (quick-interval token bug minutes)
  (define-values (start end)
    (time:quick-interval minutes))
  (new-interval token bug start end))

(define (working-on token)
    (let ([current ((sxpath "//interval [dtend = '']")
                    (list-intervals token))])
      (and (not (null? current))
           (first (search token (case-id (case-xml->dict current)))))))
