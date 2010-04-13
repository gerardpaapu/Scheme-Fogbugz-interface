#lang scheme
(require "structs.ss"
         net/url
         srfi/19
         (planet bzlib/xml:1:1)
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
        (cond [(error-response? response) => raise]
              [else response]))))

(define (fb-command token name [params '()])
  (fb-command* (list* `[cmd . ,name]
                      `[token . ,token]
                      params)))

(define (error-response? response)
  (let ([err ((sxpath "/response/error") response)])
    (and (not (null? err))
         (make-exn:fogbugz-error (first ((sxpath "@code") err))
                                 (first ((sxpath "text()") err))))))

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

(define (list-cases token)
  (define columns '("ixBug" "sTitle" "hrsCurrEst" "sProject"))
  (set-current-filter token my-cases)
  (map case-xml->dict
       ((sxpath "/response/cases/case")
        (fb-command token "search" `([cols . ,(string-join columns ",")])))))

(define (search token text
                #:max     [max #f]
                #:columns [columns #f])
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

(provide start-work stop-work working-on list-intervals new-interval set-estimate)

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
                [dtEnd . ,(and end (date->string end))])))

(define (new-interval token bug start stop)
  (fb-command token "newInterval"
              `([ixBug . ,bug]
                [dtStart . ,(date->string start)]
                [dtEnd . ,(date->string stop)])))

(define (set-estimate token bug n)
  (fb-command token "edit"
              `([ixBug . ,bug]
                [hrsCurrEst . ,(number->string n)])))

(define (working-on token)
    (let ([current ((sxpath "//interval [dtend = '']")
                    (list-intervals token))])
      (and (not (null? current))
           (case-xml->dict current))))
