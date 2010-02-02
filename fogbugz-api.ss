#lang scheme
(require "codes.ss"
         net/url
         srfi/19
         (planet bzlib/xml:1:1)
         (planet lizorkin/sxml:2:1/sxml))

;;; Customization Parameters
;;; ========================

(provide base-url default-email default-password)

(define base-url (make-parameter #f))
(define default-email (make-parameter #f))
(define default-password (make-parameter #f))

;;; API Calls
;;; =========
;;;
;;; all normal calls to the api should go through fb-command
;;; which uses the current auth-token.
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
        (cond [(error-response? response) => error]
              [else response]))))

(define (fb-command name [params '()])
  ;; makes sure you are logged on and handles the 'cmd' and 'token'
  ;; arguments
  (unless (auth-token)
    (if (and (default-email)
             (default-password))
        (logon (default-email)
               (default-password))
        (error "email and/or password not set")))
  
  (fb-command* (list* `[cmd . ,name]
                      `[token . ,(auth-token)]
                      params)))

(define (error-response? response)
  (let ([err ((sxpath "/response/error/text()") response)])
    (if (null? err)
        #f
        (first err))))

;;; Logging On and Off
;;; ==================

(provide logon logoff)

(define auth-token (make-parameter #f))

(define (logon email pw)
  (let* ([response (fb-command* `([cmd . "logon"]
                                  [email . ,email]
                                  [password . ,pw]))]
         [tokens ((sxpath "/response/token/text()") response)])
    (if (null? tokens)
        (error (format "no token in response: ~a" response))
        (auth-token (first tokens)))))

(define (logoff)
  (fb-command "logoff")
  (auth-token #f))


;;; Searching and Listing Cases
;;; ===========================

(provide list-cases search list-filters set-current-filter)

(define (list-cases)
  ((sxpath "/response/cases/case")
   (fb-command "search")))

(define (search text
                #:max     [max #f]
                #:columns [columns #f])
  (let ([response (fb-command "search"
                              `([q . ,text]
                                [max . ,max]
                                [cols . ,(and columns (string-join columns ","))]))])
    ((sxpath "/response/cases/case") response)))

(define (list-filters)
  (map (lambda (n)
         (cons (first ((sxpath "@sfilter/text()") n))
               ((sxpath "text()") n)))
       
       ((sxpath "/response/filters/filter")
        (fb-command "listFilters"))))

(define (set-current-filter id)
  ;; id *must* be an sFilter attribute returned by list-filters
  (fb-command "setCurrentFilter"
              `([sFilter . ,id])))

;;; Time Tracking
;;; =============

(provide start-work stop-work list-intervals new-interval)

(define (start-work case)
  (fb-command "startWork" `([ixBug . ,case])))

(define (stop-work)
  (fb-command "stopWork"))

(define (list-intervals #:person [person #f]
                        #:bug [bug #f]
                        #:start [start #f]
                        #:end [end #f])
  
  (fb-command "listIntervals"
              `([ixPerson . ,person]
                [ixBug . ,bug]
                [dtStart . ,(and start (date->string start))]
                [dtEnd . ,(and end (date->string end))])))

(define (new-interval bug start stop)
  (fb-command "newInterval"
              `([ixBug . ,bug]
                [dtStart . ,(date->string start)]
                [dtEnd . ,(date->string stop)])))

