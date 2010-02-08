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

;;; Editing Cases
;;; =============
;; Each of the following commands accept "ixPersonEditedBy" and "dt" parameters from admins to enable accurate imports with the API.

;; cmd=new and cmd=edit and cmd=assign and cmd=reactivate and cmd=reopen

;; Arguments:

;; ixBug (omitted for cmd=new)
;; ixBugParent - Make this case a subcase of another case
;; ixBugEvent (omitted for cmd=new - optional - if supplied, and this is not equal to the latest bug event for the case, you will receive error code 9 back to show that you were working with a "stale" view of the case).
;; sTags - A comma-separated list of tags to include in the case
;; sTitle
;; ixProject (or sProject)
;; ixArea (or sArea)
;; ixFixFor (or sFixFor - searches project first, then global fixfors)
;; ixCategory (or sCategory)
;; ixPersonAssignedTo (or sPersonAssignedTo)
;; ixPriority (or sPriority)
;; dtDue
;; hrsCurrEst
;; hrsElapsedExtra This sets additional non-timesheet time on a case. (i.e. if there was an hour long time interval for the case and you set hrsElapsedExtra to 2, then the total hrsElapsed would be 3)
;; sVersion
;; sComputer
;; sCustomerEmail - only the API lets you set this
;; ixMailbox - if you set sCustomerEmail, you'll want to set this too... otherwise you won't be able to reply to this case
;; sScoutDescription - (used only for cmd=new) if you set this, and FogBugz finds a case with this sScoutDescription, it will append to that case unless fScoutStopReporting is true for that case, and then it will do nothing.
;; sScoutMessage - the message you are supposed to display to users for this case
;; fScoutStopReporting - set this to 1 if you don't want FogBugz to record any more of these types of cases
;; sEvent - text description of the bugevent
;; cols - the columns you want returned about this case
;; If any fields are omitted, they will not be changed.

(define (edit-parameters text
                         #:bug-id
                         #:parent-id [parent-id #f]
                         #:event-id 
                         #:tags [tags #f]
                         #:title title
                         #:project project
                         #:area [area #f]
                         #:milestone milestone
                         #:category category
                         #:person-assigned-to assigned-to
                         #:priority priority
                         #:estimate estimate
                         #:cols cols)
  
  (define (id-or-string v name)
    (cond [(number? v) `([,(string->symbol (string-append "ix" name)) . ,v])]
          [(string? v) `([,(string->symbol (string-append "s" name)) . ,v])]
          [else '()]))
  
  (define (comma-separated v name)
    (if (and (list? v) (not (null? v)))
          `[,(string->symbol name) . ,(string-join v ",")]
          '()))
  
  `([izBug . ,bug-id]
    [ixParent . ,parent-id]
    [ixBugEvent . ,event-id]
    [sTitle . ,title]
    
    ,@(comma-separated tags "sTags")
    ,@(comma-separated cols "cols")
    ,@(id-or-string project "Project")
    ,@(id-or-string area "Area")
    ,@(id-or-string fix-for "FixFor")
    ,@(id-or-string category "Category")
    ,@(id-or-string person-assigned-to "PersonAssignedTo")
    ,@(id-or-string priority "Priority")))



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


