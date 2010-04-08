#lang scheme
(require  web-server/servlet
          web-server/servlet-env
          web-server/servlet/web
          web-server/templates
          web-server/formlets
          (planet bzlib/xml:1:1)
          (planet lizorkin/sxml:2:1/sxml)
          "fogbugz-api.ss")

(default-email "gerard@phosphor.co.nz")
(default-password "poasqw")
(base-url "http://fogbugz.phosphor.co.nz/api.asp")

(define (has-estimate? d)
  (< 0 (string->number (dict-ref d "hrscurrest" 0))))

(define (web/display-cases current)
  (let ([cases (list-cases '("ixBug" "sTitle" "hrsCurrEst"))])
    (define-values (estimated unestimated) (partition has-estimate? cases))
    
    (send/suspend/dispatch
     (lambda (embed-url)
       (define (case-id case)
         (dict-ref case "ixbug"))
       
       (define (case-class case)
         (if (equal? (case-id case)
                     current)
             "case current"
             "case"))
       
       (define (start-work-url case)
         (let ([id (case-id case)])
           (embed-url (lambda (req)
                        (start-work id)
                        (web/display-cases id)))))

       (define (case-title case)
         (dict-ref case "stitle"))
       
       (define (set-estimate-link case time)
         (let ([id (dict-ref case "ixbug")])
           (format "<a href=\"~a\">~a</a>"
                   (embed-url (lambda (req)
                                (set-estimate id time)
                                (web/display-cases current)))
                   time)))

       (define (estimate-links case)
         (map (curry set-estimate-link case)
              '(0.5 1 2 3 6 12)))
       
       (list #"text/html"
             (include-template "web-template.html"))))))

(define (start req)
  (match-define (list email password)
                (send/suspend/dispatch
                 (lambda (embed-url)
                   (embed-formlet embed-url login-formlet))))
  (logon email password)
  (web/display-cases #f))

(define submit '(input [(type "submit")]))
(define login-formlet
  (formlet
   (div
    "email:" ,{input-string . => . email}
    "password: " ,{input-string . => . password}
    ,submit)
   (list email password)))


(define (serve-web-interface)
  (serve/servlet start
                 #:servlet-path "/"
                 #:extra-files-paths (list "static")
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port 8000))
