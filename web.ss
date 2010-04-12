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

(define (case-id case)
  (dict-ref case "ixbug"))

(define (case-project case)
  (dict-ref case "sproject"))

(define (case-title case)
  (dict-ref case "stitle"))

(define (group-cases cases)
  (for/fold ([table #hash()])
            ([case* cases])
            (hash-update table
                         (case-project case*)
                         (curry cons case*)
                         '())))


(define (web/display-cases current)
  (let ([cases (list-cases '("ixBug" "sTitle" "hrsCurrEst" "sProject"))])
    (define-values (estimated unestimated) (partition has-estimate? cases))
    (define current-case (if current 
                           (findf (lambda (c)
                                    (string=? (case-id c) current))
                                  cases)
                           #f))

    (define estimated/grouped (group-cases estimated))

    (define (case-class case)
      (if (equal? (case-id case)
                  current)
        "case current"
        "case"))

    (send/suspend/dispatch
      (lambda (embed-url)
        (define (start-work-url case)
          (let ([id (case-id case)])
            (embed-url (lambda (req)
                         (start-work id)
                         (web/display-cases id)))))

        (define stop-work-url
          (embed-url (lambda (req)
                       (stop-work)
                       (web/display-cases #f))))

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
                    (embed-formlet/post embed-url login-formlet))))
  (logon email password)
  (web/display-cases (working-on)))

(define submit '(input [(type "submit")]))
(define login-formlet
  (let ([input-password (to-string (required (password-input)))])
    (formlet
      (div
        "email:" ,{input-string . => . email}
        "password: " ,{input-password . => . password}
        ,submit)
      (list email password))))

(define (embed-formlet/post embed/url f)
  `(form ([action ,(embed/url
                     (lambda (r)
                       (formlet-process f r)))]
          [method "post"])
         ,@(formlet-display f)))


(define (serve-web-interface)
  (serve/servlet start
                 #:servlet-path "/"
                 #:extra-files-paths (list "static")
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port 8000))
