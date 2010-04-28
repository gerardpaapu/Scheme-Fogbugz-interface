#lang scheme 
(require "structs.ss"
         (prefix-in fb: "fogbugz-api.ss")
         web-server/servlet
         web-server/servlet/web
         web-server/formlets
         web-server/http/cookie
         web-server/http/cookie-parse)

(provide (all-defined-out))

(define (login [errors #f])
  (define-values (email password) (send-login-form errors))
  (with-handlers ([exn:not-logged-on? login]) 
      (cond [(fb:logon email password)
             => (lambda (key)
                  (redirect-to "/" #:headers (list (token-header key))))]
            [else (login)]) ))

(define (display-error error)
  `(p ([class "error"])
      ,(exn-message error)))

(define (logout req)
  (redirect-to "/" #:headers (list (token-header ""))))

(define login-form
  (let ([input-password (to-string (required (password-input)))]
        [submit `(input ([type "submit"]
                         [value "login"]))])
    (formlet
      (div (div ([class "field"]) "email:" ,{input-string . => . email})
           (div ([class "field"]) "password:" ,{input-password . => . password})
           ,submit)
      (list email password))))

(define (send-login-form errors)
  (match-define (list email password)
    (send/suspend/dispatch
      (lambda (embed-url)
        `(div ,(if errors
                `(p ([class "error"])
                    ,(exn-message errors))
                '())
              ,(embed-formlet/post embed-url login-form)))))
  (display errors)
  (values email password))

(define (token-header value)
  (cookie->header (make-cookie "fbtoken" value)))

(define (embed-formlet/post embed/url f)
  `(form ([action ,(embed/url
                     (lambda (r)
                       (formlet-process f r)))]
          [method "post"])
         ,@(formlet-display f)))

;; request -> (or string? #f)
(define (get-session req)
  (define cookie (findf (lambda (c)
                          (string=? "fbtoken" (client-cookie-name c)))
                        (request-cookies req)))
  (and cookie
       (not (string=? "" (client-cookie-value cookie)))
       (client-cookie-value cookie)))

(define (exn:not-logged-on? exn)
  (and (exn:fogbugz-error? exn)
       (case (exn:fogbugz-error-code exn)
         [(1 2 3) #t]
         [else #f])))

(define-syntax-rule (define-session-page (name token req . args) body ...)
  ;; get the session-key or redirect to the login page
  (define (name req . args)
    (with-handlers ([exn:not-logged-on? login]
                    [exn:fogbugz-error? display-error])
       (cond [(get-session req)
              => (lambda (token)
                   body ... )]
             [else (login)]))))

