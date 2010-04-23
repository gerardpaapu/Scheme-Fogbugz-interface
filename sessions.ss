#lang scheme 
(require "structs.ss"
         (prefix-in fb: "fogbugz-api.ss")
         web-server/servlet
         web-server/servlet/web
         web-server/formlets
         web-server/http/cookie
         web-server/http/cookie-parse)

(provide (all-defined-out))

(define (login [error #f])
  ;; send the login form
  (match-define
    (list email password)
    (send/suspend/dispatch
      (lambda (embed-url)
        (if error
          `(p ([class "error"])
              ,(exn-message error))
          '())
        (embed-formlet/post embed-url login-form))))

  (with-handlers ([exn:not-logged-on? login]) 
      (cond [(fb:logon email password)
             => (lambda (key)
                  (redirect-to "/"
                    #:headers (list (cookie->header (make-cookie "fbtoken" key)))))]
            [else (login)]) ))

(define (logout req)
  (redirect-to "/" #:headers (list (cookie->header (make-cookie "fbtoken" "")))))

(define login-form
  (let ([input-password (to-string (required (password-input)))]
        [submit '(input ([type "submit"]))])
    (formlet
      (div (div ([class "field"])
                "email:" ,{input-string . => . email})
           (div ([class "field"])
                "password: " ,{input-password . => . password})
           ,submit)
      (list email password))))

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
         [("1" "2" "3") #t]
         [else false])))

(define-syntax-rule (define-session-page (name token . args) body ...)
  ;; get the session-key or redirect to the login page
  (define (name req . args)
    (with-handlers ([exn:not-logged-on? login]
                    [exn:fogbugz-error? display-error])
       (cond [(get-session req)
              => (lambda (token)
                   body ... )]
             [else (login)]))))

