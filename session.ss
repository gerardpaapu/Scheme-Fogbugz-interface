#lang scheme 
(require "fb-structs.ss")
(provide (all-defined-out))

(define (login)
  ;; send the login form
  (match-define (list email password)
                (send/suspend/dispatch
                  (lambda (embed-url)
                    (embed-formlet/post embed-url login-formlet))))

  (cond [(fb:logon email password)
         => (lambda (key)
              (redirect-to
                (app-url list-cases)
                #:headers (list (cookie->header (make-cookie "fbtoken" key)))))]
        [else (redirect-to (app-url login))]))

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
  (and cookie (client-cookie-value cookie)))

(define (exn:fb-not-logged-on? exn)
  (and (exn:fogbugz-exception? exn)
       (exn:fogbugz-exception-code "3")))

(define-syntax define-session-page
  ;; get the session-key or redirect to the login page
  (syntax-rules ()
    [(define-session-page (name token . args) body ...)
     (define (name req . args)
       (with-handlers ([exn:fb-not-logged-on? (lambda (err)
                                                (login))])
         (cond [(get-session req)
              => (lambda (token)
                   body
                   ...)]
             [else (login)]))) 
     ]))
