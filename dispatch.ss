#lang scheme
(require web-server/dispatch ; dispatch-rules
         web-server/http/redirect ; 

         ;; serve/servlet and friends
         web-server/servlet 
         web-server/servlet-env

         web-server/servlet/web
         web-server/formlets
         (prefix-in fb: "fogbugz-api.ss"))

(define-values (app-dispatch app-url)
    (dispatch-rules 
      [("") list-cases]
      [("start-work" (string-arg)) start-work]
      [("stop-work") stop-work]
      [("login") login]
      [("process-login") process-login]
      [else list-cases]))

(define (list-cases req)
  (with-session (req => key)
    (define cases (fb:list-cases key))
    (define current-case (fb:get-current-case key))
    ;; some sort of view I guess ...
    ))

(define (start-work req id)
  (with-session (req => key)
    (fb:start-work key id)
    (redirect-to (app-url list-cases))))

(define (stop-work req)
  (with-session (req => key)
    (fb:stop-work key)
    (redirect-to (app-url list-cases))))

(define (login req)
  ;; send the login form
  )

(define (process-login req)
  ;; process the login form
  ;; set the session-key cookie
  )

(define login-form
  ;; the form for logging in
  )

;; request -> (or string? #f)
(define (get-session req)
  )

(define-syntax with-session
  ;; get the session-key or redirect to the login page
  (syntax-rules (=>)
    [(with-session (req => var) body ...)
     (cond [(get-session req) => 
            (lambda (key)
              body
              ...)]
           [else (redirect-to (app-url login))])]))
