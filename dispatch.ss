#lang scheme
(require web-server/dispatch ; dispatch-rules
         web-server/http/redirect ; 
         "sessions.ss"
         (prefix-in view: "view.ss")
         (prefix-in fb: "fogbugz-api.ss"))

(define-values (app-dispatch app-url)
    (dispatch-rules 
      [("") list-cases]
      [("start-work" (string-arg)) start-work]
      [("stop-work") stop-work]
      [else list-cases]))

(define (list-cases req)
  (with-session (req key)
    (define cases (fb:list-cases key))
    (define current-case (fb:working-on key))
    (view:list-cases cases (case-id current-case))))

(define (start-work req id)
  (with-session (req key)
    (fb:start-work key id)
    (redirect-to (app-url list-cases))))

(define (stop-work req)
  (with-session (req key)
    (fb:stop-work key)
    (redirect-to (app-url list-cases))))

