#lang scheme
(require web-server/dispatch ; dispatch-rules
         web-server/http/redirect ; 
         "sessions.ss"
         "structs.ss"
         (prefix-in view: "view.ss")
         (prefix-in fb: "fogbugz-api.ss"))

(provide (all-defined-out))

(define-values (app-dispatch app-url)
    (dispatch-rules 
      [("") list-cases]
      [("start-work" (string-arg)) start-work]
      [("stop-work") stop-work]
      [else list-cases]))

(define-session-page (list-cases key)
  (define cases (fb:list-cases key))
  (define current-case (fb:working-on key))
  (view:list-cases cases (case-id current-case)))

(define-session-page (start-work key id)
  (fb:start-work key id)
  (redirect-to (app-url list-cases)))

(define-session-page (stop-work key)
  (fb:stop-work key)
  (redirect-to (app-url list-cases)))
