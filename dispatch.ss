#lang scheme
(require web-server/dispatch
         web-server/http/redirect
         "sessions.ss"
         "structs.ss"
         (prefix-in view: "view.ss")
         (prefix-in fb: "fogbugz-api.ss"))

(provide (all-defined-out))

(define-syntax define-action
  ;; defines a simple action that simply executes a method
  ;; redirects to the home page
  (syntax-rules ()
    [(_ controller method)
     (define-session-page (controller key . args)
        (apply method key args)
        (redirect-to (app-url list-cases)))]))

(define-values (app-dispatch app-url)
    (dispatch-rules 
      [("") list-cases]
      [("start-work" (string-arg)) start-work]
      [("stop-work") stop-work]
      [("logout") logout] ; provided from sessions.ss
      [("set-estimate" (string-arg) (number-arg)) set-estimate]))

(define-session-page (list-cases key)
  (define cases (fb:list-cases key))
  (define current-case (fb:working-on key))
  (apply view:list-cases cases current-case 
         (map (lambda (n) (curry app-url n))
              (list start-work stop-work set-estimate))))

(define-action start-work fb:start-work)
(define-action stop-work fb:stop-work)
(define-action set-estimate fb:set-estimate)
