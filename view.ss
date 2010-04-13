#lang scheme
(require "structs.ss")

(provide list-cases)

(define (list-cases cases current-id)
  (define-values (estimated unestimated)
        (split case-estimate cases))

  `(div ,@(map (lambda (case)
                 `(a ([href ,(string-append "/start-work/" (case-id case))]) 
                     ,(case-id case)
                     ,(case-title case)))
               estimated)))

(define (group-by-project cases)
  (for/fold ([table #hash()])
            ([case cases])
    (hash-update table
                 (case-project case)
                 (lambda (ls)
                   (cons case ls))
                 '())))


