#lang scheme
(require "structs.ss"
         web-server/templates)

(provide list-cases)

(define (list-cases cases current-case)
  (define-values (estimated unestimated) 
                 (partition has-estimate? cases))
  (define estimated/grouped (group-by-project estimated))
  (define (case-class case)
    (if (and current-case
             (string=? (case-id case)
                       (case-id current-case)))
      "case current"
      "case"))
  (list #"text/html" (include-template "web-template.html")))

(define (group-by-project cases)
  (for/fold ([table #hash()])
            ([case cases])
    (hash-update table
                 (case-project case)
                 (curry cons case)
                 '())))

(define (has-estimate? d)
  (< 0 (case-estimate d)))

(define (start-work-url case)
  (format "/start-work/~a" (case-id case)))

(define stop-work-url "/stop-work")

(define (set-estimate-url id estimate)
  (format "/set-estimate/~a/~a" id estimate))
