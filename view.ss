#lang scheme
(require "structs.ss"
         "hash-sort.ss"
         web-server/templates)

(provide list-cases)

(define (list-cases cases current-case start-work-url stop-work-url set-estimate-url)
  (define-values (estimated unestimated) 
                 (partition has-estimate? cases))

  (define estimated/grouped (group-by-project estimated))
  (define unestimated/grouped (group-by-project unestimated))

  (define (case-class case)
    (if (and current-case
             (string=? (case-id case)
                       (case-id current-case)))
      "case current"
      "case"))
  (list #"text/html" (include-template "web-template.html")))

(define (group-by-project/hash cases)
  (for/fold ([table #hash()])
            ([case cases])
    (hash-update table
                 (case-project case)
                 (curry cons case)
                 '())))

(define (group-by-project cases)
  (define table (group-by-project/hash cases))
  (define projects (for/list ([(key value) table])
                             (make-project key value)))
  (sort projects string<? #:key project-title))


(define (has-estimate? d)
  (< 0 (case-estimate d)))

(define-struct project (title cases))
