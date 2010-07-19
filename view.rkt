#lang racket
(require "structs.rkt"
         web-server/http/response-structs
         web-server/templates)

(provide list-cases)

(define (list-cases cases current-case bare-html?
                    start-work-url stop-work-url set-estimate-url
                    close-bug-url resolve-bug-url quick-interval-url)
  
  (define-values (estimated unestimated) (estimated/unestimated cases))
    
  (define (case-class case)
    (cond [(and current-case
             (string=? (case-id case)
                       (case-id current-case))) "case current"]      
          [(case-resolved? case) "case resolved"]
          [else "case"]))

  (define (wrapper content)
    (include-template "wrapper-template.html"))

  (define (content)
    (include-template "content-template.html"))

  (list #"text/html"
        (if bare-html?
          (content)
          (wrapper (content)))))

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

(define (estimated/unestimated cases)
  (let*-values ([(estimated unestimated) (partition has-estimate? cases)])
       (values (group-by-project estimated) 
               (group-by-project unestimated))))

(define (has-estimate? d)
  (< 0 (case-estimate d)))

(define-struct project (title cases))
