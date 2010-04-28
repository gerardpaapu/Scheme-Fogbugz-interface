#lang scheme
(require "structs.ss"
         (planet dherman/json:3:0)
         web-server/http/response-structs
         web-server/templates)

(provide list-cases list-cases-json)

(define (list-cases cases current-case
                    start-work-url stop-work-url set-estimate-url
                    close-bug-url resolve-bug-url quick-interval-url)
  
  (define-values (estimated unestimated) (estimated/unestimated cases))
    
  (define (case-class case)
    (cond [(and current-case
             (string=? (case-id case)
                       (case-id current-case))) "case current"]      
          [(case-resolved? case) "case resolved"]
          [else "case"]))
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

(define (estimated/unestimated cases)
  (let*-values ([(estimated unestimated) (partition has-estimate? cases)])
       (values (group-by-project estimated) 
               (group-by-project unestimated))))

(define (has-estimate? d)
  (< 0 (case-estimate d)))

(define-struct project (title cases))

(define (case->dict case)
  (and case
       (make-immutable-hasheq `([id . ,(case-id case)]
                                [title . ,(case-title case)]
                                [resolved . ,(case-resolved? case)]
                                [project . ,(case-project case)]))))

(define (project->dict project)
  (make-immutable-hasheq
    `([title . ,(project-title project)]
      [cases . ,(map case->dict (project-cases project))])))

(define (list-cases-json cases current-case)
  (define-values (estimated unestimated) (estimated/unestimated cases))
  (define data (make-immutable-hasheq `([current-case . ,(case->dict current-case)]
                                        [estimated . ,(map project->dict estimated)]
                                        [unestimated . ,(map project->dict unestimated)])))

  (make-response/full 200 #"Okay"
                       (current-seconds)
                       #"application/json"
                       empty
                       (list (string->bytes/utf-8 (jsexpr->json data)))))
