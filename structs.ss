#lang scheme
(require (planet bzlib/xml:1:1)
         (planet lizorkin/sxml:2:1/sxml))

(provide (all-defined-out))

(define (case-xml->dict xml)
  (for/hash ([tag ((sxpath "*") xml)])
            (values ((sxpath "name(.)") tag)
                    (let ([text ((sxpath "./text()") tag)])
                      (and (not (null? text))
                           (first text))))))

(define (case-id case)
  (dict-ref case "ixbug"))

(define (case-title case)
  (dict-ref case "stitle"))

(define (case-project case)
  (dict-ref case "sproject"))

(define (case-estimate case)
  (string->number (dict-ref case "hrscurrest")))

(define (case-resolved? case)
  (define resolved-codes
    ;; holy shit why are there so many status codes...
    '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 19 20 21 22 24 25 26 27 28 30 31 32 33 34 36 37 38 39 40 42 43 44))
  (define n (string->number (dict-ref case "ixstatus" "0")))
  (and (member n resolved-codes) n))

(define-struct (exn:fogbugz-error exn:fail) (code) #:transparent)
