#lang scheme
(require (planet bzlib/xml:1:1)
         (planet lizorkin/sxml:2:1/sxml))

(provide (all-defined-out))

(define (case-xml->dict xml)
  (for/hash ([tag ((sxpath "*") xml)])
            (values ((sxpath "name(.)") tag)
                    (first ((sxpath "./text()") tag)))))

(define (case-id case)
  (dict-ref case "ixbug"))

(define (case-title case)
  (dict-ref case "stitle"))

(define (case-project case)
  (dict-ref case "sproject"))

(define (case-estimate case)
  (dict-ref case "hrscurrest"))

(define-struct (exn:fogbugz-error exn:fail)
               (code message)
               #:transparent)
