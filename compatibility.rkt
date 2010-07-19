#lang racket
;;; A thin compatibility layer to use the sxml tools with
;;; plt xexprs, the one I was using isn't working right now
;;; for whatever reason (as of racket-5.0)
;;;
;;; I don't make any claim that this will be exhaustive.
;;; I just need this app to work with recent versions of racket

(require xml)
(provide xml-document->sxml
         read-sxml)

(define (read-sxml port)
  (xml-document->sxml (read-xml port)))

(define (xml-document->sxml xml)
  (let ([body (document-element xml)])
    (list '*TOP* (xexpr/element->sxml (xml->xexpr body)))))

(define (xexpr/element->sxml xexpr)
  (match xexpr
    [(? cdata? d) (extract-cdata d)]

    [(list tag-name '() children ...)
     (list* (symbol-downcase tag-name)
            (map xexpr/element->sxml children))]
    
    [(list tag-name attrs children ...)
     (list* (symbol-downcase tag-name)
            (xexpr-attr->sxml attrs)
            (map xexpr/element->sxml children))]
     
    [else xexpr]))

(define (extract-cdata data)
  (let* ([string (cdata-string data)]
         [matches (regexp-match #rx"<!\\[CDATA\\[(.*)\\]\\]>" string)])
    (list-ref matches 1)))

(define (xexpr-attr->sxml attrs)
  `(@ ,@(map (match-lambda [(list key value) (cons (symbol-downcase key) value)])
             attrs)))

(define (symbol-downcase sym)
  (string->symbol (string-downcase (symbol->string sym))))