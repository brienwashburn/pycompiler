#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define id-start-chars (list 'lu 'll ' lt 'lm 'nl))
(define id-continue-chars (list 'mn 'mc 'nd 'pc 'po 'no))
(define other-id-start-chars (list 'sm 'so 'sk))
(define other-id-continue-chars (list 'po 'no))
 
;;(define (id-lexer port rev-chars) (error "implement me!"))

(define (id-start? char) 
  (define category (char-general-category char))
  (cond
    [(list? (member category id-start-chars)) #t]
    [(list? (member category other-id-start-chars)) #t]
    [(char=? char #\_) #t]
    [else #f]))

(define (id-continue? char)
  (define category (char-general-category char))
  (cond
    [(id-start? char) #t]
    [(list? (member category id-continue-chars)) #t]
    [(list? (member category other-id-continue-chars)) #t]
    [else #f]))

(define (xid-start? char) 
    (define normalized (string-nfkc-normalize (string char)))
    (match (string->list normalized)
      [(list (? id-start?) (? xid-continue?) ...)]
      [else #f]))
      
(define (xid-continue? char) 
    (define normalized (string-nfkc-normalize (string char)))
    (match (string->list normalized)
      [(list (? id-continue?) ...) #t]
      [else #f]))
    
    
    (id-start? #\_)