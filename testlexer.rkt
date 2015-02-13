#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev quote-markers (union #\" #\' (:: #\" #\" #\") (:: #\' #\' #\')))

(define test-lexer
  (lexer
   [(:* (intersection (complement quote-markers) (char-complement #\\))) lexeme]))


(define test-input-port (open-input-string "hello man \\werere"))

(define name-to-unicode (make-hash))

(test-lexer test-input-port)

(define data (open-input-file "data.txt"))

(define (build-hash input)
  (cond 
    [(< (length input) 1) ]
    [else (begin 
            (define line (string-split (car input) ":"))
            (hash-set! name-to-unicode (cadr line) (car line))
            (build-hash (cdr input)))]))


(build-hash (port->lines data))



