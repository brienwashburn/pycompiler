#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev quote-markers (union #\" #\' (:: #\" #\" #\") (:: #\' #\' #\')))

(define test-lexer
  (lexer
   [(:* (intersection (complement quote-markers) (char-complement #\\))) lexeme]))


(define test-input-port (open-input-string "hello man \\werere"))


(test-lexer test-input-port)