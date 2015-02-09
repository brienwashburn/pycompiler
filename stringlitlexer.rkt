#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev stringliteral)
(define-lex-abbrev stringprefix)
(define-lex-abbrev shortstring)
(define-lex-abbrev longstring)
(define-lex-abbrev shortstringitem)
(define-lex-abbrev longstringitem)
(define-lex-abbrev shortstringchar)
(define-lex-abbrev longstringchar)
(define-lex-abbrev stringescapeseq)

(define-lex-abbrev bytesliteral)
(define-lex-abbrev bytesprefix)
(define-lex-abbrev shortbytes)
(define-lex-abbrev longbytes)
(define-lex-abbrev shortbytesitem)
(define-lex-abbrev longbytesitem)
(define-lex-abbrev shortbyteschar)
(define-lex-abbrev longbyteschar)
(define-lex-abbrev bytesescapeseq (:: #\\ (char-range #\u0 #\u127)))


(define string-lexer
  (lexer