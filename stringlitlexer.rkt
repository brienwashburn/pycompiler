#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;(define-lex-abbrev stringliteral)
;(define-lex-abbrev stringprefix)
;(define-lex-abbrev shortstring)
;(define-lex-abbrev longstring)
;(define-lex-abbrev shortstringitem)
;(define-lex-abbrev longstringitem)
;(define-lex-abbrev shortstringchar)
;(define-lex-abbrev longstringchar)
;(define-lex-abbrev stringescapeseq)

(define-lex-abbrev bytesliteral (:: bytesprefix (union shortbytes longbytes)))
(define-lex-abbrev bytesprefix (union #\b #\B (:: #\b #\r) (:: #\B #\r) (:: #\b #\R) (:: #\B #\R) (:: #\r #\b) (:: #\r #\B) (:: #\R #\b) (:: #\R #\B)))
(define-lex-abbrev shortbytes (union (:: #\' (:* shortbytesitem) #\') (:: #\" (:* shortbytesitem) #\")))
(define-lex-abbrev longbytes (union (:: #\' #\' #\' (:* longbytesitem) #\' #\' #\') (:: #\" #\" #\" (:* longbytesitem) #\" #\" #\")))
(define-lex-abbrev shortbytesitem (union shortbyteschar bytesescapeseq))
(define-lex-abbrev longbytesitem (union longbyteschar bytesescapeseq))
(define-lex-abbrev shortbyteschar (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\"))) ;;; NOT SURE ABOUT WHAT QUOTE
(define-lex-abbrev longbyteschar (intersection (char-range #\u0 #\u127) (char-complement #\\)))
(define-lex-abbrev bytesescapeseq (:: #\\ (char-range #\u0 #\u127)))


(define string-lexer
  (lexer
   [bytesprefix 'hello]
   [any-char 'nope]))

(define test-input-port (open-input-string "RB"))

(string-lexer test-input-port)
  
  
  
  

   