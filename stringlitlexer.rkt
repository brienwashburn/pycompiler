#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev stringliteral (:: (repetition 0 1 stringprefix) (union shortstring longstring)))
(define-lex-abbrev stringprefix (union #\r #\u #\R #\U))
(define-lex-abbrev shortstring (union (:: #\' (:* shortstringitem) #\') (:: #\" (:* shortstringitem) #\")))
(define-lex-abbrev longstring (union (:: #\' #\' #\' (:* longstringitem) #\' #\' #\') (:: #\" #\" #\" (:* longstringitem) #\" #\" #\")))
(define-lex-abbrev shortstringitem (union shortstringchar stringescapeseq))
(define-lex-abbrev longstringitem (union longstringchar stringescapeseq))
(define-lex-abbrev shortstringchar (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\")))
(define-lex-abbrev longstringchar (intersection any-char (char-complement #\\)))
(define-lex-abbrev stringescapeseq (:: #\\ any-char))

(define-lex-abbrev bytesliteral (:: bytesprefix (union shortbytes longbytes)))
(define-lex-abbrev bytesprefix (union #\b #\B (:: #\b #\r) (:: #\B #\r) (:: #\b #\R) (:: #\B #\R) (:: #\r #\b) (:: #\r #\B) (:: #\R #\b) (:: #\R #\B)))
(define-lex-abbrev shortbytes (union (:: #\' (:* shortbytesitem) #\') (:: #\" (:* shortbytesitem) #\")))
(define-lex-abbrev longbytes (union (:: #\' #\' #\' (:* longbytesitem) #\' #\' #\') (:: #\" #\" #\" (:* longbytesitem) #\" #\" #\")))
(define-lex-abbrev shortbytesitem (union shortbyteschar bytesescapeseq))
(define-lex-abbrev longbytesitem (union longbyteschar bytesescapeseq))
(define-lex-abbrev shortbyteschar (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\"))) ;;; NOT SURE ABOUT WHAT QUOTE
(define-lex-abbrev longbyteschar (intersection (char-range #\u0 #\u127) (char-complement #\\)))
(define-lex-abbrev bytesescapeseq (:: #\\ (char-range #\u0 #\u127)))


(define (unget port length)
  (file-position port (- (file-position port) length)))


(define string-lexer1
  (lexer
   [stringliteral (begin 
                    (unget input-port (string-length lexeme)) 
                    (string-lexer-baby input-port))]
   [bytesliteral 'dolly]
   [any-char 'nope]))

(define string-lexer-baby
    (lexer
     [#\" (string-lexer-baby input-port)]
     [(:* shortstringitem) 'the]
     [(:* any-char) 'fuck]))

(define (someshit port)
  'whatthefuck)

(define test-input-port (open-input-string "\"hellhghgjghjhjo\""))

(string-lexer1 test-input-port)


  
  
  
  

   