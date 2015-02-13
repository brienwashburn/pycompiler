#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev stringliteral (:: (repetition 0 1 stringprefix) (union shortstring longstring)))
(define-lex-abbrev stringprefix (union #\r #\u #\R #\U))
(define-lex-abbrev shortstring (union (:: #\' (:* shortstringitemsinglequote) #\') (:: #\" (:* shortstringitemdoublequote) #\")))
(define-lex-abbrev longstring (union (:: #\' #\' #\' (:* longstringitem) #\' #\' #\') (:: #\" #\" #\" (:* longstringitem) #\" #\" #\")))
(define-lex-abbrev shortstringitemsinglequote (union shortstringcharsinglequote stringescapeseq))
(define-lex-abbrev shortstringitemdoublequote (union shortstringchardoublequote stringescapeseq))
(define-lex-abbrev longstringitem (union longstringchar stringescapeseq))
(define-lex-abbrev shortstringcharsinglequote (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\')))
(define-lex-abbrev shortstringchardoublequote (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\"ZZZZ)))
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

(define-lex-abbrev quote-markers (union #\" #\' (:: #\" #\" #\") (:: #\' #\' #\')))

(define-lex-abbrev unicode-name (:: #\\ #\N #\{ (:+ (intersection any-char (char-complement #\}))) #\}))



(define (unget port length)
  (file-position port (- (file-position port) length)))

(define (advance port length)
  (file-position port (+ (file-position port) length)))


(define closing-seq "")
(define string-mode "")


(define string-lexer1
  (lexer
   [stringliteral (begin 
                    (unget input-port (string-length lexeme)) 
                    (string-lexer-baby input-port))]
   [bytesliteral 'dolly]
   [any-char 'nope]))

(define initial-string-lexer
    (lexer
     [quote-markers (begin
                      (set! current-closing-seq lexeme)
                      (cond
                        [(equal? string-mode "r") (raw-string-lexer input-port)]
                        [else (normal-string-lexer input-port)]))]
     [stringprefix (cond
                     [(equal? (string-downcase lexeme) "r") (begin 
                                                              (set! string-mode "r")
                                                              (initial-string-lexer input-port))]
                     [else (initial-string-lexer input-port)])])) 

(define raw-string-lexer
  (lexer
   [(:: (intersection (:* any-char) (complement closing-seq)))
    (begin
      (advance input-port (string-length closing-seq))
      (cons (list 'LIT lexeme)
            (basic-lexer input-port)))]))


(define (normal-lexer port literal)
  (define normal-lexer-inside
    (lexer
     [unicode-name 
      (begin
        (define thang (SEARCH A DICTIONARY (string-downcase (substring lexeme 3 (- (length lexeme) 1)))))
        (string? thang)
        ]


(define test-input-port (open-input-string "\"hellhghgjghjhjo\""))

(string-lexer1 test-input-port)


  
  
  
  

   