#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
;;;(define output-endmarker? (error "implement me!"))
;;;(define (for-all pred? list) (error "implement me!"))
;;;(define (unget port) (error "implement me!"))
(define-lex-abbrev NEWLINE (:: #\newline (union (:* #\space) (:* #\tab))))
;;;(define current-spaces (error "implement me!"))
;;;(define (reset-spaces!) (error "implement me!"))
;;;(define (inc-spaces!) (error "implement me!"))
;;;(define (inc-tab!) (error "implement me!"))
(define indent-stack '())
;;;(define (current-indent) (error "implement me!"))
;;;(define (push-indent! spaces) (error "implement me!"))
;;;(define (pop-indent!) (error "implement me!"))
;;;(define (measure-spaces!) (error "implement me!"))
;;;(define (pop-indents!) (error "implement me!"))
(define paren-stack '())
(define (push-paren! char)
  (set! paren-stack (cons char paren-stack)))
(define (pop-paren! char)
  (define top (car paren-stack))
  (set! paren-stack (cdr paren-stack))
  (match* {top char}
    [{#\(  #\)}   (void)]
    [{#\[  #\]}   (void)]
    [{#\{  #\}}   (void)]
    [{_    _}     (error "mismatched parens")]))
;;;(define (whitespace-ignored?) (error "implement me!"))
(define-lex-abbrev hash-comment (:: #\# (:* (char-complement #\newline)) #\newline))
;;;(define-lex-abbrev keyword (error "implement me!"))
;;;(define-lex-abbrev operator (error "implement me!"))
;;;(define-lex-abbrev delimiter (error "implement me!"))
(define-lex-abbrev nonzerodigit (char-range #\1 #\9))
(define-lex-abbrev digit (char-range #\0 #\9))
(define-lex-abbrev octdigit (char-range #\0 #\7))
;;;(define (octal-digit? char) (error "implement me!"))
(define-lex-abbrev hexdigit (union digit
                                   (char-range #\a #\f)
                                   (char-range #\A #\F)))
;;;(define (hex-digit? char) (error "implement me!"))
(define-lex-abbrev bindigit (union #\0 #\1))
(define-lex-abbrev octinteger (:: #\0 (:or #\o #\O) (:+ octdigit)))
(define-lex-abbrev hexinteger (:: #\0 (:or #\x #\X) (:+ hexdigit)))
(define-lex-abbrev bininteger (:: #\0 (:or #\b #\B) (:+ bindigit)))
(define-lex-abbrev decimalinteger (:or (:: nonzerodigit (:* digit)) (:+ #\0)))
(define-lex-abbrev floatnumber (:or pointfloat exponentfloat))
(define-lex-abbrev pointfloat (:or (:: (:* intpart) fraction) (:: intpart #\.)))
(define-lex-abbrev exponentfloat (:: (:or intpart pointfloat) exponent))
(define-lex-abbrev intpart (:+ digit))
(define-lex-abbrev fraction (:: #\. (:+ digit)))
(define-lex-abbrev exponent (:: (:or #\e #\E) (:* (:or #\+ #\-)) (:+ digit)))
(define-lex-abbrev imagnumber (:: (:or floatnumber intpart) (:or #\j #\J)))
;;;(define unicode-name=>integer (error "implement me!"))
;;;(define (char-for-unicode-name name) (error "implement me!"))
;;;(define (char-for string) (error "implement me!"))
;;;(define-lex-abbrev string-quote (error "implement me!"))
;;;(define (unescape-string string #:is-byte (is-byte #f)) (error "implement me!"))
;;;(define (lex-raw-string end-quote port rev-chars) (error "implement me!"))
;;;(define other-id-start-chars (error "implement me!"))
;;;(define other-id-continue-chars (error "implement me!"))
;;;(define (other-id-start? char) (error "implement me!"))
;;;(define (other-id-continue? char) (error "implement me!"))
;;;(define (id-start? char) (error "implement me!"))
;;;(define (id-continue? char) (error "implement me!"))
;;;(define (xid-start? char) (error "implement me!"))
;;;(define (xid-continue? char) (error "implement me!"))
;;;(define (id-lexer port rev-chars) (error "implement me!"))
;;;(define pylex (error "implement me!"))
(define indent-lexer
  (lexer
   [#\tab 
    (inc-tab!)
    (indent-lexer input-port)
   [#\space 
    (inc-space!)
    (indent-lexer input-port)]
   [_ 
    (cond
    [(equal? (current-indent) current-spaces) ]
    [(< (current-indent) current-spaces) (push-indent! current-spaces)]
    [(> (current-indent) current-spaces) 
     (define dedents (length (member current-spaces indent-stack)))
     (if
      (equal? 0 dedents)
      (error "mismatched indents")
      (pop-indents! (- dedents 1)))])
    (basic-lexer input-port)]]))

;;;(define test (error "implement me!"))
;;;(define test-input (error "implement me!"))
;;;(define input (error "implement me!"))
;;;(define (port->list port) (error "implement me!"))
;;;(define (port->string port) (error "implement me!"))
;;;(match (current-command-line-arguments) ((vector "-n") (set! output-endmarker? #f) (set! input (current-input-port))) ((vector (or "--test" "--drracket")) (set! input test-input)) ((vector file-name) (set! input (open-input-file file-name))) ((vector) (set! input (current-input-port))))
;;;(set! input (open-input-string (port->string input)))
;;;(define tokens (error "implement me!"))
;;;(for ((token tokens)) (write token) (newline))

(define basic-lexer
  (lexer 
   [(eof)  (list)]
   
   [(repetition 1 +inf.0 (char-range #\a #\z))

    (cons (list 'ID lexeme)
          (basic-lexer input-port))]
   
   [(repetition 1 +inf.0 (union #\space #\newline))

    (apply basic-lexer (list input-port))]
   
   [decimalinteger 
    (cons (list 'LIT (string->number lexeme))
          (basic-lexer input-port))]
   
   [hexinteger
    (cons (list 'LIT (string->number (substring lexeme 2) 16))
          (basic-lexer input-port))]
   
   [octinteger
    (cons (list 'LIT (string->number (substring lexeme 2) 8))
          (basic-lexer input-port))]
   [bininteger
    (cons (list 'LIT (string->number (substring lexeme 2) 2))
          (basic-lexer input-port))]
   [floatnumber
    (cons (list 'LIT (string->number lexeme))
          (basic-lexer input-port))]
   
   [imagnumber
    (cons (list 'LIT (string->number lexeme))
          (basic-lexer input-port))]
   
   [hash-comment 
    (basic-lexer input-port)]))
   ;;;[NEWLINE 
    ;;(cons (list 'NEWLINE)
      ;;    (indent-lexer input-port))]))


(define test-input-port (open-input-string "    foo 55 0x34 0b1011

#who dat
3.14

10.
.001 
1e100
3.14E-10
0e0
1.j
# there what 1.2 233 ello

bar   baz

"))

(basic-lexer test-input-port)