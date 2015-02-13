#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(define output-endmarker? #f)
;;;(define (for-all pred? list) (error "implement me!"))
(define-lex-abbrev NEWLINE (:: #\n))

(define paren-stack '())

(define (push-paren! char)
  (set! paren-stack (cons char paren-stack)))

(define (pop-paren! char)
  (define top (car paren-stack))
  (set! paren-stack (cdr paren-stack))
  (match* {top char}
    [{"(" ")"}   (void)]
    [{"["  "]"}   (void)]
    [{"{"  "}"}   (void)]
    [{_    _}     (error "mismatched parens")]))

;;;(define (whitespace-ignored?) (error "implement me!"))
(define-lex-abbrev hash-comment (:+ (:: #\# (:* (char-complement #\newline)) #\newline)))
(define-lex-abbrev open-paren (union #\( #\[ #\{))
(define-lex-abbrev close-paren (union #\) #\] #\}))
(define-lex-abbrev keyword (union "False"   "class"      "finally"     "is"  
                                  "return"  "None"       "continue"    "for"      
                                  "lambda"   "try"       "True"        "def"       
                                  "from"     "nonlocal"  "while"       "and"     
                                  "del"      "global"    "not"         "with"
                                  "as"       "elif"      "if"          "or"
                                  "yield"    "assert"    "else"        "import"
                                  "pass"     "break"     "except"      "in"        
                                  "raise"))

(define-lex-abbrev operators (union "+"      "-"      "*"      "**"     "/"
                                    "//"     "%"      "<<"     ">>"     "&"      
                                    "|"      "^"      "~"      "<"      ">"      
                                    "<="     "="      "=="     "!="     "<>"))

(define-lex-abbrev delimiters (union ","       ":"       "."       ";"       "@"
                                     "="       "->"      "+="      "-="      "*=" 
                                     "/="      "//="     "%="      "&="      "|="
                                     "^="      ">>="     "<<="     "**="     "..."))



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

(define name-to-unicode (make-hash))
(define data (open-input-file "data.txt"))
(define (build-hash input)
  (cond 
    [(< (length input) 1) (void)]
    [else (begin 
            (define line (string-split (car input) ":"))
            (hash-set! name-to-unicode (string-downcase (cadr line)) (string (integer->char (string->number (car line) 16))))
            (build-hash (cdr input)))]))

(build-hash (port->lines data))
;;;(define unicode-name=>integer (error "implement me!"))
;;;(define (char-for-unicode-name name) (error "implement me!"))
;;;(define (char-for string) (error "implement me!"))
;;;(define-lex-abbrev string-quote (error "implement me!"))
;;;(define (unescape-string string #:is-byte (is-byte #f)) (error "implement me!"))
;;;(define (lex-raw-string end-quote port rev-chars) (error "implement me!"))
;;;(define pylex (error "implement me!"))
;;;(define test (error "implement me!"))
(define test-input "")
(define input "")
;;(define (port->list port) (error "implement me!"))
;;(define (port->string port) (error "implement me!"))
(match 
    (current-command-line-arguments) 
    ((vector "-n") (set! output-endmarker? #f) (set! input (current-input-port))) 
    ((vector (or "--test" "--drracket")) (set! input test-input)) 
    ((vector file-name) (set! input (open-input-file file-name))) 
    ((vector) (set! input (current-input-port))))
;(set! input (open-input-string (port->string input)))
;;;(define tokens (error "implement me!"))
;;;(for ((token tokens)) (write token) (newline))


;;;;;;;;;; indent lexing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define indent-stack '())

(define current-spaces 0)

(define (reset-spaces!) 
  (set! current-spaces 0))


(define (inc-spaces!) 
  (set! current-spaces (+ current-spaces 1)))

(define (inc-tab! input-port) 
  (set! current-spaces (+ (- 8 (modulo count 8)) current-spaces)))

(define (current-indent) 
  (if
   (empty? indent-stack)
   0
   (car indent-stack)))

(define (pop-indent!)
  (set! indent-stack (cdr indent-stack)))

(define (pop-indents! number) 
  (cond
   [(< 0 number) (pop-indent!)
                 (pop-indents! (- number 1))]))

(define (generate-dedents number)
  (if 
   (< 1 number) 
   (cons '(DEDENT) (generate-dedents (- number 1)))
   (list '(DEDENT))))

(define (push-indent!) 
  (set! indent-stack (cons current-spaces indent-stack)))

(define (measure-spaces! indent-string) 
  (if 
   (equal? (string-length indent-string) 0) 
   count
   (match (string-ref indent-string 0)
     [#\space (measure-spaces! (substring indent-string 1)) 
              (inc-spaces!)]
     [#\tab (measure-spaces! (substring indent-string 1))
            (inc-tab!)])))


(define indent-lexer
  (lexer
   [(:* (union #\tab #\space)) 
    (begin
      (measure-spaces! lexeme)
      (cond
        [(equal? (current-indent) current-spaces) (basic-lexer input-port)]
        [(< (current-indent) current-spaces) (push-indent!)
                                             (reset-spaces!)
                                             (cons (list 'INDENT) 
                                                   (basic-lexer input-port))]
        [(> (current-indent) current-spaces) 
         (define dedents (member current-spaces (reverse indent-stack)))
         (cond 
           [(list? dedents) (pop-indents! (- (length dedents) 1))
                            (reset-spaces!)
                            `(,@(generate-dedents (- (length dedents) 1)) ,@(basic-lexer input-port))]
           [(equal? 0 current-spaces) (define number-pops (length indent-stack))
                                      (pop-indents! number-pops)
                                      (reset-spaces!)
                                      `(,@(generate-dedents number-pops) ,@(basic-lexer input-port))]
           [else (error "mismatched indents")])]))]
   
   [(eof)  
    (cons (list 'ENDMARKER) (list))]))






;;;;;;;;;;;;;;;; id lexer ;;;;;;;;;;;;;;;;;;;;
(define id-start-chars (list 'lu 'll ' lt 'lm 'lo 'nl))
(define id-continue-chars (list 'mn 'mc 'nd 'pc))
(define other-id-start-chars (list #\u2118 #\u212E #\u309 #\u309B #\u309C))
(define other-id-continue-chars (list #\u00B7 #\u0387 #\u1369 #\u1370 #\u1371 #\u19DA))

(define (id-start? char) 
  (define category (char-general-category char))
  (cond
    [(list? (member category id-start-chars)) #t]
    [(list? (member char other-id-start-chars)) #t]
    [(char=? char #\_) #t]
    [else #f]))

(define (id-continue? char)
  (define category (char-general-category char))
  (cond
    [(id-start? char) #t]
    [(list? (member category id-continue-chars)) #t]
    [(list? (member char other-id-continue-chars)) #t]
    [else #f]))

(define (xid-start? char) 
    (define normalized (string-normalize-nfkc char))
    (match (string->list normalized)
      [(list (? id-start?) (? xid-continue?) ...) #t]
      [else #f]))
      
(define (xid-continue? char) 
    (define normalized (string-normalize-nfkc char))
    (match (string->list normalized)
      [(list (? id-continue?) ...) #t]
      [else #f]))



(define (id-lexer port rev-chars)
  (define id-lexer-wrap
    (lexer
     [any-char
      (cond
        [(xid-continue? lexeme) (id-lexer port (string-append rev-chars lexeme))]
        [else (begin 
                (unget port 1)
                (cons (list 'ID (string-normalize-nfkc rev-chars))
                      (basic-lexer port)))])]
     
     [(eof) 
      (cond
        [(equal? 0 (string-length rev-chars)) (cons (list 'ENDMARKER) (list))]
        [else (cons (list 'ID (string-normalize-nfkc rev-chars)) (cons (list 'ENDMARKER) (list)))])]))
  (id-lexer-wrap port))




(define (unget port length)
  (file-position port (- (file-position port) length)))
(define (advance port length)
  (file-position port (+ (file-position port) length)))





;;;;string literal lexer 
(define-lex-abbrev stringliteral (:: (repetition 0 1 stringprefix) (union shortstring longstring)))
(define-lex-abbrev stringprefix (union #\r #\u #\R #\U))
(define-lex-abbrev shortstring (union (:: #\' (:* shortstringitemsinglequote) #\') (:: #\" (:* shortstringitemdoublequote) #\")))
(define-lex-abbrev longstring (union (:: #\' #\' #\' (complement (:: (:* longstringitem) (:: #\' #\' #\') (:* longstringitem))) #\' #\' #\') 
                                     (:: #\" #\" #\" (complement (:: (:* longstringitem) (:: #\" #\" #\") (:* longstringitem)))  #\" #\" #\")))
(define-lex-abbrev shortstringitemsinglequote (union shortstringcharsinglequote stringescapeseq))
(define-lex-abbrev shortstringitemdoublequote (union shortstringchardoublequote stringescapeseq))
(define-lex-abbrev longstringitem (union longstringchar stringescapeseq))
(define-lex-abbrev shortstringcharsinglequote (intersection any-char (char-complement #\\)  (char-complement #\')))
(define-lex-abbrev shortstringchardoublequote (intersection any-char (char-complement #\\)  (char-complement #\")))
(define-lex-abbrev longstringchar (intersection any-char (char-complement #\\)))
(define-lex-abbrev stringescapeseq (:: #\\ any-char))

(define-lex-abbrev bytesliteral (:: bytesprefix (union shortbytes longbytes)))
(define-lex-abbrev bytesprefix (union #\b #\B (:: #\b #\r) (:: #\B #\r) (:: #\b #\R) (:: #\B #\R) (:: #\r #\b) (:: #\r #\B) (:: #\R #\b) (:: #\R #\B)))
(define-lex-abbrev shortbytes (union (:: #\' (:* shortbytesitemsinglequote) #\') (:: #\" (:* shortbytesitemdoublequote) #\")))
(define-lex-abbrev longbytes (union (:: #\' #\' #\' (complement (:: (:* longbytesitem) (:: #\' #\' #\') (:* longbytesitem))) #\' #\' #\') 
                                    (:: #\" #\" #\" (complement (:: (:* longbytesitem) (:: #\' #\' #\') (:* longbytesitem))) #\" #\" #\")))
(define-lex-abbrev shortbytesitemsinglequote (union shortbytescharsinglequote bytesescapeseq))
(define-lex-abbrev shortbytesitemdoublequote (union shortbyteschardoublequote bytesescapeseq))
(define-lex-abbrev longbytesitem (union longbyteschar bytesescapeseq))
(define-lex-abbrev shortbytescharsinglequote (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\')))
(define-lex-abbrev shortbyteschardoublequote (intersection (char-range #\u0 #\u127) (char-complement #\\) (char-complement #\newline) (char-complement #\")))
(define-lex-abbrev longbyteschar (intersection (char-range #\u0 #\u127) (char-complement #\\)))
(define-lex-abbrev bytesescapeseq (:: #\\ (char-range #\u0 #\u127)))

(define-lex-abbrev quote-markers (union #\" #\' (:: #\" #\" #\") (:: #\' #\' #\')))
(define-lex-abbrev unicode-name (:: #\\ #\N #\{ (:+ (char-complement #\})) #\}))
(define closing-seq "\"")
(define string-mode "")

(define initial-string-lexer
    (lexer
     [quote-markers (begin
                      (set! closing-seq lexeme)
                      (define-lex-abbrev closing-seq-lex closing-seq)
                      (cond
                        [(equal? string-mode "r") (raw-string-lexer input-port "")]
                        [else (normal-string-lexer input-port "")]))]
     [stringprefix (cond
                     [(equal? (string-downcase lexeme) "r") (begin 
                                                              (set! string-mode "r")
                                                              (initial-string-lexer input-port))]
                     [else (initial-string-lexer input-port)])])) 

(define (raw-string-lexer port rev-chars)
  (define raw-string-lexer-inner
    (lexer
     [(:* (intersection (complement quote-markers) (char-complement #\\)))
      (raw-string-lexer input-port (string-append rev-chars lexeme))]
     [quote-markers 
      (cond 
        [(equal? closing-seq lexeme) 
         (cons (list 'LIT rev-chars)
               (basic-lexer input-port))]
        [else (raw-string-lexer input-port (string-append rev-chars lexeme))])]
      [(:: #\\ any-char)
       (raw-string-lexer input-port (string-append rev-chars lexeme))]))

  (raw-string-lexer-inner port))


(define (normal-string-lexer port rev-chars)
  (define normal-lexer-inside
    (lexer
     [(:* (intersection (complement quote-markers) (char-complement #\\)))
      (normal-string-lexer input-port (string-append rev-chars lexeme))]
     [quote-markers 
      (cond 
        [(equal? closing-seq lexeme) 
         (cons (list 'LIT rev-chars)
               (basic-lexer input-port))]
        [else (normal-string-lexer input-port (string-append rev-chars lexeme))])]
      [(:: #\\ any-char)
       (normal-string-lexer input-port (string-append rev-chars lexeme))]
     [unicode-name 
        (begin
          (normal-string-lexer input-port 
                               (string-append rev-chars (dict-ref name-to-unicode (string-downcase (substring lexeme 3 (- (string-length lexeme) 1)))))))]))
  (normal-lexer-inside port))



(define initial-bytestring-lexer
    (lexer
     [quote-markers (begin
                      (set! closing-seq lexeme)
                      (define-lex-abbrev closing-seq-lex closing-seq)
                      (cond
                        [(equal? string-mode "r") (raw-bytestring-lexer input-port "")]
                        [else (normal-bytestring-lexer input-port "")]))]
     [bytesprefix (cond
                     [(equal? (string-downcase lexeme) "r") (begin 
                                                              (set! string-mode "r")
                                                              (initial-bytestring-lexer input-port))]
                     [else (initial-bytestring-lexer input-port)])])) 


(define (raw-bytestring-lexer port rev-chars)
  (define raw-bytestring-lexer-inner
    (lexer
     [(:* (intersection (complement quote-markers) (char-complement #\\) (char-range #\u0 #\u127)))
      (raw-bytestring-lexer input-port (string-append rev-chars lexeme))]
     [quote-markers 
      (cond 
        [(equal? closing-seq lexeme) 
         (cons (list 'LIT rev-chars)
               (basic-lexer input-port))]
        [else (raw-bytestring-lexer input-port (string-append rev-chars lexeme))])]
      [(:: #\\ any-char)
       (raw-bytestring-lexer input-port (string-append rev-chars lexeme))]))

  (raw-bytestring-lexer-inner port))


(define (normal-bytestring-lexer port rev-chars)
  (define normal-bytelexer-inside
    (lexer
     [(:* (intersection (complement quote-markers) (char-complement #\\) (char-range #\u0 #\u127)))
      (normal-bytestring-lexer input-port (string-append rev-chars lexeme))]
     [quote-markers 
      (cond 
        [(equal? closing-seq lexeme) 
         (cons (list 'LIT rev-chars)
               (basic-lexer input-port))]
        [else (normal-bytestring-lexer input-port (string-append rev-chars lexeme))])]
      [(:: #\\ any-char)
       (normal-bytestring-lexer input-port (string-append rev-chars lexeme))]
     [unicode-name 
        (begin
          (normal-bytestring-lexer input-port 
                               (string-append rev-chars (dict-ref name-to-unicode (string-downcase (substring lexeme 3 (- (string-length lexeme) 1)))))))]))
  (normal-bytelexer-inside port))





(define initial-lexer 
  (lexer 
   [(:* (:: (:* (union #\space #\tab hash-comment)) (union #\newline)))
    (basic-lexer input-port)]
   [(eof)  
    (cons (list 'ENDMARKER) (list))]))




(define basic-lexer
  (lexer 
   [(eof)  
    (cons (list 'ENDMARKER) (list))]
   
   [(:* (union #\space #\tab))
    (basic-lexer input-port)]
   
   [(:+ (:: (:* (union #\space #\tab)) (union #\newline hash-comment)))
    (cond
      [(empty? paren-stack) (cons (list 'NEWLINE)
                                  (indent-lexer input-port))]
      [else (basic-lexer input-port)])]
   
   [(:: #\\ #\newline)
    (basic-lexer input-port)]
   
   [keyword (begin
              (define next (peek-string 1 0 input-port))
              (cond 
                [(eof-object? next) (cons (list 'KEYWORD (string->symbol lexeme))
                                          (cons (list 'NEWLINE)
                                                (basic-lexer input-port)))]
                [(xid-continue? next) (begin 
                                        (unget (string-length lexeme))
                                        (id-lexer lexeme))]
                [else (cons (list 'KEYWORD (string->symbol lexeme))
                            (basic-lexer input-port))]))]

   
   [(union operators delimiters)
    (cons (list 'PUNCT (string-trim lexeme))
          (basic-lexer input-port))]
   
   [open-paren
    (begin 
      (push-paren! lexeme)
      (cons (list 'PUNCT lexeme)
            (basic-lexer input-port)))]
   
   [close-paren
    (begin 
      (pop-paren! lexeme)
      (cons (list 'PUNCT lexeme)
            (basic-lexer input-port)))]
        
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
    (cons (list 'LIT (string->number (string-append "0+" (substring lexeme 0 (- (string-length lexeme) 1)) "i")))
          (basic-lexer input-port))]
   
   #;[hash-comment 
    (basic-lexer input-port)]
   
   [stringliteral (begin
                    (unget input-port (string-length lexeme))
                    (initial-string-lexer input-port)) ]
   
   [bytesliteral (begin
                    (unget input-port (string-length lexeme))
                    (initial-bytestring-lexer input-port))]
   
   [(union #\$ #\? #\`)
    (error lexeme)]
   
   [any-char 
    (cond
      [(xid-start? lexeme) (id-lexer input-port lexeme)]
      [else lexeme])]))





(define test-input-port (open-input-string (string-append 
"")))
(define (output dalist)
  (cond
    [(equal? 0 (length dalist)) (void)]
    [else (begin (write (car dalist)) (newline)
                 (output (cdr dalist)))]))

(output (initial-lexer input))


