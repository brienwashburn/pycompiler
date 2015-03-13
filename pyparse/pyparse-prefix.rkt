#lang racket

(require parser-tools/cfg-parser)

(require parser-tools/lex)


;; Helpers:
(define (unzip/callback lst k)
  (match lst
    ['() (k '() '())]
    [(cons (list a b) tl)
     (unzip/callback tl (λ (as bs)
       (k (cons a as) (cons b bs))))]))



;; Lexer definitions:
(define-tokens ID (NAME))

(define-tokens LIT (STRING NUMBER))

(define-empty-tokens PUNCT (+ - * ** / // % << >> & \| ^ ~ < > <= >= == !=
                      <> ... ; had to add these due to bug/omission in lexer spec
                      |(| |)| |[| |]| |{| |}| |,| : |.| |;| @ = -> += -= *= /= //= %= &= \|= ^= >>= <<= **=))


(define-empty-tokens KEYWORD (False class finally is return None continue for lambda try
                        True def from nonlocal while and del global not with as elif if 
                        or yield assert else import pass break except in raise))

(define-empty-tokens SPECIAL (ENDMARKER NEWLINE INDENT DEDENT LIT EOF))



;; Auxiliary definitions:


;; Supply auxiliary helpers here, like process-trailers:
(define (process-trailers base ops)
  (match ops 
    ['() base]
    [(cons (list call one two three four) rest)
      (process-trailers `(,call (func ,base) ,one ,two ,three ,four) rest) ]
    [(cons (list op exp) rest)
      (process-trailers `(,op ,base ,exp) rest) ]))


(define (process-args-list lst)
  (define (recurs lst arg keyword)
    (cond
      [(empty? lst) (list arg keyword)]
      [(equal? "a" (caar lst)) 
       (recurs (cdr lst) (append arg (cdar lst)) keyword)]
      [else 
       (recurs (cdr lst) arg (append keyword (cdar lst)))]))
  (recurs lst '() '()))


(define (process-dotted base ops)
  (match ops 
    [(list name)
     `(Name ,(string->symbol name))]

    [(cons name rest)
     `(Attribute ,(process-dotted name rest) ,(string->symbol name))]))

(define (process-if base lst)
    (match lst
    ['()
      `(,base)]

    [(cons (list tst bdy) rest)
     (process-if `((If (test ,tst) (body ,@bdy) (orelse ,@base))) rest)]))

(define (replace lst ind rep)
  (define (recurs new lst)
    (cond 
      [(equal? (length lst) 0) new]
      [(equal? (length new) (+ ind 1)) (recurs (append new (list rep)) (cdr lst))]
      [else (recurs (append new (list (car lst))) (cdr lst))]))
  (recurs '() lst))


(define (recombine-arglist tups)
 (define (ra-recurs tups args defaults types)
  (cond
   [(empty? tups) (list args defaults types)]
   [else (ra-recurs (cdr tups) (append args (list (caar tups))) (append defaults (list (cadar tups))) (append types (list (caddar tups))))]))
 (ra-recurs tups '() '() '()))

(define (make-dotted lst)
  (define strung (apply string-append (map string-append lst (make-list (length lst) "."))))
  (string->symbol (substring strung 0 (- (string-length strung) 1))))

(define (coalesce lst1 lst2)
  (define (rec lst out)
    (cond
      [(empty? lst) out]
      [(equal? (length (car lst)) 2) (rec (cdr lst) `(  ,(car out) 
                                                        (,@(cadr out) (,@(car lst))) 
                                                        ,(caddr out)  
                                                        ,(cadddr out)))]
      [else (rec (cdr lst) `( (,@(car out) ,@(car lst))
                              ,(cadr out)
                              ,(caddr out) 
                              ,(cadddr out)))]))
  (rec `( ,@lst2 ,@(cadr lst1)) `( () () ,(if (empty? (car lst1)) '(#f) (list (car lst1))) ,(if (empty? (caddr lst1)) '(#f) (list (caddr lst1))))))

(define (unpack lst)
  (define (recurs lst out)
    (cond
      [(empty? lst) out]
      [(equal? 1 (length (car lst))) (recurs (cdr lst) (append out (car lst)))]
      [else (recurs (cdr lst) (append out (map car (map list (car lst)))))]))
  (recurs lst '()))

(define (reorder first  second) 
  (cond
   [(empty? second) (list first '())]
   [else `( (,first ,@(take second (- (length second) 1))) ,(last second))]))


(define (process-string lst)
  (define (recurs lst currstr strs)
     (cond
       [(empty? lst) (if (equal? currstr "") strs (append strs `(Str ,currstr)))]
       [(bytes? (car lst)) (recurs (cdr lst) "" 
                                   (append (if (equal? currstr "") 
                                       strs 
                                       (append strs `(Str ,currstr))) `(Bytes ,(car lst))))]
       [else (recurs (cdr lst) (string-append currstr (car lst)) strs)]))
  (recurs lst "" '()))

(define (recombine lst)
  (define (recurs input lst1 lst2)
    (cond 
     [(empty? input) (list lst1 lst2)]
     [else (recurs (cdr input) (append lst1 (list (caar input))) (append lst2 (list (cadar input))))]))
  (recurs lst '() '()))


(define (process-comp-for lst)
  (define (recurs out base lst)
      (match lst
	['()
	 (cdr (append out (list base)))]
	[(list (list for args in target if))
	 (cdr (append (append out (list base)) `((,for ,args, in ,target ,if))))]
	[(list (list for args in target if rest))
	 (recurs (append out (list base)) `(,for ,args, in ,target ,if) rest)]
	[(list first rest ..0)
	 (recurs out (append base (list first)) rest)]))
  (recurs '() '() lst))


(define (recombine-comp lst)
  (define (recurs input lst1 lst2)
    (cond 
     [(empty? input) (list lst1 lst2)]
     [else (recurs (cdr input) (append lst1 (list (cadaar input))) (append lst2 (list (cadar input))))]))
  (recurs lst '() '()))
;; You may want to put definitions here rather than defining
;; them in the grammar itself.



;; The parser:
(define pyparse
  (cfg-parser
   
   (tokens ID LIT PUNCT KEYWORD SPECIAL)
   
   (end EOF)
  
   ; ATTENTION: To support working "bottom-up" through the grammar,
   ; the start symbol is set to `power` instead of `file_input`.
   ; You should change the start symbol as you move up the kinds
   ; of expressions.
   ;(start comparison)
   (start file_input)
   
   (error (λ (tok-ok? tok-name tok-value)
            (if tok-ok?
                (error (format "Unexpected token: ~a ~a" tok-name tok-value))
                (error (format "Invalid token: " ~a)))))
   
   (grammar


