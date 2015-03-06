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
    [(cons (list args kw str kwarg)  rest)
     (process-trailers `(Call (func  ,base) ,args ,kw ,str ,kwarg) rest)]
    [(cons (list name) rest )
      (process-trailers `(Attribute ,base ,name) rest) ]
    [(cons (list ind name) rest )
      (process-trailers `(Subscript ,base (,ind ,name)) rest) ]))

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

(define (recombine lst)
  (define (recurs input lst1 lst2)
    (cond 
     [(empty? input) (list lst1 lst2)]
     [else (recurs (cdr input) (append lst1 (list (caar input))) (append lst2 (cadar input)))]))
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
   (start file_input)
   
   (error (λ (tok-ok? tok-name tok-value)
            (if tok-ok?
                (error (format "Unexpected token: ~a ~a" tok-name tok-value))
                (error (format "Invalid token: " ~a)))))
   
   (grammar


(single_input
 ((NEWLINE)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((simple_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((compound_stmt NEWLINE)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) $$))))

(file_input
 (($nt61 ENDMARKER)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(Module ,@(unpack ($ 1))))))
 ((ENDMARKER)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) `(Module)))))

(eval_input
 ((testlist $nt63)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) 'ENDMARKER))))

(decorator
 (($@64 dotted_name $nt65 NEWLINE)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 3)
        `(decorator_list
          (Call (func ,(process-dotted '() (reverse ($ 2)))) ,@($ 3)))
        `(decorator_list ,(process-dotted '() (reverse ($ 2)))))))))

(decorators
 (($nt70)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(decorated
 ((decorators classdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (replace ($ 2) (- (length ($ 2)) 2) (car ($ 1))))))
 ((decorators funcdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (replace ($ 2) (- (length ($ 2)) 3) (car ($ 1)))))))

(funcdef
 (($def71 NAME parameters $nt72 $:75 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6)))))
      (if ($ 4)
        `(FunctionDef
          (name ,(string->symbol ($ 2)))
          (args ,($ 3))
          (body ,@($ 6))
          (decorator_list)
          (return ,($ 4)))
        `(FunctionDef
          (name ,(string->symbol ($ 2)))
          (args ,($ 3))
          (body ,@($ 6))
          (decorator_list)
          (return #f)))))))

(parameters
 ((|$(76| $nt77 |$)78|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 2)
        ($ 2)
        `(Arguments
          (args)
          (args-types)
          (vararg #f)
          (kwonlyargs)
          (kwonlyarg-types)
          (kw_defaults)
          (kwarg #f)
          (defaults)))))))

(typedargslist
 (($nt79)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(Arguments
          ,`(args ,@($ 0))
          ,`(arg-types ,@($ 1))
          ,`(vararg ,@($ 2))
          ,`(kwonlyargs ,@($ 3))
          ,`(kwonlyarg-types ,@($ 4))
          ,`(kw_defaults ,@($ 5))
          ,`(kwarg ,@($ 6))
          ,`(defaults ,@($ 7))))))))

(targ-kwonly-kwarg
 (($*92 tfpdef $nt93 $nt95)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 4)
        `(()
          ()
          (,(car ($ 2)))
          (,@(car ($ 3)))
          (,@(caddr ($ 3)))
          (,@(cadr ($ 3)))
          (,(car ($ 4)))
          ())
        `(()
          ()
          (,(car ($ 2)))
          (,@(car ($ 3)))
          (,@(caddr ($ 3)))
          (,@(cadr ($ 3)))
          (#f)
          ()))))))

(tkwonly
 ((|$,99| tfpdef $nt100)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3)
        `(,(car ($ 2)) ,($ 3) ,(cadr ($ 2)))
        `(,(car ($ 2)) #f ,(cadr ($ 2))))))))

(tkwarg
 (($**103 tfpdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(() () (#f) () () () (,(car ($ 2))) ())))))

(tfpdef
 ((NAME $nt104)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        `(,(string->symbol ($ 1)) ,($ 2))
        `(,(string->symbol ($ 1)) #f))))))

(varargslist
 (($nt107)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(Arguments
          ,`(args ,@($ 0))
          ,`(arg-types ,@($ 1))
          ,`(vararg ,@($ 2))
          ,`(kwonlyargs ,@($ 3))
          ,`(kwonlyarg-types ,@($ 4))
          ,`(kw_defaults ,@($ 5))
          ,`(kwarg ,@($ 6))
          ,`(defaults ,@($ 7))))))))

(arg-kwonly-kwarg
 (($*120 vfpdef $nt121 $nt123)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 4)
        `(()
          ()
          (,($ 2))
          (,@(car ($ 3)))
          (,@(caddr ($ 3)))
          (,@(cadr ($ 3)))
          (,($ 4))
          ())
        `(()
          ()
          (,($ 2))
          (,@(car ($ 3)))
          (,@(caddr ($ 3)))
          (,@(cadr ($ 3)))
          (#f)
          ()))))))

(kwonly
 ((|$,127| vfpdef $nt128)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3) `(,($ 2) ,($ 3) #f) `(,($ 2) #f #f))))))

(kwarg
 (($**131 vfpdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(() () (#f) () () () (,($ 2)) ())))))

(vfpdef
 ((NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (string->symbol ($ 1))))))

(stmt
 ((simple_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((compound_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(simple_stmt
 (($nt132 NEWLINE)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 1)))))

(small_stmt
 ((expr_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((del_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((pass_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((flow_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((import_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((global_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((nonlocal_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((assert_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(expr_stmt
 ((testlist_star_expr augassign $nt135)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(AugAssign ,@($ 1) ,($ 2) ,($ 3)))))
 ((testlist_star_expr $nt136)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(Assign
        (targets ,@(car (reorder ($ 1) ($ 2))))
        (value ,(cadr (reorder ($ 1) ($ 2))))))))
 ((testlist_star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) `(Expr ,($ 1))))))

(testlist_star_expr
 (($nt140)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        (if (equal? 1 (length $$)) (car $$) `(Tuple ,@$$)))))))

(augassign
 (($+=143)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Add))))
 (($-=144)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Sub))))
 (($*=145)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Mult))))
 (($/=146)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Div))))
 (($%=147)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Mod))))
 (($&=148)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'BitAnd))))
 (($\|=149)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'BitOr))))
 (($^=150)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'BitXor))))
 (($<<=151)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'LShift))))
 (($>>=152)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'RShift))))
 (($**=153)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Pow))))
 (($//=154)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'FloorDiv)))))

(del_stmt
 (($del155 exprlist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(Delete ,($ 2))))))

(pass_stmt
 (($pass156)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) '(Pass))))))

(flow_stmt
 ((break_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((continue_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((return_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((raise_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((yield_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(break_stmt
 (($break157)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) '(Break)))))

(continue_stmt
 (($continue158)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) '(Continue)))))

(return_stmt
 (($return159 $nt160)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(Return ,($ 2)) '(Return))))))

(yield_stmt
 ((yield_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(raise_stmt
 (($raise161 $nt162)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(Raise ,@($ 2)) `(Raise))))))

(import_stmt
 ((import_name)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((import_from)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(import_name
 (($import167 dotted_as_names)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(Import ,@(car ($ 2)))))))

(import_from
 (($from168 $nt169 $import178 $nt179)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(ImportFrom
        (module ,(car ($ 2)))
        (names ,@(car ($ 4)))
        (level ,(cadr ($ 2))))))))

(import_as_name
 ((NAME $nt183)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        `(,(string->symbol ($ 1)) ,($ 2))
        `(,(string->symbol ($ 1)) #f))))))

(dotted_as_name
 ((dotted_name $nt186)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,(make-dotted ($ 1)) ,($ 2)) `(,(make-dotted ($ 1)) #f))))))

(import_as_names
 (($nt189)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) $$))))

(dotted_as_names
 (($nt192)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) $$))))

(dotted_name
 (($nt193)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

(global_stmt
 (($global194 $nt195)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(Global ,(map string->symbol ($ 2)))))))

(nonlocal_stmt
 (($nonlocal196 $nt197)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(Nonlocal ,(map string->symbol ($ 2)))))))

(assert_stmt
 (($assert198 $nt199)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(Assert ,@($ 2))))))

(compound_stmt
 ((if_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((while_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((for_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((try_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((with_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((funcdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((classdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((decorated)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(if_stmt
 (($nt200)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(If (test ,(car $$)) (body ,(cadr $$)) (orelse ,@(caddr $$))))))))

(while_stmt
 (($while223 test $:224 suite $nt225)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5)))))
      (if ($ 5)
        `(While (test ,($ 2)) (body ,@($ 4)) (orelse ,@($ 5)))
        `(While (test ,($ 2)) (body ,@($ 4)) (orelse)))))))

(for_stmt
 (($for229 exprlist $in230 testlist $:231 suite $nt232)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$
                  (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7)))))
      (if ($ 7)
        `(For (target ,@($ 2)) (iter ,($ 4)) (body ,@($ 6)) (orelse ,@($ 7)))
        `(For (target ,@($ 2)) (iter ,($ 4)) (body ,@($ 6)) (orelse)))))))

(try_stmt
 (($try236 $:237 suite $nt238)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(Try
        (body ,@($ 3))
        (handlers ,@(car ($ 4)))
        (orelse ,@(cadr ($ 4)))
        (finalbody ,@(caddr ($ 4))))))))

(with_stmt
 (($with265 $nt266 $:267 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(With ,@($ 2) ,($ 4))))))

(with_item
 ((test $nt268)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,($ 1) ,($ 2)) `(,($ 1) #f))))))

(except_clause
 (($except271 $nt272)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) ($ 2) `(#f #f))))))

(suite
 ((simple_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((NEWLINE INDENT $nt277 DEDENT)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) ($ 3)))))

(test
 ((or_test $nt278)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(IfExp ,(car ($ 2)) ,($ 1) ,(cadr ($ 2))) ($ 1)))))
 ((lambdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(test_nocond
 ((or_test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((lambdef_nocond)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(lambdef
 (($lambda282 $nt283 $:284 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 4) `(Lambda ,($ 2) ,($ 4)) `(Lambda ,($ 3)))))))

(lambdef_nocond
 (($lambda285 $nt286 $:287 test_nocond)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 4) `(Lambda ,($ 2) ,($ 4)) `(Lambda ,($ 3)))))))

(or_test
 ((or_test $or288 and_test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BoolOp Or ,($ 1) ,($ 3)))))
 ((and_test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(and_test
 ((and_test $and289 not_test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BoolOp And ,($ 1) ,($ 3)))))
 ((not_test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(not_test
 (($not290 not_test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(UnaryOp Not ,($ 2)))))
 ((comparison)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(comparison
 ((comparison comp_op expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(Compare (left ,($ 1)) ,($ 2) (comparators ,($ 3))))))
 ((expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(comp_op
 (($nt291)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) `(ops ,$$))))))

(star_expr
 (($*304 expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(Starred ,($ 2))))))

(expr
 ((expr $nt305 xor_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BinOp ,($ 1) ,($ 2) ,($ 3)))))
 ((xor_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(xor_expr
 ((xor_expr $nt307 and_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BinOp ,($ 1) ,($ 2) ,($ 3)))))
 ((and_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(and_expr
 ((and_expr $nt309 shift_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BinOp ,($ 1) ,($ 2) ,($ 3)))))
 ((shift_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(shift_expr
 ((shift_expr $nt311 arith_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BinOp ,($ 1) ,($ 2) ,($ 3)))))
 ((arith_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(arith_expr
 ((arith_expr $nt314 term)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BinOp ,($ 1) ,($ 2) ,($ 3)))))
 ((term)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(term
 ((term $nt317 factor)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(BinOp ,($ 1) ,($ 2) ,($ 3)))))
 ((factor)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(factor
 (($nt322 factor)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(UnaryOp ,($ 1) ,($ 2)))))
 ((power)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(power
 ((atom $nt326 $nt327)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3)
        `(BinOp ,(process-trailers ($ 1) ($ 2)) Pow ,($ 3))
        (process-trailers ($ 1) ($ 2)))))))

(atom
 ((|$(330| $nt331 |$)332|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 2))))
 ((|$(333| |$)334|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) '(Tuple))))
 ((|$[335| testlist_comp |$]336|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) `(Tuple ,($ 2)))))
 ((|$[337| |$]338|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) '(List))))
 ((|${339| dictorsetmaker |$}340|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 2))))
 ((|${341| |$}342|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(Dict (keys) (values)))))
 ((NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      `(Name ,(string->symbol ($ 1))))))
 ((NUMBER)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) `(Num ,$$)))))
 (($nt343)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      `(Str ,(apply string-append ($ 1))))))
 (($...344)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) '(Ellipsis)))))
 (($None345)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(NameConstant ,(string->symbol $$))))))
 (($True346)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(NameConstant ,(string->symbol $$))))))
 (($False347)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(NameConstant ,(string->symbol $$)))))))

(testlist_comp
 (($nt348 comp_for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(GeneratorExp ,($ 1) ,($ 2)))))
 (($nt349)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (if (equal? 1 (length ($ 1)))
        `(GeneratorExp ,@($ 1))
        `(Tuple ,@($ 1)))))))

(trailer
 ((|$(352| arglist |$)353|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 2))))
 ((|$(354| |$)355|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `((args) (keywords) (starargs #f) (kwargs #f)))))
 ((|$[356| subscriptlist |$]357|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) `(Index ,($ 2)))))
 (($.358 NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      `(,(string->symbol ($ 2)))))))

(subscriptlist
 ((subscript)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($nt359)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) `(Tuple ,@$$))))))

(subscript
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((test $:362 test $nt363)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 4) `(Slice ,($ 1) ,($ 3) ,($ 4)) `(Slice ,($ 1) ,($ 3) #f)))))
 ((test $:364 $nt365)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3) `(Slice ,($ 1) #f ,($ 3)) `(Slice ,($ 1) #f #f)))))
 (($:366 test $nt367)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3) `(Slice #f ,($ 2) ,($ 3)) `(Slice #f ,($ 2) #f)))))
 (($:368 $nt369)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(Slice #f #f ,($ 2)) `(Slice #f #f #f))))))

(sliceop
 (($:370 $nt371)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (if ($ 2) ($ 2) #f)))))

(exprlist
 (($nt372 $nt373 $nt378)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if (empty? ($ 2)) `(,($ 1)) `(,($ 1) ,($ 2)))))))

(testlist
 ((test $nt380 $nt384)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if (empty? ($ 2)) ($ 1) `(Tuple ,($ 1) ,@($ 2)))))))

(dictorsetmaker
 ((test $:386 test comp_for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(DictComp ,($ 1) ,($ 3) ,($ 4)))))
 (($nt387)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(Dict (keys ,@(car $$)) (values ,(cadr $$)))))))
 ((test comp_for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(SetComp ,($ 1) ,($ 2)))))
 (($nt391)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) `(Set ,@$$))))))

(classdef
 (($class394 NAME $nt395 $:400 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5)))))
      (if ($ 3)
        `(ClassDef
          (name ,(string->symbol ($ 2)))
          ,@($ 3)
          (body ,@($ 5))
          (decorator_list))
        `(ClassDef
          (name ,(string->symbol ($ 2)))
          (bases)
          (keywords)
          (starargs #f)
          (kwargs #f)
          (body ,@($ 5))
          (decorator_list)))))))

(arglist
 (($nt401)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(,`(args ,($ 0))
          ,`(keywords ,@($ 1))
          ,`(starargs ,@($ 2))
          ,`(kwargs ,@($ 3))))))))

(argument
 ((test $nt419)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(GeneratorExp ,($ 1) ,($ 2)) `(,($ 1))))))
 ((test $=420 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(,(cadr ($ 1)) ,($ 3))))))

(comp_iter
 ((comp_for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((comp_if)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(comp_for
 (($for421 exprlist $in422 or_test $nt423)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5)))))
      (if ($ 5)
        `(for ,@($ 2) in ,($ 4) if ,@($ 5))
        `(for ,@($ 2) in ,($ 4) if))))))

(comp_if
 (($if424 test_nocond $nt425)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3) `(,($ 2) ,@($ 3)) `(,($ 2)))))))

(encoding_decl
 ((NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(yield_expr
 (($yield426 $nt427)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) ($ 2) '(Yield))))))

(yield_arg
 (($from428 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(YieldFrom ,($ 2)))))
 ((testlist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) `(Yield ,($ 1)))))))

($rep60
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,429| test $rep60)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep59
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,430| nt58 $rep59)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

(nt58
 ((test $:431 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) `(,($ 1) ,($ 3))))))

($rep57
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,432| subscript $rep57)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep56
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,433| nt55 $rep56)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

(nt55
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($rep54
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,434| with_item $rep54)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep53
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,435| test $rep53)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep52
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,436| NAME $rep52)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep51
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,437| NAME $rep51)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep50
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 (($.438 NAME $rep50)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep49
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,439| dotted_as_name $rep49)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep48
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,440| import_as_name $rep48)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

($rep47
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$,441| nt46 $rep47)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

(nt46
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($rep45
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '())))
 ((|$;442| small_stmt $rep45)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 2) ($ 3))))))

(|$;442|
 ((|;|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ";"))))

(|$,441|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,440|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,439|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($.438
 ((|.|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "."))))

(|$,437|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,436|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,435|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,434|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,433|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,432|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($:431
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

(|$,430|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

(|$,429|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($from428
 ((from)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "from"))))

($nt427
 ((yield_arg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($yield426
 ((yield)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "yield"))))

($nt425
 ((comp_iter)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($if424
 ((if)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "if"))))

($nt423
 ((comp_iter)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($in422
 ((in)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "in"))))

($for421
 ((for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "for"))))

($=420
 ((=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "="))))

($nt419
 ((comp_for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt401
 (($nt402 $nt406)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (coalesce ($ 2) ($ 1))))))

($nt406
 ((argument $nt407)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(() (,($ 1)) ()))))
 (($*409 test $nt410 $nt414)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      (if ($ 4) `(,($ 2) ,($ 3) ,($ 4)) `(,($ 2) ,($ 3) ())))))
 (($**418 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) `(() () ,($ 2))))))

($**418
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

($nt414
 (($nt415)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt415
 ((|$,416| $**417 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($**417
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

(|$,416|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt410
 (($nt411)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt411
 (($nt412 $nt411)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt412
 ((|$,413| argument)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

(|$,413|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($*409
 ((*)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*"))))

($nt407
 ((|$,408|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,408|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt402
 (($nt403)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt403
 (($nt404 $nt403)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt404
 ((argument |$,405|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 1)))))

(|$,405|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($:400
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt395
 (($nt396)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt396
 ((|$(397| $nt398 |$)399|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 2) ($ 2) `((bases) (keywords) (starargs #f) (kwargs #f)))))))

(|$)399|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

($nt398
 ((arglist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$(397|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

($class394
 ((class)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "class"))))

($nt391
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((test $rep60 $nt392)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt392
 ((|$,393|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,393|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt387
 (($nt388)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (recombine (car $$))))))

($nt388
 ((nt58)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((nt58 $rep59 $nt389)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt389
 ((|$,390|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,390|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($:386
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt384
 ((|$,385|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,385|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt380
 (($nt381)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt381
 (($nt382 $nt381)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt382
 ((|$,383| test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

(|$,383|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt378
 ((|$,379|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,379|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt373
 (($nt374)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt374
 (($nt375 $nt374)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt375
 ((|$,376| $nt377)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($nt377
 ((expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(|$,376|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt372
 ((expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt371
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($:370
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt369
 ((sliceop)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($:368
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt367
 ((sliceop)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($:366
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt365
 ((sliceop)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($:364
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt363
 ((sliceop)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($:362
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt359
 ((subscript)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((subscript $rep57 $nt360)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt360
 ((|$,361|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,361|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($.358
 ((|.|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "."))))

(|$]357|
 ((|]|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "]"))))

(|$[356|
 ((|[|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "["))))

(|$)355|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

(|$(354|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

(|$)353|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

(|$(352|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

($nt349
 ((nt55)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((nt55 $rep56 $nt350)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt350
 ((|$,351|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,351|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt348
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($False347
 ((False)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "False"))))

($True346
 ((True)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "True"))))

($None345
 ((None)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "None"))))

($...344
 ((...)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "..."))))

($nt343
 ((STRING $nt343)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 ((STRING)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

(|$}342|
 ((|}|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "}"))))

(|${341|
 ((|{|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "{"))))

(|$}340|
 ((|}|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "}"))))

(|${339|
 ((|{|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "{"))))

(|$]338|
 ((|]|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "]"))))

(|$[337|
 ((|[|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "["))))

(|$]336|
 ((|]|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "]"))))

(|$[335|
 ((|[|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "["))))

(|$)334|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

(|$(333|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

(|$)332|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

($nt331
 ((yield_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((testlist_comp)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(|$(330|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

($nt327
 (($nt328)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt328
 (($**329 factor)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($**329
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

($nt326
 ((trailer $nt326)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt322
 (($+323)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'UAdd))))
 (($-324)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'USub))))
 (($~325)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Invert)))))

($~325
 ((~)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "~"))))

($-324
 ((-)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "-"))))

($+323
 ((+)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "+"))))

($nt317
 (($*318)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Mult))))
 (($/319)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Div))))
 (($%320)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Mod))))
 (($//321)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'FloorDiv)))))

($//321
 ((//)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "//"))))

($%320
 ((%)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "%"))))

($/319
 ((/)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "/"))))

($*318
 ((*)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*"))))

($nt314
 (($+315)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Add))))
 (($-316)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Sub)))))

($-316
 ((-)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "-"))))

($+315
 ((+)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "+"))))

($nt311
 (($<<312)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'LShift))))
 (($>>313)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'RShift)))))

($>>313
 ((>>)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ">>"))))

($<<312
 ((<<)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "<<"))))

($nt309
 (($&310)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'BitAnd)))))

($&310
 ((&)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "&"))))

($nt307
 (($^308)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'BitXor)))))

($^308
 ((^)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "^"))))

($nt305
 (($\|306)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'BitOr)))))

($\|306
 ((\|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "|"))))

($*304
 ((*)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*"))))

($nt291
 (($<292)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Lt))))
 (($>293)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Gt))))
 (($==294)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Eq))))
 (($>=295)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'GtE))))
 (($<=296)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'LtE))))
 (($!=297)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'NotEq))))
 (($in298)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'In))))
 (($not299 $in300)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) 'NotIn)))
 (($is301)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) 'Is))))
 (($is302 $not303)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) 'IsNot))))

($not303
 ((not)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "not"))))

($is302
 ((is)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "is"))))

($is301
 ((is)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "is"))))

($in300
 ((in)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "in"))))

($not299
 ((not)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "not"))))

($in298
 ((in)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "in"))))

($!=297
 ((!=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "!="))))

($<=296
 ((<=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "<="))))

($>=295
 ((>=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ">="))))

($==294
 ((==)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "=="))))

($>293
 ((>)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ">"))))

($<292
 ((<)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "<"))))

($not290
 ((not)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "not"))))

($and289
 ((and)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "and"))))

($or288
 ((or)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "or"))))

($:287
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt286
 ((varargslist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($lambda285
 ((lambda)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "lambda"))))

($:284
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt283
 ((varargslist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($lambda282
 ((lambda)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "lambda"))))

($nt278
 (($nt279)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt279
 (($if280 or_test $else281 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(,($ 2) ,($ 4))))))

($else281
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($if280
 ((if)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "if"))))

($nt277
 ((stmt $nt277)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 ((stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt272
 (($nt273)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt273
 ((test $nt274)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,($ 1) ,($ 2)) `(,($ 1) #f))))))

($nt274
 (($nt275)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt275
 (($as276 NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (string->symbol ($ 2))))))

($as276
 ((as)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "as"))))

($except271
 ((except)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "except"))))

($nt268
 (($nt269)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt269
 (($as270 expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($as270
 ((as)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "as"))))

($:267
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt266
 ((with_item)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((with_item $rep54)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2))))))

($with265
 ((with)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "with"))))

($nt238
 (($nt239 $nt242 $nt245)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 3) `(,($ 1) ,($ 2) ,($ 3)) `(,($ 1) ,($ 2) ())))))
 (($nt249 $nt252)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,($ 1) ,($ 2) ()) `(,($ 1) () ())))))
 (($nt256 $nt259)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,($ 1) () ,($ 2)) `(,($ 1) () ())))))
 (($finally263 $:264 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) `(() () ,($ 3))))))

($:264
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($finally263
 ((finally)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "finally"))))

($nt259
 (($nt260)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt260
 (($finally261 $:262 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:262
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($finally261
 ((finally)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "finally"))))

($nt256
 (($nt257 $nt256)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (($nt257)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt257
 ((except_clause $:258 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(except ,@($ 1) ,@($ 3))))))

($:258
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt252
 (($nt253)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt253
 (($else254 $:255 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:255
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($else254
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($nt249
 (($nt250 $nt249)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (($nt250)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt250
 ((except_clause $:251 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(except ,@($ 1) ,@($ 3))))))

($:251
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt245
 (($nt246)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt246
 (($finally247 $:248 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:248
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($finally247
 ((finally)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "finally"))))

($nt242
 (($else243 $:244 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:244
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($else243
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($nt239
 (($nt240 $nt239)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (($nt240)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt240
 ((except_clause $:241 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      `(except ,@($ 1) ,@($ 3))))))

($:241
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($:237
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($try236
 ((try)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "try"))))

($nt232
 (($nt233)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt233
 (($else234 $:235 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:235
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($else234
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($:231
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($in230
 ((in)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "in"))))

($for229
 ((for)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "for"))))

($nt225
 (($nt226)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt226
 (($else227 $:228 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:228
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($else227
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($:224
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($while223
 ((while)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "while"))))

($nt200
 (($if201 test $:202 suite $nt203)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5)))))
      (if ($ 5)
        `(,($ 2) ,@($ 4) ,@(process-if '() ($ 5)))
        `(,($ 2) ,@($ 4))))))
 (($if207 test $:208 suite $nt209 $nt213)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6)))))
      (if ($ 6)
        `(,($ 2) ,@($ 4) ,@(process-if ($ 6) ($ 5)))
        `(,($ 2) ,@($ 4) ,@(process-if '() ($ 5)))))))
 (($if217 test $:218 suite $nt219)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5)))))
      (if ($ 5) `(,($ 2) ,@($ 4) ,($ 5)) `(,($ 2) ,@($ 4) ()))))))

($nt219
 (($nt220)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt220
 (($else221 $:222 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:222
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($else221
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($:218
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($if217
 ((if)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "if"))))

($nt213
 (($nt214)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt214
 (($else215 $:216 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($:216
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($else215
 ((else)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "else"))))

($nt209
 (($nt210 $nt209)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (($nt210)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt210
 (($elif211 test $:212 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(,($ 2) ,($ 4))))))

($:212
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($elif211
 ((elif)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "elif"))))

($:208
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($if207
 ((if)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "if"))))

($nt203
 (($nt204 $nt203)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt204
 (($elif205 test $:206 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4)))))
      `(,($ 2) ,($ 4))))))

($:206
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($elif205
 ((elif)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "elif"))))

($:202
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($if201
 ((if)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "if"))))

($nt199
 ((test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((test $rep53)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2))))))

($assert198
 ((assert)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "assert"))))

($nt197
 ((NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((NAME $rep52)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2))))))

($nonlocal196
 ((nonlocal)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "nonlocal"))))

($nt195
 ((NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((NAME $rep51)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2))))))

($global194
 ((global)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "global"))))

($nt193
 ((NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((NAME $rep50)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2))))))

($nt192
 ((dotted_as_name)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((dotted_as_name $rep49)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2))))))

($nt189
 ((import_as_name)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((import_as_name $rep48 $nt190)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt190
 ((|$,191|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,191|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt186
 (($nt187)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt187
 (($as188 NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (string->symbol ($ 2))))))

($as188
 ((as)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "as"))))

($nt183
 (($nt184)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt184
 (($as185 NAME)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (string->symbol ($ 2))))))

($as185
 ((as)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "as"))))

($nt179
 (($*180)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) `(((* #f))))))
 ((|$(181| import_as_names |$)182|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 2))))
 ((import_as_names)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(|$)182|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

(|$(181|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

($*180
 ((*)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*"))))

($import178
 ((import)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "import"))))

($nt169
 (($nt170 dotted_name)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 1)
        `(,(make-dotted ($ 2)) ,(apply + (map string-length (flatten ($ 1)))))
        `(,(make-dotted ($ 2)) 0)))))
 (($nt174)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      `(#f ,(apply + (map string-length (flatten ($ 1)))))))))

($nt174
 (($nt175 $nt174)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (($nt175)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt175
 (($.176)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($...177)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($...177
 ((...)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "..."))))

($.176
 ((|.|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "."))))

($nt170
 (($nt171 $nt170)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt171
 (($.172)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($...173)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($...173
 ((...)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "..."))))

($.172
 ((|.|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "."))))

($from168
 ((from)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "from"))))

($import167
 ((import)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "import"))))

($nt162
 (($nt163)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt163
 ((test $nt164)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,($ 1) ,($ 2)) `(,($ 1)))))))

($nt164
 (($nt165)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt165
 (($from166 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($from166
 ((from)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "from"))))

($raise161
 ((raise)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "raise"))))

($nt160
 ((testlist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($return159
 ((return)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "return"))))

($continue158
 ((continue)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "continue"))))

($break157
 ((break)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "break"))))

($pass156
 ((pass)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "pass"))))

($del155
 ((del)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "del"))))

($//=154
 ((//=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "//="))))

($**=153
 ((**=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**="))))

($>>=152
 ((>>=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ">>="))))

($<<=151
 ((<<=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "<<="))))

($^=150
 ((^=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "^="))))

($\|=149
 ((\|=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "|="))))

($&=148
 ((&=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "&="))))

($%=147
 ((%=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "%="))))

($/=146
 ((/=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "/="))))

($*=145
 ((*=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*="))))

($-=144
 ((-=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "-="))))

($+=143
 ((+=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "+="))))

($nt140
 ((nt46)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((nt46 $rep47 $nt141)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt141
 ((|$,142|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$,142|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt136
 (($nt137 $nt136)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (($nt137)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt137
 (($=138 $nt139)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($nt139
 ((yield_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((testlist_star_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($=138
 ((=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "="))))

($nt135
 ((yield_expr)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((testlist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt132
 ((small_stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1)))))
 ((small_stmt $rep45 $nt133)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (cons ($ 1) ($ 2))))))

($nt133
 ((|$;134|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$;134|
 ((|;|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ";"))))

($**131
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

($nt128
 (($nt129)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt129
 (($=130 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($=130
 ((=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "="))))

(|$,127|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt123
 (($nt124)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt124
 ((|$,125| $**126 vfpdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($**126
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

(|$,125|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt121
 (($nt122)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        (recombine-arglist $$))))))

($nt122
 ((kwonly $nt122)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($*120
 ((*)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*"))))

($nt107
 (($nt108 $nt115)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        `((,@(car ($ 1)))
          ()
          ,(list-ref ($ 2) 2)
          ,(list-ref ($ 2) 3)
          ,(list-ref ($ 2) 4)
          ,(list-ref ($ 2) 5)
          ,(list-ref ($ 2) 6)
          (,@(cadr ($ 1))))
        `((,@(car ($ 1))) (,($ 2)) (#f) () () () (#f) (,@(cadr ($ 1))))))))
 ((arg-kwonly-kwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((kwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt115
 (($nt116)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt116
 ((|$,117| $nt118)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) ($ 2) `(() () () () () () () ()))))))

($nt118
 (($nt119)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt119
 ((arg-kwonly-kwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((kwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(|$,117|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt108
 (($nt109 $nt113)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        (recombine-arglist `(,($ 1) ,@($ 2)))
        (recombine-arglist `(($ 1))))))))

($nt113
 (($nt114)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt114
 ((kwonly $nt114)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt109
 ((vfpdef $nt110)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(,($ 1) ,($ 2) #f) `(,($ 1) #f #f))))))

($nt110
 (($nt111)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt111
 (($=112 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($=112
 ((=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "="))))

($nt104
 (($nt105)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt105
 (($:106 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($:106
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($**103
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

($nt100
 (($nt101)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt101
 (($=102 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($=102
 ((=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "="))))

(|$,99|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt95
 (($nt96)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt96
 ((|$,97| $**98 tfpdef)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) ($ 3)))))

($**98
 ((**)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "**"))))

(|$,97|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt93
 (($nt94)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        (recombine-arglist $$))))))

($nt94
 ((tkwonly $nt94)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($*92
 ((*)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "*"))))

($nt79
 (($nt80 $nt87)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        `((,@(car ($ 1)))
          (,@(caddr ($ 1)))
          ,(list-ref ($ 2) 2)
          ,(list-ref ($ 2) 3)
          ,(list-ref ($ 2) 4)
          ,(list-ref ($ 2) 5)
          ,(list-ref ($ 2) 6)
          (,@(cadr ($ 1))))
        `((,@(car ($ 1)))
          (,@(caddr ($ 1)))
          (#f)
          ()
          ()
          ()
          (#f)
          (,@(cadr ($ 1))))))))
 ((targ-kwonly-kwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((tkwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt87
 (($nt88)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt88
 ((|$,89| $nt90)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) ($ 2) `(() () (#f) () () () (#f) ()))))))

($nt90
 (($nt91)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt91
 ((targ-kwonly-kwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((tkwarg)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(|$,89|
 ((|,|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ","))))

($nt80
 (($nt81 $nt85)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        (recombine-arglist `(,($ 1) ,@($ 2)))
        (recombine-arglist `(,($ 1))))))))

($nt85
 (($nt86)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt86
 ((tkwonly $nt86)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt81
 ((tfpdef $nt82)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2)
        `(,(car ($ 1)) ,($ 2) ,(cadr ($ 1)))
        `(,(car ($ 1)) #f ,(cadr ($ 1))))))))

($nt82
 (($nt83)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt83
 (($=84 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($=84
 ((=)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "="))))

(|$)78|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

($nt77
 ((typedargslist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$(76|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

($:75
 ((:)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ":"))))

($nt72
 (($nt73)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt73
 (($->74 test)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) ($ 2)))))

($->74
 ((->)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "->"))))

($def71
 ((def)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "def"))))

($nt70
 ((decorator $nt70)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 ((decorator)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt65
 (($nt66)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

($nt66
 ((|$(67| $nt68 |$)69|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3)))))
      (if ($ 2) ($ 2) `((bases) (keywords) (starargs #f) (kwargs #f)))))))

(|$)69|
 ((|)|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ")"))))

($nt68
 ((arglist)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) #f))))

(|$(67|
 ((|(|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "("))))

($@64
 ((@)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) "@"))))

($nt63
 ((NEWLINE $nt63)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt61
 (($nt62 $nt61)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) (cons ($ 1) ($ 2)))))
 (()
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list)))) '()))))

($nt62
 ((NEWLINE)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((stmt)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

) ; grammar

) ; parser

) ; pyparse


(define (token-generator)
  (define next (read))

  (match next
    [(? eof-object?)
      (token-EOF)]

    ; ID:
    [`(ID ,name)
      (token-NAME name)]

    ; KEYWORD:
    [`(KEYWORD ,symbol)
      symbol]

    ; PUNCT:
    [`(PUNCT ,p)
      ; BUG: This will fail if we ever include positional info:
      (string->symbol p)]

    ; SPECIAL:
    ['(NEWLINE)
      (token-NEWLINE)]

    ['(INDENT)
      (token-INDENT)]

    ['(DEDENT)
      (token-DEDENT)]

    ['(ENDMARKER)
      (token-ENDMARKER)]

    ; LIT:
    [`(LIT ,(and value (? number?)))
      (token-NUMBER value)]

    [`(LIT ,(and value (or (? bytes?) (? string?))))
      (token-STRING value)]

      ))

(pretty-write (pyparse token-generator))

