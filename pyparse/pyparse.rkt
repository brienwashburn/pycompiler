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

(define (process-trailers base trailers)
  (match trailers 
    ['()
     base]

    [(cons (list op exp) rest)
     (process-trailers `(,(string->symbol op) ,base ,exp) rest)]))

(define (process-if base lst)
    (match lst
    ['()
      `(,base)]

    [(cons (list tst bdy) rest)
     (process-if `((If (test ,tst) (body ,@bdy) (orelse ,@base))) rest)]))

(define (recombine-arglist tups)
 (define (ra-recurs tups args defaults types)
  (cond
   [(empty? tups) (list args defaults types)]
   [else (ra-recurs (cdr tups) (append args (list (caar tups))) (append defaults (list (cadar tups))) (append types (list (caddar tups))))]))
 (ra-recurs tups '() '() '()))


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
   (start nonlocal_stmt)
   
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
 (($nt57 ENDMARKER)
  (let-syntax (($
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

(eval_input
 ((testlist $nt59 ENDMARKER)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

(decorator
 (($@60 dotted_name $nt61 NEWLINE)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$))))

(decorators
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(decorated
 ((decorators $nt67)
  (let-syntax (($
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

(funcdef
 (($def68 NAME parameters $nt69 $:72 suite)
  (let-syntax (($
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
      $$))))

(parameters
 ((|$(73| $nt74 |$)75|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

(typedargslist
 (($nt76)
  (let-syntax (($
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
        `(Argument
          ,`(args ,@($ 0))
          ,`(args-types ,@($ 1))
          ,`(varargs ,@($ 2))
          ,`(kwonlyargs ,@($ 3))
          ,`(kwonlyarg-types ,@($ 4))
          ,`(kw_defaults ,@($ 5))
          ,`(kwarg ,@($ 6))
          ,`(defaults ,@($ 7))))))))

(targ-kwonly-kwarg
 (($*89 tfpdef $nt90 $nt92)
  (let-syntax (($
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
 ((|$,96| tfpdef $nt97)
  (let-syntax (($
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
 (($**100 tfpdef)
  (let-syntax (($
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
 ((NAME $nt101)
  (let-syntax (($
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
 (($nt104)
  (let-syntax (($
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
          ,`(args-types ,@($ 1))
          ,`(varargs ,@($ 2))
          ,`(kwonlyargs ,@($ 3))
          ,`(kwonlyarg-types ,@($ 4))
          ,`(kw_defaults ,@($ 5))
          ,`(kwarg ,@($ 6))
          ,`(defaults ,@($ 7))))))))

(arg-kwonly-kwarg
 (($*117 vfpdef $nt118 $nt120)
  (let-syntax (($
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
 ((|$,124| vfpdef $nt125)
  (let-syntax (($
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
 (($**128 vfpdef)
  (let-syntax (($
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
 (($nt129 NEWLINE)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

(testlist_star_expr
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
 ((nt46 $rep47 $nt132)
  (let-syntax (($
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

(augassign
 (($+=134)
  (let-syntax (($
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
 (($-=135)
  (let-syntax (($
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
 (($*=136)
  (let-syntax (($
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
 (($/=137)
  (let-syntax (($
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
 (($%=138)
  (let-syntax (($
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
 (($&=139)
  (let-syntax (($
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
 (($\|=140)
  (let-syntax (($
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
 (($^=141)
  (let-syntax (($
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
 (($<<=142)
  (let-syntax (($
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
 (($>>=143)
  (let-syntax (($
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
 (($**=144)
  (let-syntax (($
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
 (($//=145)
  (let-syntax (($
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
 (($del146 exprlist)
  (let-syntax (($
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
 (($pass147)
  (let-syntax (($
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
 (($break148)
  (let-syntax (($
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
 (($continue149)
  (let-syntax (($
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
 (($return150 $nt151)
  (let-syntax (($
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
 (($raise152 $nt153)
  (let-syntax (($
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
 (($import158 dotted_as_names)
  (let-syntax (($
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

(import_from
 (($from159 $nt160 $import169 $nt170)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$))))

(import_as_name
 ((NAME $nt174)
  (let-syntax (($
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

(dotted_as_name
 ((dotted_name $nt177)
  (let-syntax (($
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

(import_as_names
 ((import_as_name $nt180 $nt183)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

(dotted_as_names
 ((dotted_as_name $nt185)
  (let-syntax (($
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

(dotted_name
 ((NAME $nt188)
  (let-syntax (($
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

(global_stmt
 (($global191 NAME $nt192)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

(nonlocal_stmt
 (($nonlocal195 $nt196)
  (let-syntax (($
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
      `(Nonlocal ,(apply string->symbol (($ 2))))))))

(assert_stmt
 (($assert197 $nt198)
  (let-syntax (($
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
 (($nt199)
  (let-syntax (($
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
 (($while222 test $:223 suite $nt224)
  (let-syntax (($
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
 (($for228 exprlist $in229 testlist $:230 suite $nt231)
  (let-syntax (($
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
 (($try235 $:236 suite $nt237)
  (let-syntax (($
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
 (($with264 $nt265 $:266 suite)
  (let-syntax (($
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
 ((test $nt267)
  (let-syntax (($
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
 (($except270 $nt271)
  (let-syntax (($
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
 ((NEWLINE INDENT $nt276 DEDENT)
  (let-syntax (($
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
 ((or_test $nt277)
  (let-syntax (($
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
 (($lambda281 $nt282 $:283 test)
  (let-syntax (($
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
 (($lambda284 $nt285 $:286 test_nocond)
  (let-syntax (($
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
 ((or_test $or287 and_test)
  (let-syntax (($
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
 ((and_test $and288 not_test)
  (let-syntax (($
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
 (($not289 not_test)
  (let-syntax (($
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
 (($nt290)
  (let-syntax (($
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
 (($*303 expr)
  (let-syntax (($
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
 ((expr $nt304 xor_expr)
  (let-syntax (($
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
 ((xor_expr $nt306 and_expr)
  (let-syntax (($
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
 ((and_expr $nt308 shift_expr)
  (let-syntax (($
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
 ((shift_expr $nt310 arith_expr)
  (let-syntax (($
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
 ((arith_expr $nt313 term)
  (let-syntax (($
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
 ((term $nt316 factor)
  (let-syntax (($
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
 (($nt321 factor)
  (let-syntax (($
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
 ((atom $nt325 $nt326)
  (let-syntax (($
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
 ((|$(329| $nt330 |$)331|)
  (let-syntax (($
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
 ((|$(332| |$)333|)
  (let-syntax (($
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
 ((|$[334| testlist_comp |$]335|)
  (let-syntax (($
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
 ((|$[336| |$]337|)
  (let-syntax (($
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
 ((|${338| dictorsetmaker |$}339|)
  (let-syntax (($
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
 ((|${340| |$}341|)
  (let-syntax (($
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
 (($nt342)
  (let-syntax (($
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
 (($...343)
  (let-syntax (($
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
 (($None344)
  (let-syntax (($
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
 (($True345)
  (let-syntax (($
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
 (($False346)
  (let-syntax (($
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
 (($nt347 comp_for)
  (let-syntax (($
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
      `(ListComp ,($ 1) ,($ 2)))))
 (($nt348 $nt349 $nt351)
  (let-syntax (($
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
      (if ($ 3) `(,($ 1) ,@($ 3)) `(,($ 1)))))))

(trailer
 ((|$(354| arglist |$)355|)
  (let-syntax (($
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
 ((|$(356| |$)357|)
  (let-syntax (($
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
 ((|$[358| subscriptlist |$]359|)
  (let-syntax (($
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
 (($.360 NAME)
  (let-syntax (($
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
 (($nt361)
  (let-syntax (($
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
 ((test $:364 test $nt365)
  (let-syntax (($
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
 ((test $:366 $nt367)
  (let-syntax (($
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
 (($:368 test $nt369)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2)))))
      (if ($ 2) `(Slice #f #f ,($ 2)) `(Slice #f #f #f))))))

(sliceop
 (($:372 $nt373)
  (let-syntax (($
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
 (($nt374 $nt375 $nt380)
  (let-syntax (($
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
 ((test $nt382 $nt386)
  (let-syntax (($
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
 ((test $:388 test comp_for)
  (let-syntax (($
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
 (($nt389)
  (let-syntax (($
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
 (($nt393)
  (let-syntax (($
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
 (($class396 NAME $nt397 $:402 suite)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5))))) $$))))

(arglist
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
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        `(,`(args ,@($ 0))
          ,`(keywords ,@($ 1))
          ,`(starargs ,@($ 2))
          ,`(kwargs ,@($ 3))))))))

(argument
 ((test $nt421)
  (let-syntax (($
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
 ((test $=422 test)
  (let-syntax (($
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
 (($for423 exprlist $in424 or_test $nt425)
  (let-syntax (($
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
 (($if426 test_nocond $nt427)
  (let-syntax (($
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
 (($yield428 $nt429)
  (let-syntax (($
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
      (if ($ 2) ($ 2) (`Yield))))))

(yield_arg
 (($from430 test)
  (let-syntax (($
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
 ((|$,431| test $rep56)
  (let-syntax (($
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

($rep55
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
 ((|$,432| nt54 $rep55)
  (let-syntax (($
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

(nt54
 ((test $:433 test)
  (let-syntax (($
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
 ((|$,434| subscript $rep53)
  (let-syntax (($
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
 ((|$,435| nt51 $rep52)
  (let-syntax (($
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

(nt51
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
 ((|$,436| with_item $rep50)
  (let-syntax (($
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
 ((|$,437| test $rep49)
  (let-syntax (($
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
 ((|$,438| NAME $rep48)
  (let-syntax (($
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
 ((|$,439| nt46 $rep47)
  (let-syntax (($
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
 ((|$;440| small_stmt $rep45)
  (let-syntax (($
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

(|$;440|
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

(|$,438|
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

($:433
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

(|$,431|
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

($from430
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

($nt429
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

($yield428
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

($nt427
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

($if426
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

($in424
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

($for423
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

($=422
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

($nt421
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

($nt403
 (($nt404 $nt408)
  (let-syntax (($
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

($nt408
 ((argument $nt409)
  (let-syntax (($
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
 (($*411 test $nt412 $nt416)
  (let-syntax (($
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
 (($**420 test)
  (let-syntax (($
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

($**420
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

($nt416
 (($nt417)
  (let-syntax (($
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

($nt417
 ((|$,418| $**419 test)
  (let-syntax (($
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

($**419
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

(|$,418|
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

($nt412
 (($nt413)
  (let-syntax (($
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

($nt413
 (($nt414 $nt413)
  (let-syntax (($
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

($nt414
 ((|$,415| argument)
  (let-syntax (($
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

(|$,415|
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

($*411
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

($nt409
 ((|$,410|)
  (let-syntax (($
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

(|$,410|
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

($nt404
 (($nt405)
  (let-syntax (($
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

($nt405
 (($nt406 $nt405)
  (let-syntax (($
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

($nt406
 ((argument |$,407|)
  (let-syntax (($
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

(|$,407|
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

($:402
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

($nt397
 (($nt398)
  (let-syntax (($
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

($nt398
 ((|$(399| $nt400 |$)401|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

(|$)401|
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

($nt400
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

(|$(399|
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

($class396
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

($nt393
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
 ((test $rep56 $nt394)
  (let-syntax (($
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

($nt394
 ((|$,395|)
  (let-syntax (($
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

(|$,395|
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

($nt389
 (($nt390)
  (let-syntax (($
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

($nt390
 ((nt54)
  (let-syntax (($
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
 ((nt54 $rep55 $nt391)
  (let-syntax (($
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

($nt391
 ((|$,392|)
  (let-syntax (($
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

(|$,392|
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

($:388
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

($nt386
 ((|$,387|)
  (let-syntax (($
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

(|$,387|
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

($nt382
 (($nt383)
  (let-syntax (($
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

($nt383
 (($nt384 $nt383)
  (let-syntax (($
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

($nt384
 ((|$,385| test)
  (let-syntax (($
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
 ((|$,381|)
  (let-syntax (($
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

(|$,381|
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

($nt375
 (($nt376)
  (let-syntax (($
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

($nt376
 (($nt377 $nt376)
  (let-syntax (($
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

($nt377
 ((|$,378| $nt379)
  (let-syntax (($
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

($nt379
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

(|$,378|
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

($nt374
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

($nt373
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

($:372
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

($nt371
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

($nt361
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
 ((subscript $rep53 $nt362)
  (let-syntax (($
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

($nt362
 ((|$,363|)
  (let-syntax (($
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

(|$,363|
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

($.360
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

(|$]359|
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

(|$[358|
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

(|$)357|
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

(|$(356|
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

($nt351
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
 ((nt51)
  (let-syntax (($
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
 ((nt51 $rep52 $nt352)
  (let-syntax (($
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

($nt352
 ((|$,353|)
  (let-syntax (($
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

(|$,353|
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

($nt349
 ((|$,350|)
  (let-syntax (($
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

(|$,350|
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

($nt347
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

($False346
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

($True345
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

($None344
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

($...343
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

($nt342
 ((STRING $nt342)
  (let-syntax (($
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

(|$}341|
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

(|${340|
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

(|$}339|
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

(|${338|
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

(|$]337|
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

(|$[336|
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

(|$]335|
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

(|$[334|
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

(|$)333|
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

(|$(332|
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

(|$)331|
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

($nt330
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

(|$(329|
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

($nt326
 (($nt327)
  (let-syntax (($
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

($nt327
 (($**328 factor)
  (let-syntax (($
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

($**328
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

($nt325
 ((trailer $nt325)
  (let-syntax (($
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

($nt321
 (($+322)
  (let-syntax (($
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
 (($-323)
  (let-syntax (($
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
 (($~324)
  (let-syntax (($
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

($~324
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

($-323
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

($+322
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

($nt316
 (($*317)
  (let-syntax (($
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
 (($/318)
  (let-syntax (($
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
 (($%319)
  (let-syntax (($
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
 (($//320)
  (let-syntax (($
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

($//320
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

($%319
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

($/318
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

($*317
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

($nt313
 (($+314)
  (let-syntax (($
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
 (($-315)
  (let-syntax (($
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

($-315
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

($+314
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

($nt310
 (($<<311)
  (let-syntax (($
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
 (($>>312)
  (let-syntax (($
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

($>>312
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

($<<311
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

($nt308
 (($&309)
  (let-syntax (($
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

($&309
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

($nt306
 (($^307)
  (let-syntax (($
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

($^307
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

($nt304
 (($\|305)
  (let-syntax (($
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

($\|305
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

($*303
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

($nt290
 (($<291)
  (let-syntax (($
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
 (($>292)
  (let-syntax (($
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
 (($==293)
  (let-syntax (($
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
 (($>=294)
  (let-syntax (($
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
 (($<=295)
  (let-syntax (($
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
 (($!=296)
  (let-syntax (($
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
 (($in297)
  (let-syntax (($
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
 (($not298 $in299)
  (let-syntax (($
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
 (($is300)
  (let-syntax (($
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
 (($is301 $not302)
  (let-syntax (($
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

($not302
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

($is300
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

($in299
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

($not298
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

($in297
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

($!=296
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

($<=295
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

($>=294
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

($==293
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

($>292
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

($<291
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

($not289
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

($and288
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

($or287
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

($:286
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

($nt285
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

($lambda284
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

($:283
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

($nt282
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

($lambda281
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

($nt277
 (($nt278)
  (let-syntax (($
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

($nt278
 (($if279 or_test $else280 test)
  (let-syntax (($
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

($else280
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

($if279
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

($nt276
 ((stmt $nt276)
  (let-syntax (($
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

($nt271
 (($nt272)
  (let-syntax (($
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

($nt272
 ((test $nt273)
  (let-syntax (($
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

($nt273
 (($nt274)
  (let-syntax (($
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

($nt274
 (($as275 NAME)
  (let-syntax (($
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

($as275
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

($except270
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

($nt267
 (($nt268)
  (let-syntax (($
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

($nt268
 (($as269 expr)
  (let-syntax (($
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

($as269
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

($:266
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

($nt265
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
 ((with_item $rep50)
  (let-syntax (($
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

($with264
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

($nt237
 (($nt238 $nt241 $nt244)
  (let-syntax (($
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
 (($nt248 $nt251)
  (let-syntax (($
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
 (($nt255 $nt258)
  (let-syntax (($
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
 (($finally262 $:263 suite)
  (let-syntax (($
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

($:263
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

($finally262
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

($nt258
 (($nt259)
  (let-syntax (($
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

($nt259
 (($finally260 $:261 suite)
  (let-syntax (($
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

($:261
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

($finally260
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

($nt255
 (($nt256 $nt255)
  (let-syntax (($
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
 (($nt256)
  (let-syntax (($
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

($nt256
 ((except_clause $:257 suite)
  (let-syntax (($
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

($:257
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

($nt251
 (($nt252)
  (let-syntax (($
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

($nt252
 (($else253 $:254 suite)
  (let-syntax (($
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

($:254
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

($else253
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

($nt248
 (($nt249 $nt248)
  (let-syntax (($
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
 (($nt249)
  (let-syntax (($
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

($nt249
 ((except_clause $:250 suite)
  (let-syntax (($
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

($:250
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

($nt244
 (($nt245)
  (let-syntax (($
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

($nt245
 (($finally246 $:247 suite)
  (let-syntax (($
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

($:247
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

($finally246
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

($nt241
 (($else242 $:243 suite)
  (let-syntax (($
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

($:243
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

($else242
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

($nt238
 (($nt239 $nt238)
  (let-syntax (($
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
 (($nt239)
  (let-syntax (($
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

($nt239
 ((except_clause $:240 suite)
  (let-syntax (($
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

($:240
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

($:236
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

($try235
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

($nt231
 (($nt232)
  (let-syntax (($
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

($nt232
 (($else233 $:234 suite)
  (let-syntax (($
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

($:234
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

($else233
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

($:230
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

($in229
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

($for228
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

($nt224
 (($nt225)
  (let-syntax (($
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

($nt225
 (($else226 $:227 suite)
  (let-syntax (($
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

($:227
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

($else226
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

($:223
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

($while222
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

($nt199
 (($if200 test $:201 suite $nt202)
  (let-syntax (($
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
 (($if206 test $:207 suite $nt208 $nt212)
  (let-syntax (($
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
 (($if216 test $:217 suite $nt218)
  (let-syntax (($
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

($nt218
 (($nt219)
  (let-syntax (($
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

($nt219
 (($else220 $:221 suite)
  (let-syntax (($
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

($:221
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

($else220
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

($:217
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

($if216
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

($nt212
 (($nt213)
  (let-syntax (($
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

($nt213
 (($else214 $:215 suite)
  (let-syntax (($
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

($:215
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

($else214
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

($nt208
 (($nt209 $nt208)
  (let-syntax (($
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
 (($nt209)
  (let-syntax (($
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

($nt209
 (($elif210 test $:211 suite)
  (let-syntax (($
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

($:211
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

($elif210
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

($:207
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

($if206
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

($nt202
 (($nt203 $nt202)
  (let-syntax (($
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

($nt203
 (($elif204 test $:205 suite)
  (let-syntax (($
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

($:205
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

($elif204
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

($:201
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

($if200
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

($nt198
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
 ((test $rep49)
  (let-syntax (($
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

($assert197
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

($nt196
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
 ((NAME $rep48)
  (let-syntax (($
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

($nonlocal195
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

($nt192
 (($nt193 $nt192)
  (let-syntax (($
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

($nt193
 ((|$,194| NAME)
  (let-syntax (($
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

(|$,194|
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

($global191
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

($nt188
 (($nt189 $nt188)
  (let-syntax (($
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

($nt189
 (($.190 NAME)
  (let-syntax (($
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

($.190
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

($nt185
 (($nt186 $nt185)
  (let-syntax (($
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

($nt186
 ((|$,187| dotted_as_name)
  (let-syntax (($
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

(|$,187|
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

($nt183
 ((|$,184|)
  (let-syntax (($
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

(|$,184|
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

($nt180
 (($nt181 $nt180)
  (let-syntax (($
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

($nt181
 ((|$,182| import_as_name)
  (let-syntax (($
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

(|$,182|
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

($nt177
 (($nt178)
  (let-syntax (($
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

($nt178
 (($as179 NAME)
  (let-syntax (($
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

($as179
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

($nt174
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

($nt175
 (($as176 NAME)
  (let-syntax (($
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

($as176
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

($nt170
 (($*171)
  (let-syntax (($
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
 ((|$(172| import_as_names |$)173|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$)))
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

(|$)173|
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

(|$(172|
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

($*171
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

($import169
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

($nt160
 (($nt161 dotted_name)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) $$)))
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt165
 (($nt166 $nt165)
  (let-syntax (($
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
 (($nt166)
  (let-syntax (($
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

($nt166
 (($.167)
  (let-syntax (($
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
 (($...168)
  (let-syntax (($
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

($...168
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

($.167
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

($nt161
 (($nt162 $nt161)
  (let-syntax (($
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

($nt162
 (($.163)
  (let-syntax (($
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
 (($...164)
  (let-syntax (($
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

($...164
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

($.163
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

($from159
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

($import158
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

($nt153
 (($nt154)
  (let-syntax (($
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

($nt154
 ((test $nt155)
  (let-syntax (($
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

($nt155
 (($nt156)
  (let-syntax (($
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

($nt156
 (($from157 test)
  (let-syntax (($
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

($from157
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

($raise152
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

($nt151
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

($return150
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

($continue149
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

($break148
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

($pass147
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

($del146
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

($//=145
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

($**=144
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

($>>=143
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

($<<=142
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

($^=141
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

($\|=140
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

($&=139
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

($%=138
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

($/=137
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

($*=136
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

($-=135
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

($+=134
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

($nt132
 ((|$,133|)
  (let-syntax (($
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

(|$,133|
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

($nt129
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
 ((small_stmt $rep45 $nt130)
  (let-syntax (($
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

($nt130
 ((|$;131|)
  (let-syntax (($
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

(|$;131|
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

($**128
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

($nt125
 (($nt126)
  (let-syntax (($
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

($nt126
 (($=127 test)
  (let-syntax (($
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

($=127
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

(|$,124|
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

($nt120
 (($nt121)
  (let-syntax (($
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

($nt121
 ((|$,122| $**123 vfpdef)
  (let-syntax (($
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

($**123
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

(|$,122|
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
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        (recombine-arglist $$))))))

($nt119
 ((kwonly $nt119)
  (let-syntax (($
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

($*117
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

($nt104
 (($nt105 $nt112)
  (let-syntax (($
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

($nt112
 (($nt113)
  (let-syntax (($
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

($nt113
 ((|$,114| $nt115)
  (let-syntax (($
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

(|$,114|
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

($nt105
 (($nt106 $nt110)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt111
 ((kwonly $nt111)
  (let-syntax (($
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

($nt106
 ((vfpdef $nt107)
  (let-syntax (($
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

($nt107
 (($nt108)
  (let-syntax (($
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

($nt108
 (($=109 test)
  (let-syntax (($
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

($=109
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

($nt101
 (($nt102)
  (let-syntax (($
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

($nt102
 (($:103 test)
  (let-syntax (($
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

($:103
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

($**100
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

($nt97
 (($nt98)
  (let-syntax (($
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

($nt98
 (($=99 test)
  (let-syntax (($
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

($=99
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

(|$,96|
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

($nt92
 (($nt93)
  (let-syntax (($
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

($nt93
 ((|$,94| $**95 tfpdef)
  (let-syntax (($
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

($**95
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

(|$,94|
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
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        (recombine-arglist $$))))))

($nt91
 ((tkwonly $nt91)
  (let-syntax (($
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

($*89
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

($nt76
 (($nt77 $nt84)
  (let-syntax (($
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
          ()
          ()
          ()
          ()
          ()
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

($nt84
 (($nt85)
  (let-syntax (($
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

($nt85
 ((|$,86| $nt87)
  (let-syntax (($
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

(|$,86|
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

($nt77
 (($nt78 $nt82)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1))) $$)))))

($nt83
 ((tkwonly $nt83)
  (let-syntax (($
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

($nt78
 ((tfpdef $nt79)
  (let-syntax (($
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

($nt79
 (($nt80)
  (let-syntax (($
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

($nt80
 (($=81 test)
  (let-syntax (($
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

($=81
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

(|$)75|
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

($nt74
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

(|$(73|
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

($:72
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

($nt69
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

($nt70
 (($->71 test)
  (let-syntax (($
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

($->71
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

($def68
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

($nt67
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt66
 ((decorator $nt66)
  (let-syntax (($
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

($nt61
 (($nt62)
  (let-syntax (($
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

($nt62
 ((|$(63| $nt64 |$)65|)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

(|$)65|
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

($nt64
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

(|$(63|
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

($@60
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

($nt59
 ((NEWLINE $nt59)
  (let-syntax (($
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

($nt57
 (($nt58 $nt57)
  (let-syntax (($
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

($nt58
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

