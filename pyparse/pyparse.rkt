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
  ['()      base]
  [else     (error "can't handle trailers yet")]))


(define (recombine-arglist tups)
 (define (ra-recurs tups args defaults)
  (cond
   [(empty? tups) (list args defaults)]
   [(< (length (car tups)) 2) (ra-recurs (cdr tups) (append args (list (caar tups))) defaults)]
   [else (ra-recurs (cdr tups) (append args (list (caar tups))) (append defaults (list (cadar tups))))]))
 (ra-recurs tups '() '()))

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
   (start varargslist)
   
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
 (($nt45 ENDMARKER)
  (let-syntax (($
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
 ((testlist $nt47 ENDMARKER)
  (let-syntax (($
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
 (($@48 dotted_name $nt49 NEWLINE)
  (let-syntax (($
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
 (($nt54)
  (let-syntax (($
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
 ((decorators $nt55)
  (let-syntax (($
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
 (($def56 NAME parameters $nt57 $:60 suite)
  (let-syntax (($
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
 ((|$(61| $nt62 |$)63|)
  (let-syntax (($
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
 ((tfpdef $nt64 $nt67 $nt73)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
 (($*91 $nt92 $nt93 $nt99)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) $$))))

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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) $$))))

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
        `(ARGUMENTS
          ,`(args ,@($ 0))
          ,`(args-types ,@($ 1))
          ,`(varargs ,@($ 2))
          ,`(kwonlyargs ,@($ 3))
          ,`(kwonlyarg-types ,@($ 4))
          ,`(kw_defaults ,@($ 5))
          ,`(kwarg ,@($ 6))
          ,`(defaults ,@($ 7))))))))

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
 ((small_stmt $nt148 $nt151 NEWLINE)
  (let-syntax (($
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
 ((testlist_star_expr $nt153)
  (let-syntax (($
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

(testlist_star_expr
 (($nt159 $nt160 $nt164)
  (let-syntax (($
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

(augassign
 (($+=166)
  (let-syntax (($
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
 (($-=167)
  (let-syntax (($
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
 (($*=168)
  (let-syntax (($
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
 (($/=169)
  (let-syntax (($
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
 (($%=170)
  (let-syntax (($
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
 (($&=171)
  (let-syntax (($
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
 (($\|=172)
  (let-syntax (($
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
 (($^=173)
  (let-syntax (($
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
 (($<<=174)
  (let-syntax (($
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
 (($>>=175)
  (let-syntax (($
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
 (($**=176)
  (let-syntax (($
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
 (($//=177)
  (let-syntax (($
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

(del_stmt
 (($del178 exprlist)
  (let-syntax (($
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

(pass_stmt
 (($pass179)
  (let-syntax (($
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
 (($break180)
  (let-syntax (($
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

(continue_stmt
 (($continue181)
  (let-syntax (($
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

(return_stmt
 (($return182 $nt183)
  (let-syntax (($
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
 (($raise184 $nt185)
  (let-syntax (($
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
 (($import190 dotted_as_names)
  (let-syntax (($
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
 (($from191 $nt192 $import201 $nt202)
  (let-syntax (($
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
 ((NAME $nt206)
  (let-syntax (($
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
 ((dotted_name $nt209)
  (let-syntax (($
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
 ((import_as_name $nt212 $nt215)
  (let-syntax (($
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
 ((dotted_as_name $nt217)
  (let-syntax (($
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
 ((NAME $nt220)
  (let-syntax (($
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
 (($global223 NAME $nt224)
  (let-syntax (($
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
 (($nonlocal227 NAME $nt228)
  (let-syntax (($
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

(assert_stmt
 (($assert231 test $nt232)
  (let-syntax (($
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
 (($if235 test $:236 suite $nt237 $nt241)
  (let-syntax (($
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

(while_stmt
 (($while245 test $:246 suite $nt247)
  (let-syntax (($
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

(for_stmt
 (($for251 exprlist $in252 testlist $:253 suite $nt254)
  (let-syntax (($
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
      $$))))

(try_stmt
 (($try258 $:259 suite $nt260)
  (let-syntax (($
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

(with_stmt
 (($with274 with_item $nt275 $:278 suite)
  (let-syntax (($
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

(with_item
 ((test $nt279)
  (let-syntax (($
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

(except_clause
 (($except282 $nt283)
  (let-syntax (($
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
 ((NEWLINE INDENT $nt288 DEDENT)
  (let-syntax (($
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

(test
 ((or_test $nt289)
  (let-syntax (($
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
 (($lambda293 $nt294 $:295 test)
  (let-syntax (($
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

(lambdef_nocond
 (($lambda296 $nt297 $:298 test_nocond)
  (let-syntax (($
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

(or_test
 ((or_test $or299 and_test)
  (let-syntax (($
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
 ((and_test $and300 not_test)
  (let-syntax (($
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
 (($not301 not_test)
  (let-syntax (($
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
      `(Compare ,($ 1) ,($ 2) ,($ 3)))))
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
 (($nt302)
  (let-syntax (($
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
 (($*315 expr)
  (let-syntax (($
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
 ((expr $nt316 xor_expr)
  (let-syntax (($
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
 ((xor_expr $nt318 and_expr)
  (let-syntax (($
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
 ((and_expr $nt320 shift_expr)
  (let-syntax (($
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
 ((shift_expr $nt322 arith_expr)
  (let-syntax (($
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
 ((arith_expr $nt325 term)
  (let-syntax (($
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
 ((term $nt328 factor)
  (let-syntax (($
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
 (($nt333 factor)
  (let-syntax (($
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
 ((atom $nt337 $nt338)
  (let-syntax (($
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
 ((|$(341| $nt342 |$)344|)
  (let-syntax (($
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
 ((|$[345| $nt346 |$]347|)
  (let-syntax (($
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
 ((|${348| $nt349 |$}350|)
  (let-syntax (($
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
 (($nt351)
  (let-syntax (($
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
 (($...352)
  (let-syntax (($
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
 (($None353)
  (let-syntax (($
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
 (($True354)
  (let-syntax (($
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
 (($False355)
  (let-syntax (($
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
 (($nt356 $nt357)
  (let-syntax (($
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

(trailer
 ((|$(364| $nt365 |$)366|)
  (let-syntax (($
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
 ((|$[367| subscriptlist |$]368|)
  (let-syntax (($
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
 (($.369 NAME)
  (let-syntax (($
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

(subscriptlist
 ((subscript $nt370 $nt373)
  (let-syntax (($
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
 (($nt375 $:376 $nt377 $nt378)
  (let-syntax (($
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

(sliceop
 (($:379 $nt380)
  (let-syntax (($
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

(exprlist
 (($nt381 $nt382 $nt386)
  (let-syntax (($
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

(testlist
 ((test $nt388 $nt391)
  (let-syntax (($
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

(dictorsetmaker
 ((test $:393 test $nt394)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
 ((test $nt401)
  (let-syntax (($
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

(classdef
 (($class407 NAME $nt408 $:413 suite)
  (let-syntax (($
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
 (($nt414 $nt417)
  (let-syntax (($
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

(argument
 ((test $nt429)
  (let-syntax (($
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
 ((test $=430 test)
  (let-syntax (($
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
 (($for431 exprlist $in432 or_test $nt433)
  (let-syntax (($
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

(comp_if
 (($if434 test_nocond $nt435)
  (let-syntax (($
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
 (($yield436 $nt437)
  (let-syntax (($
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

(yield_arg
 (($from438 test)
  (let-syntax (($
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

($from438
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

($nt437
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

($yield436
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

($nt435
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

($if434
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

($nt433
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

($in432
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

($for431
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

($=430
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

($nt429
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

($nt417
 ((argument $nt418)
  (let-syntax (($
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
 (($*420 test $nt421 $nt424)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
 (($**428 test)
  (let-syntax (($
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

($**428
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

($nt424
 (($nt425)
  (let-syntax (($
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

($nt425
 ((|$,426| $**427 test)
  (let-syntax (($
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

($**427
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

(|$,426|
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

($nt421
 (($nt422 $nt421)
  (let-syntax (($
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

($nt422
 ((|$,423| argument)
  (let-syntax (($
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

(|$,423|
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

($*420
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

($nt418
 ((|$,419|)
  (let-syntax (($
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

(|$,419|
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

($nt414
 (($nt415 $nt414)
  (let-syntax (($
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

($nt415
 ((argument |$,416|)
  (let-syntax (($
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

($:413
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

($nt408
 (($nt409)
  (let-syntax (($
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

($nt409
 ((|$(410| $nt411 |$)412|)
  (let-syntax (($
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

(|$)412|
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

($nt411
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

(|$(410|
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

($class407
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

($nt401
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
 (($nt402 $nt405)
  (let-syntax (($
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

($nt405
 ((|$,406|)
  (let-syntax (($
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

(|$,406|
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
 (($nt403 $nt402)
  (let-syntax (($
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

($nt403
 ((|$,404| test)
  (let-syntax (($
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

(|$,404|
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

($nt394
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
 (($nt395 $nt399)
  (let-syntax (($
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

($nt399
 ((|$,400|)
  (let-syntax (($
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

(|$,400|
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

($nt395
 (($nt396 $nt395)
  (let-syntax (($
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

($nt396
 ((|$,397| test $:398 test)
  (let-syntax (($
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

($:398
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

(|$,397|
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

($:393
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

($nt388
 (($nt389 $nt388)
  (let-syntax (($
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

($nt389
 ((|$,390| test)
  (let-syntax (($
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
 (($nt383 $nt382)
  (let-syntax (($
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

($nt383
 ((|$,384| $nt385)
  (let-syntax (($
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

($nt385
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

(|$,384|
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

($nt381
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

($nt380
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

($:379
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

($nt378
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

($nt377
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

($:376
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

($nt375
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

($nt373
 ((|$,374|)
  (let-syntax (($
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

(|$,374|
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

($nt370
 (($nt371 $nt370)
  (let-syntax (($
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

($nt371
 ((|$,372| subscript)
  (let-syntax (($
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

(|$,372|
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

($.369
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

(|$]368|
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

(|$[367|
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

(|$)366|
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

($nt365
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

(|$(364|
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

($nt357
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
 (($nt358 $nt362)
  (let-syntax (($
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

($nt358
 (($nt359 $nt358)
  (let-syntax (($
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

($nt359
 ((|$,360| $nt361)
  (let-syntax (($
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

($nt361
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

(|$,360|
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

($nt356
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

($False355
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

($True354
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

($None353
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

($...352
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

($nt351
 ((STRING $nt351)
  (let-syntax (($
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

(|$}350|
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

($nt349
 ((dictorsetmaker)
  (let-syntax (($
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

(|${348|
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

(|$]347|
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

($nt346
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

(|$[345|
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

(|$)344|
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

($nt342
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

($nt343
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

(|$(341|
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

($nt338
 (($nt339)
  (let-syntax (($
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

($nt339
 (($**340 factor)
  (let-syntax (($
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

($**340
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

($nt337
 ((trailer $nt337)
  (let-syntax (($
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

($nt333
 (($+334)
  (let-syntax (($
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
 (($-335)
  (let-syntax (($
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
 (($~336)
  (let-syntax (($
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

($~336
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

($-335
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

($+334
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

($nt328
 (($*329)
  (let-syntax (($
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
 (($/330)
  (let-syntax (($
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
 (($%331)
  (let-syntax (($
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
 (($//332)
  (let-syntax (($
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

($//332
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

($%331
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

($/330
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

($*329
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

($nt325
 (($+326)
  (let-syntax (($
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
 (($-327)
  (let-syntax (($
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

($-327
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

($+326
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

($nt322
 (($<<323)
  (let-syntax (($
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
 (($>>324)
  (let-syntax (($
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

($>>324
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

($<<323
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

($nt320
 (($&321)
  (let-syntax (($
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

($&321
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

($nt318
 (($^319)
  (let-syntax (($
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

($^319
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

($nt316
 (($\|317)
  (let-syntax (($
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

($\|317
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

($*315
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

($nt302
 (($<303)
  (let-syntax (($
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
 (($>304)
  (let-syntax (($
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
 (($==305)
  (let-syntax (($
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
 (($>=306)
  (let-syntax (($
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
 (($<=307)
  (let-syntax (($
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
 (($!=308)
  (let-syntax (($
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
 (($in309)
  (let-syntax (($
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
 (($not310 $in311)
  (let-syntax (($
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
 (($is312)
  (let-syntax (($
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
 (($is313 $not314)
  (let-syntax (($
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

($not314
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

($is313
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

($is312
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

($in311
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

($not310
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

($in309
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

($!=308
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

($<=307
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

($>=306
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

($==305
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

($>304
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

($<303
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

($not301
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

($and300
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

($or299
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

($:298
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

($nt297
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

($lambda296
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

($:295
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

($nt294
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

($lambda293
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

($nt289
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

($nt290
 (($if291 or_test $else292 test)
  (let-syntax (($
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

($else292
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

($if291
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

($nt288
 ((stmt $nt288)
  (let-syntax (($
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

($nt283
 (($nt284)
  (let-syntax (($
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

($nt284
 ((test $nt285)
  (let-syntax (($
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

($nt285
 (($nt286)
  (let-syntax (($
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

($nt286
 (($as287 NAME)
  (let-syntax (($
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

($as287
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

($except282
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

($nt279
 (($nt280)
  (let-syntax (($
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

($nt280
 (($as281 expr)
  (let-syntax (($
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

($as281
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

($:278
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

($nt275
 (($nt276 $nt275)
  (let-syntax (($
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

($nt276
 ((|$,277| with_item)
  (let-syntax (($
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

(|$,277|
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

($with274
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

($nt260
 (($nt261 $nt264 $nt268)
  (let-syntax (($
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
 (($finally272 $:273 suite)
  (let-syntax (($
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

($:273
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

($finally272
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
 (($finally270 $:271 suite)
  (let-syntax (($
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

($:271
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

($finally270
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

($nt264
 (($nt265)
  (let-syntax (($
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

($nt265
 (($else266 $:267 suite)
  (let-syntax (($
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

($else266
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

($nt261
 (($nt262 $nt261)
  (let-syntax (($
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
 (($nt262)
  (let-syntax (($
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

($nt262
 ((except_clause $:263 suite)
  (let-syntax (($
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

($:259
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

($try258
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

($nt254
 (($nt255)
  (let-syntax (($
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

($nt255
 (($else256 $:257 suite)
  (let-syntax (($
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

($else256
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

($:253
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

($in252
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

($for251
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

($nt247
 (($nt248)
  (let-syntax (($
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

($nt248
 (($else249 $:250 suite)
  (let-syntax (($
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

($else249
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

($:246
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

($while245
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

($nt241
 (($nt242)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

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

($nt237
 (($nt238 $nt237)
  (let-syntax (($
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

($nt238
 (($elif239 test $:240 suite)
  (let-syntax (($
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

($elif239
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

($if235
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
 ((|$,234| test)
  (let-syntax (($
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

(|$,234|
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

($assert231
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

($nt228
 (($nt229 $nt228)
  (let-syntax (($
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

($nt229
 ((|$,230| NAME)
  (let-syntax (($
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

(|$,230|
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

($nonlocal227
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

($nt224
 (($nt225 $nt224)
  (let-syntax (($
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

($nt225
 ((|$,226| NAME)
  (let-syntax (($
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

(|$,226|
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

($global223
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

($nt220
 (($nt221 $nt220)
  (let-syntax (($
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

($nt221
 (($.222 NAME)
  (let-syntax (($
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

($.222
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

($nt217
 (($nt218 $nt217)
  (let-syntax (($
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

($nt218
 ((|$,219| dotted_as_name)
  (let-syntax (($
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

(|$,219|
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

($nt215
 ((|$,216|)
  (let-syntax (($
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

(|$,216|
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

($nt212
 (($nt213 $nt212)
  (let-syntax (($
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

($nt213
 ((|$,214| import_as_name)
  (let-syntax (($
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

(|$,214|
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

($nt209
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

($nt210
 (($as211 NAME)
  (let-syntax (($
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

($as211
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

($nt206
 (($nt207)
  (let-syntax (($
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

($nt207
 (($as208 NAME)
  (let-syntax (($
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

($as208
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

($nt202
 (($*203)
  (let-syntax (($
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
 ((|$(204| import_as_names |$)205|)
  (let-syntax (($
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

(|$)205|
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

(|$(204|
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

($*203
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

($import201
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

($nt192
 (($nt193 dotted_name)
  (let-syntax (($
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
 (($nt197)
  (let-syntax (($
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

($nt197
 (($nt198 $nt197)
  (let-syntax (($
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
 (($nt198)
  (let-syntax (($
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

($nt198
 (($.199)
  (let-syntax (($
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
 (($...200)
  (let-syntax (($
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

($...200
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

($.199
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

($nt193
 (($nt194 $nt193)
  (let-syntax (($
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

($nt194
 (($.195)
  (let-syntax (($
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
 (($...196)
  (let-syntax (($
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

($...196
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

($.195
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

($from191
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

($import190
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

($nt185
 (($nt186)
  (let-syntax (($
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

($nt186
 ((test $nt187)
  (let-syntax (($
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

($nt187
 (($nt188)
  (let-syntax (($
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

($nt188
 (($from189 test)
  (let-syntax (($
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

($from189
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

($raise184
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

($nt183
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

($return182
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

($continue181
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

($break180
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

($pass179
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

($del178
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

($//=177
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

($**=176
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

($>>=175
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

($<<=174
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

($^=173
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

($\|=172
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

($&=171
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

($%=170
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

($/=169
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

($*=168
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

($-=167
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

($+=166
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

($nt164
 ((|$,165|)
  (let-syntax (($
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

(|$,165|
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

($nt160
 (($nt161 $nt160)
  (let-syntax (($
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

($nt161
 ((|$,162| $nt163)
  (let-syntax (($
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

($nt163
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

(|$,162|
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

($nt159
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

($nt153
 ((augassign $nt154)
  (let-syntax (($
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
 (($nt155)
  (let-syntax (($
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

($nt155
 (($nt156 $nt155)
  (let-syntax (($
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

($nt156
 (($=157 $nt158)
  (let-syntax (($
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

($nt158
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

($=157
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

($nt154
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

($nt151
 ((|$;152|)
  (let-syntax (($
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

(|$;152|
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

($nt148
 (($nt149 $nt148)
  (let-syntax (($
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

($nt149
 ((|$;150| small_stmt)
  (let-syntax (($
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

(|$;150|
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

($nt107
 ((vfpdef $nt108 $nt111 $nt117)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
 (($*135 vfpdef $nt136 $nt143)
  (let-syntax (($
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
        `(() () (,($ 2)) (,@(car ($ 3))) () (,@(cadr ($ 3))) (,($ 4)) ())
        `(() () (,($ 2)) (,@(car ($ 3))) () (,@(cadr ($ 3))) () ())))))
 (($**147 vfpdef)
  (let-syntax (($
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
      `(() () () () () () (,($ 2)) ())))))

($**147
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

($nt143
 (($nt144)
  (let-syntax (($
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

($nt144
 ((|$,145| $**146 vfpdef)
  (let-syntax (($
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

($**146
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

(|$,145|
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
    (let-syntax (($$ (λ (_) #'(list ($ 1)))))
      (let (($ (λ (n) (list-ref ($ 1) n))) ($$ ($ 1)))
        (recombine-arglist $$))))))

($nt137
 (($nt138 $nt137)
  (let-syntax (($
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

($nt138
 ((|$,139| vfpdef $nt140)
  (let-syntax (($
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
      (if ($ 3) `(,($ 2) ,($ 3)) `(,($ 2) #f))))))

($nt140
 (($nt141)
  (let-syntax (($
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

($nt141
 (($=142 test)
  (let-syntax (($
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

($=142
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

(|$,139|
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

($*135
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

($nt117
 (($nt118)
  (let-syntax (($
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

($nt118
 ((|$,119| $nt120)
  (let-syntax (($
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
 (($*122 $nt123 $nt124 $nt130)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
 (($**134 vfpdef)
  (let-syntax (($
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

($**134
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

($nt130
 (($nt131)
  (let-syntax (($
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

($nt131
 ((|$,132| $**133 vfpdef)
  (let-syntax (($
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

($**133
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

(|$,132|
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

($nt124
 (($nt125 $nt124)
  (let-syntax (($
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

($nt125
 ((|$,126| vfpdef $nt127)
  (let-syntax (($
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

($nt127
 (($nt128)
  (let-syntax (($
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

($nt128
 (($=129 test)
  (let-syntax (($
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

($=129
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

(|$,126|
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
 ((vfpdef)
  (let-syntax (($
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

($*122
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

(|$,119|
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

($nt111
 (($nt112 $nt111)
  (let-syntax (($
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

($nt112
 ((|$,113| vfpdef $nt114)
  (let-syntax (($
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

($nt114
 (($nt115)
  (let-syntax (($
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

($nt115
 (($=116 test)
  (let-syntax (($
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

($=116
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

(|$,113|
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
 (($nt109)
  (let-syntax (($
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

($nt109
 (($=110 test)
  (let-syntax (($
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

($=110
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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) $$))))

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

($nt99
 (($nt100)
  (let-syntax (($
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

($nt100
 ((|$,101| $**102 tfpdef)
  (let-syntax (($
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

($**102
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

(|$,101|
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
 (($nt94 $nt93)
  (let-syntax (($
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

($nt94
 ((|$,95| tfpdef $nt96)
  (let-syntax (($
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

($nt96
 (($nt97)
  (let-syntax (($
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

($nt97
 (($=98 test)
  (let-syntax (($
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

($=98
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

(|$,95|
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
 ((tfpdef)
  (let-syntax (($
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

($*91
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

($nt73
 (($nt74)
  (let-syntax (($
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

($nt74
 ((|$,75| $nt76)
  (let-syntax (($
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

($nt76
 (($nt77)
  (let-syntax (($
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

($nt77
 (($*78 $nt79 $nt80 $nt86)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3) ($ 4))))) $$)))
 (($**90 tfpdef)
  (let-syntax (($
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

($**90
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

($nt86
 (($nt87)
  (let-syntax (($
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

($nt87
 ((|$,88| $**89 tfpdef)
  (let-syntax (($
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

($**89
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

(|$,88|
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
 (($nt81 $nt80)
  (let-syntax (($
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
 ((|$,82| tfpdef $nt83)
  (let-syntax (($
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

($nt83
 (($nt84)
  (let-syntax (($
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

($nt84
 (($=85 test)
  (let-syntax (($
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

($=85
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

(|$,82|
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

($nt79
 ((tfpdef)
  (let-syntax (($
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

($*78
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

(|$,75|
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

($nt67
 (($nt68 $nt67)
  (let-syntax (($
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

($nt68
 ((|$,69| tfpdef $nt70)
  (let-syntax (($
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

($nt70
 (($nt71)
  (let-syntax (($
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

($nt71
 (($=72 test)
  (let-syntax (($
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

($=72
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

(|$,69|
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

($nt64
 (($nt65)
  (let-syntax (($
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

($nt65
 (($=66 test)
  (let-syntax (($
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

($=66
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

(|$)63|
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

($nt62
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

(|$(61|
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

($:60
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

($nt57
 (($nt58)
  (let-syntax (($
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

($nt58
 (($->59 test)
  (let-syntax (($
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

($->59
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

($def56
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

($nt55
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

($nt54
 ((decorator $nt54)
  (let-syntax (($
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

($nt49
 (($nt50)
  (let-syntax (($
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

($nt50
 ((|$(51| $nt52 |$)53|)
  (let-syntax (($
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

(|$)53|
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

($nt52
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

(|$(51|
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

($@48
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

($nt47
 ((NEWLINE $nt47)
  (let-syntax (($
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

($nt45
 (($nt46 $nt45)
  (let-syntax (($
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

($nt46
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

