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
   (start or_test)
   
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
 ((vfpdef $nt107 $nt110 $nt116)
  (let-syntax (($
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
 (($*134 $nt135 $nt136 $nt142)
  (let-syntax (($
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
 (($**146 vfpdef)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

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
 ((small_stmt $nt147 $nt150 NEWLINE)
  (let-syntax (($
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
 ((testlist_star_expr $nt152)
  (let-syntax (($
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
 (($nt158 $nt159 $nt163)
  (let-syntax (($
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
 (($+=165)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($-=166)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($*=167)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($/=168)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($%=169)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($&=170)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($\|=171)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($^=172)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($<<=173)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($>>=174)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($**=175)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($//=176)
  (let-syntax (($
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
 (($del177 exprlist)
  (let-syntax (($
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
 (($pass178)
  (let-syntax (($
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
 (($break179)
  (let-syntax (($
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
 (($continue180)
  (let-syntax (($
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
 (($return181 $nt182)
  (let-syntax (($
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
 (($raise183 $nt184)
  (let-syntax (($
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
 (($import189 dotted_as_names)
  (let-syntax (($
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
 (($from190 $nt191 $import200 $nt201)
  (let-syntax (($
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
 ((NAME $nt205)
  (let-syntax (($
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
 ((dotted_name $nt208)
  (let-syntax (($
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
 ((import_as_name $nt211 $nt214)
  (let-syntax (($
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
 ((dotted_as_name $nt216)
  (let-syntax (($
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
 ((NAME $nt219)
  (let-syntax (($
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
 (($global222 NAME $nt223)
  (let-syntax (($
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
 (($nonlocal226 NAME $nt227)
  (let-syntax (($
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
 (($assert230 test $nt231)
  (let-syntax (($
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
 (($if234 test $:235 suite $nt236 $nt240)
  (let-syntax (($
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
 (($while244 test $:245 suite $nt246)
  (let-syntax (($
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
 (($for250 exprlist $in251 testlist $:252 suite $nt253)
  (let-syntax (($
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
 (($try257 $:258 suite $nt259)
  (let-syntax (($
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
 (($with273 with_item $nt274 $:277 suite)
  (let-syntax (($
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
 ((test $nt278)
  (let-syntax (($
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
 (($except281 $nt282)
  (let-syntax (($
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
 ((NEWLINE INDENT $nt287 DEDENT)
  (let-syntax (($
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
 ((or_test $nt288)
  (let-syntax (($
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
 (($lambda292 $nt293 $:294 test)
  (let-syntax (($
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
 (($lambda295 $nt296 $:297 test_nocond)
  (let-syntax (($
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
 ((or_test $or298 and_test)
  (let-syntax (($
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
 ((and_test $and299 not_test)
  (let-syntax (($
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
 (($not300 not_test)
  (let-syntax (($
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
 (($nt301)
  (let-syntax (($
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
 (($*314 expr)
  (let-syntax (($
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
 ((expr $nt315 xor_expr)
  (let-syntax (($
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
 ((xor_expr $nt317 and_expr)
  (let-syntax (($
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
 ((and_expr $nt319 shift_expr)
  (let-syntax (($
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
 ((shift_expr $nt321 arith_expr)
  (let-syntax (($
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
 ((arith_expr $nt324 term)
  (let-syntax (($
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
 ((term $nt327 factor)
  (let-syntax (($
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
 (($nt332 factor)
  (let-syntax (($
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
 ((atom $nt336 $nt337)
  (let-syntax (($
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
 ((|$(340| $nt341 |$)343|)
  (let-syntax (($
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
 ((|$[344| $nt345 |$]346|)
  (let-syntax (($
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
 ((|${347| $nt348 |$}349|)
  (let-syntax (($
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
 (($nt350)
  (let-syntax (($
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
 (($...351)
  (let-syntax (($
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
 (($None352)
  (let-syntax (($
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
 (($True353)
  (let-syntax (($
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
 (($False354)
  (let-syntax (($
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
 (($nt355 $nt356)
  (let-syntax (($
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
 ((|$(363| $nt364 |$)365|)
  (let-syntax (($
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
 ((|$[366| subscriptlist |$]367|)
  (let-syntax (($
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
 (($.368 NAME)
  (let-syntax (($
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
 ((subscript $nt369 $nt372)
  (let-syntax (($
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
 (($nt374 $:375 $nt376 $nt377)
  (let-syntax (($
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
 (($:378 $nt379)
  (let-syntax (($
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
 (($nt380 $nt381 $nt385)
  (let-syntax (($
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
 ((test $nt387 $nt390)
  (let-syntax (($
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
 ((test $:392 test $nt393)
  (let-syntax (($
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
 ((test $nt400)
  (let-syntax (($
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
 (($class406 NAME $nt407 $:412 suite)
  (let-syntax (($
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
 (($nt413 $nt416)
  (let-syntax (($
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
 ((test $nt428)
  (let-syntax (($
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
 ((test $=429 test)
  (let-syntax (($
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
 (($for430 exprlist $in431 or_test $nt432)
  (let-syntax (($
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
 (($if433 test_nocond $nt434)
  (let-syntax (($
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
 (($yield435 $nt436)
  (let-syntax (($
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
 (($from437 test)
  (let-syntax (($
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

($from437
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

($nt436
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

($yield435
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

($nt434
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

($if433
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

($nt432
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

($in431
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

($for430
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

($=429
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

($nt428
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

($nt416
 ((argument $nt417)
  (let-syntax (($
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
 (($*419 test $nt420 $nt423)
  (let-syntax (($
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
 (($**427 test)
  (let-syntax (($
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

($nt423
 (($nt424)
  (let-syntax (($
                (λ (stx)
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

($nt424
 ((|$,425| $**426 test)
  (let-syntax (($
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

($**426
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

(|$,425|
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

($nt420
 (($nt421 $nt420)
  (let-syntax (($
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

($nt421
 ((|$,422| argument)
  (let-syntax (($
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

(|$,422|
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

($*419
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

($nt417
 ((|$,418|)
  (let-syntax (($
                (λ (stx)
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
 ((argument |$,415|)
  (let-syntax (($
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

($:412
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

($nt407
 (($nt408)
  (let-syntax (($
                (λ (stx)
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

($nt408
 ((|$(409| $nt410 |$)411|)
  (let-syntax (($
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

(|$)411|
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

($nt410
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

(|$(409|
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

($class406
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

($nt400
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
 (($nt401 $nt404)
  (let-syntax (($
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

($nt404
 ((|$,405|)
  (let-syntax (($
                (λ (stx)
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

($nt401
 (($nt402 $nt401)
  (let-syntax (($
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

($nt402
 ((|$,403| test)
  (let-syntax (($
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

(|$,403|
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

($nt393
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
 (($nt394 $nt398)
  (let-syntax (($
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

($nt398
 ((|$,399|)
  (let-syntax (($
                (λ (stx)
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

(|$,399|
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
 (($nt395 $nt394)
  (let-syntax (($
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

($nt395
 ((|$,396| test $:397 test)
  (let-syntax (($
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

($:397
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

(|$,396|
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

($:392
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

($nt390
 ((|$,391|)
  (let-syntax (($
                (λ (stx)
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

(|$,391|
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
 (($nt388 $nt387)
  (let-syntax (($
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

($nt388
 ((|$,389| test)
  (let-syntax (($
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

(|$,389|
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

($nt385
 ((|$,386|)
  (let-syntax (($
                (λ (stx)
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

(|$,386|
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
 ((|$,383| $nt384)
  (let-syntax (($
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

($nt384
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

($nt380
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

($nt379
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

($:378
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

($nt377
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

($nt376
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

($:375
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

($nt374
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

($nt372
 ((|$,373|)
  (let-syntax (($
                (λ (stx)
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

(|$,373|
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

($nt369
 (($nt370 $nt369)
  (let-syntax (($
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

($nt370
 ((|$,371| subscript)
  (let-syntax (($
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

(|$,371|
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

($.368
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

(|$]367|
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

(|$[366|
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

(|$)365|
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

($nt364
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

(|$(363|
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

($nt356
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
 (($nt357 $nt361)
  (let-syntax (($
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
 ((|$,362|)
  (let-syntax (($
                (λ (stx)
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

(|$,362|
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

($nt357
 (($nt358 $nt357)
  (let-syntax (($
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

($nt358
 ((|$,359| $nt360)
  (let-syntax (($
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

($nt360
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

(|$,359|
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

($nt355
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

($False354
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

($True353
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

($None352
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

($...351
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

($nt350
 ((STRING $nt350)
  (let-syntax (($
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

(|$}349|
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

($nt348
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

(|${347|
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

(|$]346|
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

($nt345
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

(|$[344|
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

(|$)343|
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

($nt341
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

($nt342
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

(|$(340|
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

($nt337
 (($nt338)
  (let-syntax (($
                (λ (stx)
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

($nt338
 (($**339 factor)
  (let-syntax (($
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

($**339
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

($nt336
 ((trailer $nt336)
  (let-syntax (($
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

($nt332
 (($+333)
  (let-syntax (($
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
 (($-334)
  (let-syntax (($
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
 (($~335)
  (let-syntax (($
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

($~335
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

($-334
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

($+333
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

($nt327
 (($*328)
  (let-syntax (($
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
 (($/329)
  (let-syntax (($
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
 (($%330)
  (let-syntax (($
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
 (($//331)
  (let-syntax (($
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

($//331
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

($%330
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

($/329
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

($*328
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

($nt324
 (($+325)
  (let-syntax (($
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
 (($-326)
  (let-syntax (($
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

($-326
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

($+325
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

($nt321
 (($<<322)
  (let-syntax (($
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
 (($>>323)
  (let-syntax (($
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

($>>323
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

($<<322
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

($nt319
 (($&320)
  (let-syntax (($
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

($&320
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

($nt317
 (($^318)
  (let-syntax (($
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

($^318
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

($nt315
 (($\|316)
  (let-syntax (($
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

($\|316
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

($*314
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

($nt301
 (($<302)
  (let-syntax (($
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
 (($>303)
  (let-syntax (($
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
 (($==304)
  (let-syntax (($
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
 (($>=305)
  (let-syntax (($
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
 (($<=306)
  (let-syntax (($
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
 (($!=307)
  (let-syntax (($
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
 (($in308)
  (let-syntax (($
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
 (($not309 $in310)
  (let-syntax (($
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
 (($is311)
  (let-syntax (($
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
 (($is312 $not313)
  (let-syntax (($
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

($not313
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

($is311
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

($in310
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

($not309
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

($in308
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

($!=307
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

($<=306
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

($>=305
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

($==304
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

($>303
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

($<302
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

($not300
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

($and299
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

($or298
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

($:297
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

($nt296
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

($lambda295
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

($:294
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

($nt293
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

($lambda292
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

($nt288
 (($nt289)
  (let-syntax (($
                (λ (stx)
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

($nt289
 (($if290 or_test $else291 test)
  (let-syntax (($
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

($else291
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

($if290
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

($nt287
 ((stmt $nt287)
  (let-syntax (($
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

($nt282
 (($nt283)
  (let-syntax (($
                (λ (stx)
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

($nt283
 ((test $nt284)
  (let-syntax (($
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

($nt284
 (($nt285)
  (let-syntax (($
                (λ (stx)
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

($nt285
 (($as286 NAME)
  (let-syntax (($
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

($as286
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

($except281
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
 (($as280 expr)
  (let-syntax (($
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

($as280
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

($:277
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

($nt274
 (($nt275 $nt274)
  (let-syntax (($
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

($nt275
 ((|$,276| with_item)
  (let-syntax (($
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

(|$,276|
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

($with273
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

($nt259
 (($nt260 $nt263 $nt267)
  (let-syntax (($
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
 (($finally271 $:272 suite)
  (let-syntax (($
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

($:272
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

($finally271
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
 (($finally269 $:270 suite)
  (let-syntax (($
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

($:270
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

($finally269
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

($nt263
 (($nt264)
  (let-syntax (($
                (λ (stx)
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

($nt264
 (($else265 $:266 suite)
  (let-syntax (($
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

($else265
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

($nt260
 (($nt261 $nt260)
  (let-syntax (($
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
 (($nt261)
  (let-syntax (($
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

($nt261
 ((except_clause $:262 suite)
  (let-syntax (($
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

($try257
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

($nt253
 (($nt254)
  (let-syntax (($
                (λ (stx)
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

($nt254
 (($else255 $:256 suite)
  (let-syntax (($
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

($:256
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

($else255
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

($:252
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

($in251
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

($for250
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

($nt246
 (($nt247)
  (let-syntax (($
                (λ (stx)
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

($nt247
 (($else248 $:249 suite)
  (let-syntax (($
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

($:249
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

($else248
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

($:245
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

($while244
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

($nt240
 (($nt241)
  (let-syntax (($
                (λ (stx)
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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2) ($ 3))))) $$))))

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

($nt236
 (($nt237 $nt236)
  (let-syntax (($
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

($nt237
 (($elif238 test $:239 suite)
  (let-syntax (($
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

($:239
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

($elif238
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

($if234
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
 ((|$,233| test)
  (let-syntax (($
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

(|$,233|
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

($assert230
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

($nt227
 (($nt228 $nt227)
  (let-syntax (($
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

($nt228
 ((|$,229| NAME)
  (let-syntax (($
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

(|$,229|
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

($nonlocal226
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

($nt223
 (($nt224 $nt223)
  (let-syntax (($
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

($nt224
 ((|$,225| NAME)
  (let-syntax (($
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

(|$,225|
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

($global222
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

($nt219
 (($nt220 $nt219)
  (let-syntax (($
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

($nt220
 (($.221 NAME)
  (let-syntax (($
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

($.221
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

($nt216
 (($nt217 $nt216)
  (let-syntax (($
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

($nt217
 ((|$,218| dotted_as_name)
  (let-syntax (($
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

(|$,218|
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

($nt214
 ((|$,215|)
  (let-syntax (($
                (λ (stx)
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

(|$,215|
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

($nt211
 (($nt212 $nt211)
  (let-syntax (($
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

($nt212
 ((|$,213| import_as_name)
  (let-syntax (($
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

(|$,213|
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

($nt208
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

($nt209
 (($as210 NAME)
  (let-syntax (($
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

($as210
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

($nt205
 (($nt206)
  (let-syntax (($
                (λ (stx)
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

($nt206
 (($as207 NAME)
  (let-syntax (($
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

($as207
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

($nt201
 (($*202)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 ((|$(203| import_as_names |$)204|)
  (let-syntax (($
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

(|$)204|
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

(|$(203|
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

($*202
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

($import200
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

($nt191
 (($nt192 dotted_name)
  (let-syntax (($
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
 (($nt196)
  (let-syntax (($
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

($nt196
 (($nt197 $nt196)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) (list ($ 1))))))

($nt197
 (($.198)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($...199)
  (let-syntax (($
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

($...199
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

($.198
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
 (($.194)
  (let-syntax (($
                (λ (stx)
                  (syntax-case stx ()
                    ((_ n)
                     (datum->syntax
                      #'n
                      (string->symbol
                       (string-append
                        "$"
                        (number->string (syntax->datum #'n))))))))))
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1))))
 (($...195)
  (let-syntax (($
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

($...195
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

($.194
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

($from190
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

($import189
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

($nt184
 (($nt185)
  (let-syntax (($
                (λ (stx)
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

($nt185
 ((test $nt186)
  (let-syntax (($
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
 (($from188 test)
  (let-syntax (($
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

($from188
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

($raise183
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

($nt182
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

($return181
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

($continue180
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

($break179
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

($pass178
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

($del177
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

($//=176
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

($**=175
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

($>>=174
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

($<<=173
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

($^=172
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

($\|=171
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

($&=170
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

($%=169
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

($/=168
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

($*=167
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

($-=166
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

($+=165
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

($nt163
 ((|$,164|)
  (let-syntax (($
                (λ (stx)
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

(|$,164|
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
 (($nt160 $nt159)
  (let-syntax (($
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

($nt160
 ((|$,161| $nt162)
  (let-syntax (($
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

($nt162
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

(|$,161|
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

($nt158
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

($nt152
 ((augassign $nt153)
  (let-syntax (($
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
    (let-syntax (($$ (λ (_) #'(list ($ 1))))) ($ 1)))))

($nt154
 (($nt155 $nt154)
  (let-syntax (($
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

($nt155
 (($=156 $nt157)
  (let-syntax (($
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

($nt157
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

($=156
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

($nt153
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

($nt150
 ((|$;151|)
  (let-syntax (($
                (λ (stx)
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

(|$;151|
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

($nt147
 (($nt148 $nt147)
  (let-syntax (($
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

($nt148
 ((|$;149| small_stmt)
  (let-syntax (($
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

(|$;149|
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

($nt142
 (($nt143)
  (let-syntax (($
                (λ (stx)
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

($nt143
 ((|$,144| $**145 vfpdef)
  (let-syntax (($
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

($**145
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

(|$,144|
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
 (()
  (let-syntax (($
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

($nt137
 ((|$,138| vfpdef $nt139)
  (let-syntax (($
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

($nt139
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

($nt140
 (($=141 test)
  (let-syntax (($
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

($=141
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

(|$,138|
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

($nt135
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

($*134
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

($nt116
 (($nt117)
  (let-syntax (($
                (λ (stx)
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

($nt117
 ((|$,118| $nt119)
  (let-syntax (($
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

($nt119
 (($nt120)
  (let-syntax (($
                (λ (stx)
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

($nt120
 (($*121 $nt122 $nt123 $nt129)
  (let-syntax (($
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
 (($**133 vfpdef)
  (let-syntax (($
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

($nt129
 (($nt130)
  (let-syntax (($
                (λ (stx)
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

($nt130
 ((|$,131| $**132 vfpdef)
  (let-syntax (($
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

($**132
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

(|$,131|
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
 (($nt124 $nt123)
  (let-syntax (($
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

($nt124
 ((|$,125| vfpdef $nt126)
  (let-syntax (($
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

($nt126
 (($nt127)
  (let-syntax (($
                (λ (stx)
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

($nt127
 (($=128 test)
  (let-syntax (($
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

($=128
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

($nt122
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

($*121
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

(|$,118|
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

($nt110
 (($nt111 $nt110)
  (let-syntax (($
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

($nt111
 ((|$,112| vfpdef $nt113)
  (let-syntax (($
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

($nt114
 (($=115 test)
  (let-syntax (($
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

($=115
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

(|$,112|
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
    (let-syntax (($$ (λ (_) #'(list ($ 1) ($ 2))))) $$))))

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

