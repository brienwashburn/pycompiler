#lang racket


(require pywalk)

(define false #f)

(require (file "../python-compiler/pysugarp3_rkt.zo"))


(define (eliminate-with stmt)
  (define temp0 (tmp))
  (define temp1 (tmp))
  
  (match stmt
    [`(With ((,ctxt ,o)) ,body)
     
     `(,(assign `(Name ,temp0) ctxt) 
       ,(assign o `(Call (func (Attribute (Name  ,temp0) __enter__)) 
                         (args) 
                         (keywords) 
                         (starargs ,false) 
                         (kwargs ,false)))
       (Try (body ,body)
            (handlers (except (Name BaseException) 
                              ,temp1
                              ,(expr-stack `(Attribute (Name ,temp0) __exit__)
                                           `(args
                                             (Call
                                              (func (Name type))
                                              (args (Name ,temp1))
                                              (keywords)
                                              (starargs ,false)
                                              (kwargs ,false))
                                             (Name ,temp1)
                                             (Attribute (Name ,temp1) __traceback__)))
                              (Raise (Name ,temp1))))
       (orelse
        ,(expr-stack `(Attribute (Name ,temp0) __exit__)
                     `(args (NameConstant None)
                            (NameConstant None)
                            (NameConstant None))))
       (finalbody)))]
    
    [else (list stmt)]))

(define (expr-stack val args)
  `(Expr (Call (func ,val) ,args (keywords) (starargs ,false) (kwargs ,false))))

(define (eliminate-assert stmt)
  (match stmt
    
    [`(Assert . ,cond)
     `((If (test (Name __debug__))
           (body (If (test (UnaryOp Not ,@cond))
                     (body (Raise (Call (func (Name AssertionError))
                                        (args)
                                        (keywords)
                                        (starargs ,false)
                                        (kwargs ,false))))
                     (orelse)))
           (orelse)))]
    
    [else (list stmt)]))


;(define (extract-exceptions example tmp)
;  (match example
;    [`(except ,exception ,name ,body)
;     `((test (Call (func (Name isinstance))
;                  (args (Name ,tmp)
;                        (Name ,exception))
;                        (keywords)
;                        (starargs ,false)
;                        (kwargs   ,false)))
;      (body ,(if (equal? name false)
;                 body
;                 (assign name `(Name ,tmp)) body)))] 
;
;    [else  body]))

(define (extract-exceptions lst tmp)
  (if (equal? (length lst) 1)  
      (fourth (car lst))
      (match (car lst)
        [  `(except ,exception ,name ,body)
           `(If (test (Call (func (Name isinstance))
                            (args (Name ,tmp)
                                  ,exception)
                            (keywords)
                            (starargs ,false)
                            (kwargs   ,false)))
                ,(if (equal? name false)
                     `(body ,body)
                     `(body ,(assign `(Name ,name) `(Name ,tmp)) ,body)) 
                
                (orelse ,(extract-exceptions (cdr lst) tmp)))])))


#;(define (generate-if vars)
  (define curr (car vars)
    (if (equal? (length vars) 1) 
        (car vars)
        `(If (caar vars) (cdar cars) (orelse (generate-if cdr vars))))))

(define (canonicalize-exceptions stmt)
  (define temp (tmp))
  (match stmt
    
    [`(Try ,body ,handlers ,orelse ,finalbody)
     
     `((Try ,body 
           (handlers 
             (except (Name BaseExceptions) ,temp ,(extract-exceptions (cdr handlers) temp)))
           ,orelse
           ,finalbody))]
    
    [else (list stmt)]))


#;(define (normalize stmt)
  (define temp (tmp))
  (match stmt
    [`(Assign ,lhs (value (IfExp ,cond ,on-true ,on-false)))
     `((If (test ,cond)   
           (body 
            (Assign 
             ,lhs 
             (value 
              ,on-true)))   
           (orelse 
            (Assign 
             ,lhs
             (value 
              ,on-false)))))]
    [`(Expr (IfExp ,cond ,on-true ,on-false))
     `((If (test ,cond)   
           (body (Assign (targets (Name ,temp)) 
                         (value ,on-true)))   
           (orelse 
            (Assign (targets 
                     (Name ,temp)) 
                    (value 
                     ,on-false)))))]
    [else (list stmt)]))


;;; Normalization

; Helpers
(define (for-all pred? lst)
  (match lst
    ['()          #t]
    [`(,el)       (pred? el)]
    [(cons hd tl) (and (pred? hd) (for-all pred? tl))]))
  

(define (arguments-lifted? arguments)
  (match arguments
    [`(Arguments
       (args ,_ ...)
       (arg-types #f ...)
       (vararg ,_)
       (kwonlyargs ,_ ...)
       (kwonlyarg-types #f ...)
       (kw_defaults #f ...)
       (kwarg ,_)
       (defaults #f ...))
     #t]
    
    [else #f]))



(define (atomic? expr)
  (match expr
    [`(Name ,name)           #t]
    [`(NameConstant ,const)  #t]
    [`(Ellipsis)             #t]
    [`(Num ,num)             #t]
    [else                    #f]))


(define (atomic-slice? slice)
  (match slice
    [`(Index ,expr) 
     (atomic? expr)]
    
    [`(Slice ,lo ,hi ,step) 
     (and (atomic? lo)
          (atomic? hi)
          (atomic? step))]
    
    [`(ExtSlice ,slices ...) 
     (for-all atomic-slice? slices)]
    
    [else (error (format "bad slice during normalization: ~s" slice))]))

(define (normalized? expr)
  
  (match expr
    
    [`(BoolOp ,boolop ,expr)
     (atomic? expr)]
    
    [`(BoolOp ,boolop ,expr1 ,expr2)
     (and (atomic? expr1) (atomic? expr2))]
    
    [`(BoolOp ,boolop ,exprs ...)
     #f]
    
    [`(BinOp ,lhs ,operator ,rhs)
     (and (atomic? lhs) (atomic? rhs))]
    
    [`(UnaryOp ,unaryop ,expr)
     (atomic? expr)]
    
    [`(Lambda ,arguments ,expr)
     ; Further normalization will be blocked 
     ; until this gets lifted into a def.
     #t]
    
    [`(IfExp ,cond ,iftrue ,iffalse)
     ; Further normalization will be blocked 
     ; until this gets converted into an If stmt.
     #t]
    
    [`(Dict (keys ,keys ...) (values ,values ...))
     (and (for-all atomic? keys)
          (for-all atomic? values))]
    
    [`(Set ,exprs ...)
     (for-all atomic? exprs)]
    
    [`(ListComp ,expr [for ,target in ,seq if ,conds ...] ,comprehension ...)
     #f]
    
    [`(SetComp ,expr ,comprehensions ...)
     ; SetComp should be converted into a Set(<generator>)
     #f]
    
    [`(DictComp ,key ,value ,comprehensions ...)
     #f]
    
    [`(GeneratorExp <expr> <comprehension>*)
     ; Further normalization will be blocked
     ; until this get converted into a class.
     #t]
    
    [`(Yield)
     #t]
    
    [`(Yield ,expr)
     (atomic? expr)]
    
    [`(YieldFrom ,expr)
     (atomic? expr)]
    
    [`(Compare (left        ,lhs) 
               (ops         ,cmpop)
               (comparators ,rhs))
     (and (atomic? lhs) (atomic? rhs))]
    
    [`(Compare (left        ,lhs) 
               (ops         . ,cmpops)
               (comparators . ,rhses))
     #f]
    
    [`(Call (func ,func)
            (args ,args ...)
            (keywords [,keynames ,keyvals] ...)
            (starargs ,starargs)
            (kwargs ,kwargs))
     (and (atomic? func)
          (for-all atomic? args)
          (for-all atomic? keyvals)
          (atomic? starargs)
          (atomic? kwargs))]
    
    
    [`(Num ,number)
     #t]
    
    [`(Str ,str)
     #t]
    
    [`(Bytes ,bytes)
     #t]
    
    [`(NameConstant ,name-constant)
     #t]
    
    ['(Ellipsis)
     #t]
    
    [`(Attribute ,expr ,field)
     (atomic? expr)]
    
    [`(Subscript ,expr ,slice)
     (and (atomic? expr) (atomic-slice? slice))]
    
    ['(Starred ,expr)
     (error "Starred not allowed during normalization")]
    
    [`(Name ,id)
     #t]
    
    [`(List ,exprs ...)
     (for-all atomic? exprs)]
    
    [`(Tuple ,exprs ...)
     (for-all atomic? exprs)]
    
    [else (error (format "unrecognized expression during normalization test: ~s" expr))]))


;;; Normalize expressions

(define (normalize/stmt stmt)
  
  (define prepended '())
  
  (define (bind! name expr?)
    (if expr?
        (set! prepended
              (cons (assign `(Name ,name) expr?)
                    prepended))
        #f))
  
  (define (atomize! expr)
    (match expr
      [#f                  #f]
      [`(Num ,num)         expr]
      [`(Name ,name)       expr]
      [`(NameConstant ,_)  expr]
      [`(Ellipsis)         expr]
      
      [else 
       (define $tmp (tmp))
       (bind! $tmp expr)
       `(Name ,$tmp)]))
  (define $tmp (tmp))
  
  
  (define (normalize-slice! slice)
    (match slice
      [`(Index ,expr)           `(Index ,(atomize! expr))]
      [`(Slice ,lo ,hi ,step)   `(Slice ,(atomize! lo)
                                        ,(atomize! hi)
                                        ,(atomize! step))]
      [`(ExtSlice ,slices ...)  `(ExtSlice ,@(map normalize-slice! slices))]
      
      [else (error (format "unrecognized slice during normalization: ~s" slice))]))
  
  (define (normalize-expr! old-expr)
    
    (match old-expr
      
      [`(BoolOp ,boolop ,args ...)
        `(BoolOp ,boolop (atomize! ,args))]
      
      [`(BinOp ,lhs ,op ,rhs)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(UnaryOp ,unop ,expr)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(Lambda ,arguments ,expr)
       (error "can't normalize lambdas outside of assignment")]
      
      [`(IfExp ,cond ,on-true ,on-false)
       `((If (test ,cond)   
           (body (Assign (targets (Name ,$tmp)) 
                         (value ,on-true)))   
           (orelse 
            (Assign (targets 
                     (Name ,$tmp)) 
                    (value 
                     ,on-false)))))]
      
      [`(Dict (keys ,keys ...) (values ,values ...))
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(Set ,exprs ...)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
       
      [`(ListComp ,expr ,comprehensions ...)
       (error (format "TODO: normalize-expr! ~s" old-expr))]       
      
      [`(SetComp ,expr ,comprehensions ...)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
       
      [`(DictComp ,key ,value ,comprehensions ...)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(GeneratorExp ,expr ,comprehensions ...)
       (error "can't normalize generators outside of assignment")]
      
      [`(Yield)
       `(Yield)]
      
      [`(Yield ,expr)
       `(Yield ,(atomize! expr))]
      
      [`(YieldFrom ,expr)
       (error (format "TODO: normalize-expr! ~s" old-expr))]

      [`(Compare (left ,lhs)
                 (ops ,cmpop)
                 (comparators ,rhs))
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(Compare (left        ,lhs)
                 (ops         ,cmpop1 ,cmpops ...)
                 (comparators ,rhs1 ,rhses ...))       
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(Call (func ,func)
              (args ,args ...)
              (keywords [,keynames ,keyvals] ...)
              (starargs ,starargs)
              (kwargs ,kwargs))
       (begin
         (define tmp1 (tmp))
         `(Boobs)
         #;((Assign   
            (targets 
             (Name ,tmp1))   
            (value    
             (Call     
              (func 
               (Name f))     
              (args ,args ...) 
              (keywords [,keynames ,keyvals] ...)
              (starargs ,starargs)
              (kwargs ,kwargs))))))]
      
      [`(Num ,num)
       old-expr]
      
      [`(Str ,str)
       old-expr]
      
      [`(Bytes ,bytes)
       old-expr]
      
      [`(NameConstant ,const)
       old-expr]
      
      [`(Ellipsis)
       old-expr]
      
      [`(Attribute ,expr ,name)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(Subscript ,expr ,slice)
       `(Subscript ,expr ,(normalize-slice! slice))]
      
      [`(Starred <expr>)
       (error "can't normalize Starred expr")]
      
      [`(Name ,id)
       `(expr)]
      
      
      [`(List ,exprs ...)
       (error (format "TODO: normalize-expr! ~s" old-expr))]
      
      [`(Tuple ,exprs ...)
       (error (format "TODO: normalize-expr! ~s" old-expr))]))
  
  
  
  (define transformed
    
    (match stmt
      
      [`(FunctionDef
         (name ,name)
         (args ,arguments)
         (body ,body ...)
         (decorator_list ,decorators ...)
         (returns ,returns))
       #:when (or returns (not (null? decorators)) (not (arguments-lifted? arguments)))
       (error (format "cannot normalize function def without lifting first: ~s" stmt))]
      
      [`(ClassDef . ,_)
       (error "normalization expects class elimination")]
      
      [`(Return ,expr)
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Delete ,expr)
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Delete ,exprs ...)
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Expr ,expr)
       `(,expr)]
      
      [`(Assign (targets ,lhs)
                (value (YieldFrom ,iter)))
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Assign (targets ,lhs)
                (value (GeneratorExp ,result [for ,targets in ,iters if ,condss ...] ...)))
       (error (format "TODO: normalize: ~s" stmt))]
      
      
      [`(Assign (targets (Name ,fn)) (value (Lambda ,arguments ,expr)))
       (error (format "TODO: normalize: ~s" stmt))]
      
      ; TODO/Complexity: Normalize lhs first, to avoid doubling:
      [`(Assign (targets ,lhs) 
                (value (IfExp ,cond ,on-true ,on-false)))
       `((If (test ,cond)   
             (body 
              (Assign 
               (targets ,lhs) 
               (value 
                ,on-true)))   
             (orelse 
              (Assign 
               (targets ,lhs)
               (value 
                ,on-false)))))]
      
      
      [`(Assign (targets ,lhs)
                (value (BoolOp And ,this ,that)))
       (define $this (atomize! this))
       
       (list 
        `(If (test ,$this)
             (body (Assign (targets ,lhs) (value ,that)))
             (orelse (Assign (targets ,lhs) (value ,$this)))))]
      
      
      [`(Assign (targets ,lhs)
                (value (BoolOp Or ,this ,that)))
       
       (define $this (atomize! this))
       
       (list 
        `(If (test   ,$this)
             (body   (Assign (targets ,lhs) (value ,$this)))
             (orelse (Assign (targets ,lhs) (value ,that)))))]
      
      
      
      [`(Assign (targets ,lhs)
                (value (BoolOp ,boolop ,this ,that ,therest ...)))
       (list 
        `(Assign (targets ,lhs)
                 (value (BoolOp ,boolop ,this (BoolOp ,boolop ,that ,@therest)))))]
      
      
      [`(Assign (targets ,target) (value ,rhs))
       #:when (not (normalized? rhs))
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Assign (targets ,target) (value ,rhs))
       #:when (not (normalized? target))
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(AugAssign ,lhs ,op ,rhs)
       #:when (not (normalized? lhs))
       (error (format "TODO: normalize: ~s" stmt))]

      [`(AugAssign ,lhs ,op ,rhs)
       #:when (not (normalized? rhs))
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(For (target ,target) (iter ,iter) (body . ,body) (orelse . ,orelse))
       #:when (not (atomic? iter))
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(While (test (NameConstant True)) (body . ,body) (orelse . ,orelse))
       (list stmt)]
      
      [`(While (test ,test) (body . ,body) (orelse . ,orelse))
       (error (format "TODO: normalize: ~s" stmt))]
      
      
      [`(If (test ,test) (body . ,body) (orelse . ,orelse))
       #:when (not (atomic? test))
       (error (format "TODO: normalize: ~s" stmt))]
      
      ; TODO: Implement eliminate-with
      [`(With ([,ctxts ,names] ...) . ,body)
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Raise ,expr)
       (error (format "TODO: normalize: ~s" stmt))]

      [`(Raise ,exn ,msg)
       (error (format "TODO: normalize: ~s" stmt))]
      
      [`(Try (body . ,_)
             (handlers [except (Name ,(? symbol?)) ,_ . ,_])
             (orelse . ,_)
             (finalbody . ,_))
       (list stmt)]
      
      [`(Try . ,_)
       (error (format "normalization expects canonicalized exceptions: ~s" stmt))]
      
      [`(Assert . ,_)
       (error "normalization doesn't expect `assert`")]
      
      [else (list stmt)]))
  
  (append (reverse prepended) transformed))

     



(define prog (read))

(set! prog (walk-module prog #:transform-stmt eliminate-with))

(set! prog (walk-module prog #:transform-stmt eliminate-assert))

(set! prog (walk-module prog #:transform-stmt canonicalize-exceptions))

(set! prog (walk/fix prog 
  #:transform-stmt (compose-transform-stmt 
                       eliminate-for 
                       normalize/stmt)))

(pretty-write prog)


