#lang racket


(require pywalk)




;;; Normalize return
(define (canonicalize-return stmt)
  (match stmt
    [`(Return)  (list '(Return (NameConstant None)))]
    [else       (list stmt)]))


  

;;; Lift defaults
(define (lift-defaults stmt)
  
  ; <local definitions go here>
  
  ; A helper:
  (define (strip-defaults! arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))

      `((Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,(if (empty? kwonlyargs) '() (make-list (length kwonlyargs) #f)))
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,(if (empty? ids) '() (make-list (length ids) #f)))))
        ]))

  (define (args-empty? arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       
       (or (empty? defaults) (equal? (car defaults) #f))]))

  (define (get-defaults arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       
       (extract-kwonly-defaults ids defaults)]))

  (define (kwonly-empty? arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       
       (or (empty? kw_defaults) (equal? (car kw_defaults) #f))]))

  (define (extract-kwonly-args kwonly-args kwonly-defaults)
    (define keys '())
    (for/list ([i kwonly-defaults] [j kwonly-args])
      (if (equal? i #f)
          (void)
            (set! keys (append keys `(,j)))))
     keys)

  (define (extract-kwonly-defaults kwonly-args kwonly-defaults)
    (define keys '())
    (for/list ([i kwonly-defaults] [j kwonly-args])
      (if (equal? i #f)
          (void)
            (set! keys (append keys `(,i)))))
     keys)



  (define (get-kwdefaults arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       
       `((,@(extract-kwonly-args kwonlyargs kw_defaults))
         (,@(extract-kwonly-defaults kwonlyargs kw_defaults)))]))
  
       
  (match stmt
    
    [`(FunctionDef 
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))

     `((FunctionDef 
       (name ,id)
       (args ,@(strip-defaults! args))
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns)) 
         (Assign
            (targets 
              (Attribute
                 (Name ,id) __defaults__))
             (value ,(if (args-empty? args) '(NameConstant None) `(Tuple ,@(get-defaults args)))))
         (Assign
            (targets 
              (Attribute
                 (Name ,id) __kwdefaults__))
             (value ,(if (kwonly-empty? args) '(NameConstant None) `(Dict 
                                                                     (keys ,@(map (lambda (x) 
                                                                                 `(Str ,(symbol->string x))) (car (get-kwdefaults args))))
                                                                     (values ,@(cadr (get-kwdefaults args))))))))]


     [else (list stmt)]))











;;; Lift annotations
(define (lift-annotations stmt)

  ; <similar local definitions as lift-defaults>

  (define (strip-types! arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))

      `((Arguments
         (args . ,ids)
         (arg-types . ,(if (empty? arg-types) '() (make-list (length arg-types) #f)))
         (vararg ,vararg) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,(if (empty? kwonlyarg-types) '() (make-list (length kwonlyarg-types) #f)))
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg )
         (defaults . ,defaults)))
        ]))



  (define (extract-args args arg-types)
    (define keys '())
    (for/list ([i arg-types] [j args])
      (if (equal? i #f)
          (void)
            (set! keys (append keys `(,j)))))
     (reverse keys))

  (define (extract-argtypes args arg-types)
    (define keys '())
    (for/list ([i arg-types] [j args])
      (if (equal? i #f)
          (void)
          (set! keys (append keys `(,i)))))
     (reverse keys))

  (define (get-types arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type) 
         (kwonlyargs . ,kwonlyargs) 
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       
       `((,@(if (equal? vararg-type '()) '() `(,vararg))
          ,@(if (equal? kwarg-type '()) '() `(,kwarg))
          ,@(extract-args kwonlyargs kwonlyarg-types)
          ,@(extract-args ids arg-types))
         (,@(if (equal? vararg-type '()) '() vararg-type)
          ,@(if (equal? kwarg-type '()) '() kwarg-type)
          ,@(extract-argtypes kwonlyargs kwonlyarg-types)
          ,@(extract-argtypes ids arg-types)))]))

       
  
  (match stmt
    [`(FunctionDef 
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))

     `((FunctionDef 
       (name ,id)
       (args ,@(strip-types! args))
       (body . ,body)
       (decorator_list . ,decorators)
       (returns #f))
       (Assign
         (targets
          (Attribute
           (Name ,id) __annotations__))
         (value 
           (Dict
             (keys ,@(if (equal? returns #f) '() `((Str "return")))
                   ,@(map (lambda (x)  `(Str ,(symbol->string x))) (car (get-types args))))
             (values ,@(if (equal? returns #f) '() `(,returns))
                     ,@(cadr (get-types args)))))))]

     
     
    
    [else (list stmt)]))

       
    
    

;;; Lift decorators
(define (lift-decorators stmt)
  
  (define (apply-decorators id decs)
    (match decs
      ['()  '()]
      [(cons dec rest)
       (cons (assign `(Name ,id) (call dec (list `(Name ,id))))
             (apply-decorators id rest))]))
  
  (match stmt
    [`(FunctionDef 
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))


     
      `((FunctionDef 
        (name ,id)
        (args ,args)
        (body . ,body)
        (decorator_list)
        (returns ,returns))
        ,@(map (lambda (x) 
           `(Assign
	    (targets 
	      (Name ,id))
	     (value 
              (Call
               (func ,x)
               (args (Name ,id))
               (keywords)
               (starargs #f)
               (kwargs #f))))) decorators))
]
     
     [`(ClassDef
        (name ,id)
        (bases . ,bases)
        (keywords . ,keywords)
        (starargs ,starargs)
        (kwargs ,kwargs)
        (body . ,body)
        (decorator_list . ,decorators))
      
      `((ClassDef
        (name ,id)
        (bases . ,bases)
        (keywords . ,keywords)
        (starargs ,starargs)
        (kwargs ,kwargs)
        (body . ,body)
        (decorator_list . ,decorators))
        ,@(map (lambda (x) 
           `(Assign
	    (targets 
	      (Name ,id))
	     (value 
              (Call
               (func ,x)
               (args (Name ,id))
               (keywords)
               (starargs #f)
               (kwargs #f))))) decorators))
]

    [else     (list stmt)]))

    
    
    


;;; Flatten assignments
(define (flatten-assign stmt)
  (define tmp0 (tmp))
  (define index -1)
  (match stmt
    [`(Assign (targets (Name ,id)) (value ,expr))
     (list stmt)]
    
    [`(Assign (targets (,(or 'Tuple 'List) . ,exprs)) (value ,expr))
     
    `(,(assign `(Name ,tmp0)
                (call `(Name list) 
                      `(,expr)))
      ,@(map (lambda (x)  
		(match x 
		  [(list 'Name x) (begin (set! index (+ index 1))
					 (assign `(Name ,x) `(Subscript (Name ,tmp0) (Index (Num ,index)))))]
		  [(list 'Starred q) (begin (define tmpind (+ 1 index))
                                            (if (< tmpind 0) (set! index (+ 1 index)) (set! index (+ 1 (- index (length exprs)))))
					    (assign q 
						 `(Subscript (Name ,tmp0) 
						    (Slice (Num ,tmpind)
							   ,(cond 
                                                              [(equal? tmpind -1) #f]
                                                              [(< tmpind 0) `(Num ,(+ tmpind 1))]
							      [else  `(Num ,(+ 1 (- tmpind (length exprs))))])
							    #f))))]))
                               exprs))]
       
    [`(Assign (targets ,t1 ,t2 . ,ts) (value ,expr))

    `(,(assign `(Name ,tmp0)
                      expr)
       ,@(map (lambda (x) 
                (assign x `(Name ,tmp0))) `(,t1 ,t2 . ,ts)))] 
     
    [else (list stmt)]))


;;; Convert for to while
(define (eliminate-for stmt)
  (define tmp1 (tmp))
  (define tmp2 (tmp)) 
  (match stmt
    
    ; (For (target <expr>) (iter <expr>) (body <stmt>*) (orelse <stmt>*))
    [`(For (target ,target)
           (iter ,iter)
           (body . ,body)
           (orelse . ,orelse))
      `(,(assign `(Name ,tmp1) `(GeneratorExp (Name ,tmp2) (for (Name ,tmp2) in ,iter if)))
         (While 
           (test
             (NameConstant True))
           (body
             (Try
               (body ,(assign target (call `(Attribute (Name ,tmp2) __next__) '())))
               (handlers 
                 (except 
                   (Name StopIteration) #f
                   (Break)))
               (orelse)
               (finalbody))
             ,@body)
             (orelse . ,orelse)))]
              
    [else    (list stmt)]))


;;; Insert locals


;;; Insert locals adds a (Local <var> ...) statement to the top
;;; of function and class bodies, so that you know which variables
;;; are assigned in that scope.
(define (insert-locals stmt)
  (match stmt
    
    [`(FunctionDef 
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))
     
     (list
      `(FunctionDef 
       (name ,id)
       (args ,args)
       (body (Local ,@(set->list (locally-assigned body))) . ,body)
       (decorator_list . ,decorators)
       (returns ,returns)))]
    
    [`(ClassDef
       (name ,id)
       (bases . ,bases)
       (keywords . ,keywords)
       (starargs ,starargs)
       (kwargs ,kwargs)
       (body . ,body)
       (decorator_list . ,decorators))
     (append
      (list 
       `(ClassDef
         (name ,id)
         (bases ,@bases)
         (keywords ,@keywords)
         (starargs ,starargs)
         (kwargs ,kwargs)
         (body (Local ,@(set->list (locally-assigned body))) ,@body)
         (decorator_list . ,decorators))))]
       
    [else (list stmt)]))
     
      
        



;;; Eliminate classes

(define (eliminate-classes-expr expr . _)
  
  ; Store the fields in a dictionary:
  (define $fields '(Name __dict__))


  (match expr
    [`(Name ,var) 
     ; =>
     (if (and (set-member? (local-vars) var) (eq? (var-scope) 'class))
         `(Subscript ,$fields (Index (Str ,(symbol->string var))))
         expr)]
    
    [else expr]))


(define (call-more fun args kwarg)
  `(Call (func ,fun)
          (args . ,args)
          (keywords) 
          (starargs #f)
          (kwargs . ,kwarg)))


(define (eliminate-classes-stmt stmt)
  (define tmp1 (tmp)) 
  (define tmp2 (tmp)) 
  (define tmp3 (tmp)) 
  (define tmp4 (tmp)) 
  (define false #f)

  (define (extract-locals bdy)
   (match bdy
     [`(Assign (targets (Name ,var)) (value ,val)) 
        
       (assign `(Subscript (Name __dict__) (Index (Str ,(symbol->string var)))) val)]))

  (define (extract-local-variables bdy)
   (match bdy
     [`(Assign (targets ,var) (value ,val)) `(,var)]))


  (match stmt
    [`(ClassDef
       (name ,id)
       (bases . ,bases)
       (keywords . ,keywords)
       (starargs ,starargs)
       (kwargs ,kwargs)
       (body . ,body)
       (decorator_list . ,decorators))
     
     `((FunctionDef 
       (name ,tmp1)
       (args
         (Arguments 
	     (args ,tmp2) 
	     (arg-types false) 
	     (vararg ,tmp3) 
	     (kwonlyargs metaclass) 
	     (kwonlyarg-types false) 
	     (kw_defaults 
	      (Name type)) 
	     (kwarg ,tmp4) 
	     (defaults 
	      (Name object))))
       (body ,(assign `(Name __dict__) `(Dict (keys) (values)))
             ,@body
             (Return ,(call-more `(Name metaclass) 
                           `((Str ,(symbol->string id)) 
                                  (BinOp (Tuple (Name ,tmp2)) Add (Name ,tmp3)) 
                                  (Name __dict__)) `((Name ,tmp4)))))
       (decorator_list))
       ,(assign `(Name ,id) (call `(Name ,tmp1) bases)))]
;          ,(assign `(Name __dict__) `(Dict (keys) (values)))
;        (Local ) ;(map extract-local-variables body))
;        ,@(map extract-locals body))))]
    
    [else  (list stmt)]))

       
      

(define prog (read))



(set! prog (walk-module prog #:transform-stmt insert-locals))

(set! prog (walk-module prog #:transform-stmt canonicalize-return))

;; Uncomment each of these as you finish them:

(set! prog (walk-module prog #:transform-stmt lift-decorators))

(set! prog (walk-module prog #:transform-stmt lift-defaults))

(set! prog (walk-module prog #:transform-stmt lift-annotations))

(set! prog (walk-module prog #:transform-stmt eliminate-for))

(set! prog (walk/fix prog #:transform-stmt flatten-assign))


(set! prog (walk-module prog #:transform-expr/bu eliminate-classes-expr))

(set! prog (walk-module prog #:transform-stmt eliminate-classes-stmt))


(write prog)

