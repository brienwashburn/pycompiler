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
       
       (empty? kwonlyargs)]))

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
  (define (extract-args args arg-types)
    (define keys '())
    (for/list ([i arg-types] [j args])
      (if (equal? i #f)
          (void)
            (set! keys (append keys `(,j)))))
     keys)

  (define (extract-argtypes args arg-types)
    (define keys '())
    (for/list ([i arg-types] [j args])
      (if (equal? i #f)
          (void)
          (set! keys (append keys `(,i)))))
     keys)

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
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))
       (Assign
         (targets
          (Attribute
           (Name ,id) __annotations__))
         (value 
           (Dict
             (keys ,@(if (equal? returns #f) '() `((Str "return")))
                   ,@(map (lambda (x)  `(Str ,(symbol->string x))) (car (get-types args))))
             (values ,@(if (equal? returns #f) '() returns)
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
  (match stmt
    [`(Assign (targets (Name ,id)) (value ,expr))
     (list stmt)]
    
    [`(Assign (targets (,(or 'Tuple 'List) . ,exprs)) (value ,expr))
     (error "finish me")]
       
    [`(Assign (targets ,t1 ,t2 . ,ts) (value ,expr))
     (error "finish me")]
     
    [else (list stmt)]))



;;; Convert for to while
(define (eliminate-for stmt)
  (match stmt
    
    ; (For (target <expr>) (iter <expr>) (body <stmt>*) (orelse <stmt>*))
    [`(For (target ,target)
           (iter ,iter)
           (body . ,body)
           (orelse . ,orelse))
     
     (error "todo: build a while stmt")]
              
    
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




(define (eliminate-classes-stmt stmt)
  
  (match stmt
    [`(ClassDef
       (name ,id)
       (bases . ,bases)
       (keywords . ,keywords)
       (starargs ,starargs)
       (kwargs ,kwargs)
       (body . ,body)
       (decorator_list . ,decorators))
     
     (error "complete me!")]
    
    [else  (list stmt)]))

       
      

(define prog (read))



(set! prog (walk-module prog #:transform-stmt insert-locals))

(set! prog (walk-module prog #:transform-stmt canonicalize-return))

;; Uncomment each of these as you finish them:

(set! prog (walk-module prog #:transform-stmt lift-decorators))

(set! prog (walk-module prog #:transform-stmt lift-defaults))

(set! prog (walk-module prog #:transform-stmt lift-annotations))

;(set! prog (walk-module prog #:transform-stmt eliminate-for))

;(set! prog (walk/fix prog #:transform-stmt flatten-assign))


;(set! prog (walk-module prog #:transform-expr/bu eliminate-classes-expr))

;(set! prog (walk-module prog #:transform-stmt eliminate-classes-stmt))


(write prog)

