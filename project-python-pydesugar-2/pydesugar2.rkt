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


(define (normalize stmt)
  (error "TODO: normalization not implemented"))



(define prog (read))

(set! prog (walk-module prog #:transform-stmt eliminate-with))

(set! prog (walk-module prog #:transform-stmt eliminate-assert))

(set! prog (walk-module prog #:transform-stmt canonicalize-exceptions))

;(set! prog (walk/fix prog 
;  #:transform-stmt (compose-transform-stmt 
;                       eliminate-for 
;                       normalize)))

(pretty-write prog)


