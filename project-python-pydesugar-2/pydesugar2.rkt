#lang racket


(require pywalk)

(define false #f)

(require (file "../python-compiler/pysugarp3_rkt.zo"))


(define (eliminate-with stmt)
  (match stmt
    
    [`(With . ,_)
     (error "TODO: eliminate with")]
    
    [else (list stmt)]))


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


(define (extract-exceptions example tmp)
  (match example
    [`(except ,exception ,name ,body)
     `((test (Call (func (Name isinstance))
                  (args (Name ,tmp)
                        (Name ,exception))
                        (keywords)
                        (starargs ,false)
                        (kwargs   ,false)))
      (body ,(if (equal? name false)
                 body
                 (assign name `(Name ,tmp)) body)))] 

    [else  body]))

(define (generate-if vars)
  (define curr (car vars)
  (if (equal? (length vars) 1) 
  (car vars)
  `(If (caar vars) (cdar cars) (orelse (generate-if cdr vars))))))

(define (canonicalize-exceptions stmt)
  (define (
  (match tmp (tmp))
    
    [`(Try ,body ,handlers ,orelse ,finalbody)

     `(Try ,body (map extract-exceptions ,handlers ,tmp)
       
 )]
    
    [else (list stmt)]))


(define (normalize stmt)
  (error "TODO: normalization not implemented"))



(define prog (read))

;(set! prog (walk-module prog #:transform-stmt eliminate-with))

(set! prog (walk-module prog #:transform-stmt eliminate-assert))

;(set! prog (walk-module prog #:transform-stmt canonicalize-exceptions))

;(set! prog (walk/fix prog 
;  #:transform-stmt (compose-transform-stmt 
;                       eliminate-for 
;                       normalize)))

(pretty-write prog)

