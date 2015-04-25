#lang racket


(require pywalk)

;(define false #f)

(require (file "../python-compiler/pysugarp3_rkt.zo"))


(define (eliminate-with stmt)
  (match stmt
    
    [`(With . ,_)
     (error "TODO: eliminate with")]
    
    [else (list stmt)]))


(define (eliminate-assert stmt)
  (match stmt
    [`(Assert . ,_)
     (error "TODO: eliminate assert")]
    
;    [`(Assert . ,cond)
;      `(If (test (Name __debug__))
;           (body (If (test (UnaryOp Not ,cond))
;           (body (Raise (Call (func (Name AssertionError))
;                              (args)
;                              (keywords)
;                              (starargs false)
;                              (kwargs false))))
;                         (orelse)))
;                 (orelse))]
    
    [else (list stmt)]))


(define (canonicalize-exceptions stmt)
  (match stmt
    
    [`(Assert . ,_)
     (error "TODO: eliminate assert")]
    
    [else (list stmt)]))


(define (normalize stmt)
  (error "TODO: normalization not implemented"))



(define prog (read))

(set! prog (walk-module prog #:transform-stmt eliminate-with))

(set! prog (walk-module prog #:transform-stmt eliminate-assert))

(set! prog (walk-module prog #:transform-stmt canonicalize-exceptions))

(set! prog (walk-module/fix prog 
  #:transform-stmt (compose-transform-stmt 
                       eliminate-for 
                       normalize)))

(pretty-write prog)

