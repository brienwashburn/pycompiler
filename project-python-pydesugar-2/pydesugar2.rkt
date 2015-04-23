#lang racket


(require pywalk)

(require (file "/usr/local/share/python-compiler/pysugarp3_rkt.zo"))


(define (eliminate-with stmt)
  (match stmt
    
    [`(With . ,_)
     (error "TODO: eliminate with")]
    
    [else (list stmt)]))


(define (eliminate-assert stmt)
  (match stmt
    
    [`(Assert . ,_)
     (error "TODO: eliminate assert")]
    
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

