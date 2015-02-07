#lang racket

(define paren-stack '())

(define (push-paren! char)
  (set! paren-stack (cons char paren-stack)))

(define (pop-paren! char)
  (define top (car paren-stack))
  (set! paren-stack (cdr paren-stack))
  (match* {top char}
    [{#\(  #\)}   (void)]
    [{#\[  #\]}   (void)]
    [{#\{  #\}}   (void)]
    [{_    _}     (error "mismatched parens")]))

;;;(define (reset-spaces!) (error "implement me!"))
;;;(define (inc-spaces!) (error "implement me!"))
;;;(define (inc-tab!) (error "implement me!"))


(define indent-stack '())

(define current-spaces 0)

(define (reset-spaces!) 
  (set! current-spaces 0))

(define (inc-spaces!) 
  (+ 1 current-spaces))

(define (inc-tab!) 
  (+ (- 8 (modulo count 8)) current-spaces))

(define (current-indent) 
  (car indent-stack))

(define (push-indent! spaces) 
  (set! indent-stack (cons spaces indent-stack)))

(define (measure-spaces! indent-string) 
  (define (measure-spaces-recurs! indent-string count)
    (if 
     (equal? (string-length indent-string) 0) 
     count
     (match (string-ref indent-string 0)
      [#\space (measure-spaces-recurs! (substring indent-string 1) (+ count 1))]
      [#\tab (measure-spaces-recurs! (substring indent-string 1) (+ (- 8 (modulo count 8)) count))])))
  (measure-spaces-recurs! indent-string 0))


(define (handle-spaces! spaces) 
  (define number-spaces (measure-spaces! spaces))
  (cond
    [(equal? (current-indent) number-spaces) ]
    [(< (current-indent) number-spaces) (push-indent number-spaces)]
    [(> (current-indent) number-spaces) 
     (define dedents (member number-spaces indent-stack))
     (if
      (equal? 0 (length dedents))
      (error "mismatched parens")
      (for ([i (cdr dedents)])
        (pop-indent!) )

     )]))

(define indent-lexer
  (lexer
   [#\tab 
    (inc-tab!)
    (indent-lexer input-port)
   [(repitition 1 +inf.0 #\space)
    (measure-spaces! lexeme)
    (indent-lexer input-port)]
   [_ 
    (cond
    [(equal? (current-indent) current-spaces) ]
    [(< (current-indent) current-spaces) (push-indent! current-spaces)
                                         (cons (list 'INDENT) 
                                               (basic-lexer input-port))]
    [(> (current-indent) current-spaces) 
     (define dedents (length (member current-spaces indent-stack)))
     (if
      (equal? 0 dedents)
      (error "mismatched indents")
      (pop-indents! (- dedents 1)))])
    (basic-lexer input-port)]]))
   
(define (pop-paren! char)
  (define top (car paren-stack))
  (set! paren-stack (cdr paren-stack))
  (match* {top char}
    [{#\(  #\)}   (void)]
    [{#\[  #\]}   (void)]
    [{#\{  #\}}   (void)]
    [{_    _}     (error "mismatched parens")]))

(define (pop-indent!)
  (set! indent-stack (cdr indent-stack)))

(define (pop-indents! number) 
  (for ([i number])
    (pop-indent!)))