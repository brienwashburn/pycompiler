#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;;;(define (reset-spaces!) (error "implement me!"))
;;;(define (inc-spaces!) (error "implement me!"))
;;;(define (inc-tab!) (error "implement me!"))

(define indent-stack '())

(define current-spaces 0)

(define (reset-spaces!) 
  (set! current-spaces 0))

(define (inc-spaces!) 
  (set! current-spaces (+ current-spaces 1)))

(define (inc-tab! input-port) 
  (set! current-spaces (+ (- 8 (modulo count 8)) current-spaces)))

(define (current-indent) 
  (if
   (empty? indent-stack)
   0
   (car indent-stack)))

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
    [(< (current-indent) number-spaces) (push-indent! number-spaces)]
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
    (begin 
      (inc-tab!)
      (indent-lexer input-port))]
   [#\space
    (begin
      (inc-spaces!)
      (indent-lexer input-port))]
   [any-char 
    (cond
    [(equal? (current-indent) current-spaces) ]
    [(< (current-indent) current-spaces) (push-indent! current-spaces)
                                         (cons (list 'INDENT) 
                                               (list 1 2 3))]
    [(> (current-indent) current-spaces) 
     (define dedents (member current-spaces indent-stack))
     (if
      (list? dedents)
      (
       ((pop-indents! (- (length dedents) 1))
        (`(,@(generate-dedents (- (length dedents) 1)) (list 1 2 3)))))
      (error "mismatched indents"))])]))

   
(define (generate-dedents number)
  (if 
   (< 1 number) 
   (cons '(DEDENT) (generate-dedents (- number 1)))
   (list '(DEDENT))))
   

(define (pop-indent!)
  (set! indent-stack (cdr indent-stack)))

(define (pop-indents! number) 
  (pop-indent!)
  (pop-indents! (- number 1)))


;(define test-input-port (open-input-string "     foo 55 0x34 0b1011"))


;(indent-lexer test-input-port)

