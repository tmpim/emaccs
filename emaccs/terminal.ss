(define (width)
  (call-with-values
    (lambda () (call/native '(term getSize)))
    (lambda (x y) x)))

(define (height)
  (call-with-values
    (lambda () (call/native '(term getSize)))
    (lambda (x y) y))) 

(define keys    (hash-ref (environment) "keys"))
(define colours (hash-ref (environment) "red"))

(define (term-write x)
  (call/native '(term write) x))

(define (term-set-text-colour c)
  (call/native '(term setTextColour) c))

(define (term-set-background-colour c)
  (call/native '(term setBackgroundColour) c))

(define (term-clear) (call/native '(term clear)))
(define (term-set-cursor-pos x y)
  (call/native '(term setCursorPos) x y))

(define (term-get-cursor-pos)
  (call/native '(term getCursorPos)))

(define (term-cursor-x)
  (call-with-values
    (lambda () (term-get-cursor-pos))
    (lambda (x y) x)))

(define (term-cursor-y)
  (call-with-values
    (lambda () (term-get-cursor-pos))
    (lambda (x y) y)))

(define (term-clear-line)
  (call/native '(term clearLine)))

(define term-cursor
  (case-lambda
    [() (term-get-cursor-pos)]
    [(x y) (term-set-cursor-pos x y)])) 

