(define (text-after-point x y)
  (string-chop (hash-ref lines y) x))

(define (text-before-point x y)
  (substring (hash-ref lines y) 1 x))

(define (get-char-ev)
  (list (call/native '(os pullEvent) "char")))

(define text-object-table
  (make-hash-table
    (cons "$"
      (lambda (x y) `(fore ,x . ,(length-of-line y))))))

(define-syntax define-text-object
  (case-lambda
    (((name . args) . body)
     `(define-text-object ,name (lambda ,args . ,body)))
    ((name body)
     `(hash-set! text-object-table ,(symbol->string name) ,body))))

(define-text-object (e x y)
  (case (string-find (text-after-point x y) "^%s*%w+")
    [(start . end) `(fore ,x . ,(+ x end))]
    [#f #f]))

(define-text-object (w x y)
  (case (string-find (text-after-point x y) "^%s*%w+%s*")
    [(start . end) `(fore ,x . ,(+ x end))]
    [#f #f]))

(define-text-object (W x y)
  (case (string-find (text-after-point x y) "^%s*[^%s]+%s*")
    [(start . end) `(fore ,x . ,(+ x end))]
    [#f #f]))

(define-text-object (b x y)
  (case (string-find (text-before-point (- x 1) y) "%w+%s*$")
    [(start . end) `(back ,start  . ,(+ 1 end))]
    [#f #f]))

(define-text-object (B x y)
  (case (string-find (text-before-point (- x 1) y) "[^%s]+%s*$")
    [(start . end) `(back ,start  . ,(+ 1 end))]
    [#f #f]))

(define (get-text-object action x y)
  (get-char-ev)
  (define ev (get-char-ev))
  (case ev
    [("char" c)
     (cond
       ((= c action) (cons 1 (+ 1 (length-of-line y))))
       ((procedure? (hash-ref text-object-table c))
        ((hash-ref text-object-table c) x y))
       (else #f))]))
