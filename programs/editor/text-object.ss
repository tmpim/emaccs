(define (text-after-point x y)
  (string-chop (hash-ref lines y) x))

(define (text-before-point x y)
  (substring (hash-ref lines y) 1 x))

(define (get-char-ev)
  (list (call/native '(os pullEvent) "char")))

(define (word-after-point x y)
  (case (string-find (text-after-point x y) "^%w+")
    [(start . end) (cons x (+ x end))]
    [#f #f]))

(define (WORD-after-point x y)
  (case (string-find (text-after-point x y) "^[^%s]+")
    [(start . end) (cons x (+ x end))]
    [#f #f]))

(define (word-before-point x y)
  (case (string-find (text-before-point x y) "%w+$")
    [(start . end) (cons start end)]
    [#f #f]))

(define (WORD-before-point x y)
  (case (string-find (text-before-point x y) "[^%s]+$")
    [(start . end) (cons start end)]
    [#f #f]))

(define (get-text-object action x y)
  (get-char-ev)
  (define ev (get-char-ev))
  (case ev
    [("char" "w") (word-after-point x y)]
    [("char" "W") (WORD-after-point x y)]
    [("char" "b") (word-before-point x y)]
    [("char" "B") (WORD-before-point x y)]
    [("char" "$") (cons x (+ 1 (string-length (hash-ref lines y ""))))]
    [("char" c)
     (if (= c action)
       (cons 1 (+ 1 (length-of-line y)))
       #f)]))
