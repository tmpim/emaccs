(define (string-append . strs)
  (case strs
    [() ""]
    [(x) x]
    [(a b . as)
     (apply string-append (cons (call/native '(string format) "%s%s" a b) as))]))

(define (string-slice s i j)
  (call/native '(string sub) s i j))

(define (string-chop s i)
  (call/native '(string sub) s i))

(define (string-ref s i)
  (string-slice s i i))

(define (string-length s)
  (call/native '(string len) s))

