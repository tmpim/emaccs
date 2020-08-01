(define (string-append . strs)
  (case strs
    [() ""]
    [(x) x]
    [(a b . as)
     (apply string-append
            (cons (call/native '(string format) "%s%s" a b) as))]))

(define (substring s i j)
  (call/native '(string sub) s i j))

(define (string-chop s i)
  (call/native '(string sub) s i))

(define (string-ref s i)
  (substring s i i))

(define (string-length s)
  (call/native '(string len) s))

(define/native (string-match str pattern)
  "return _str:match(_pattern) or false")

(define/native (string-find str pattern)
  "local start, fin = string.find(_str, _pattern)
   return start and {start,fin} or false")


(define (string-upcase x) (call/native '(string upper) x))
(define (string-downcase x) (call/native '(string lower) x))
