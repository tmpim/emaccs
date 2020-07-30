(define max
  (case-lambda
    "Return the maximum of the given arguments."
    ((x) x)
    ((x y) (if (> x y) x y))
    ((x y . z)
     (if (> x y)
       (apply max (cons x z))
       (apply max (cons y z))))))

(define min
  (case-lambda
    "Return the minimum of the given arguments."
    ((x) x)
    ((x y) (if (> x y) y x))
    ((x y . z)
     (if (> x y)
       (apply min (cons y z))
       (apply min (cons x z))))))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (car x)))))

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define (fifth x) (car (cddddr x)))
(define (sixth x) (cadr (cddddr x)))
(define (seventh x) (caddr (cddddr x)))
(define (eighth x) (cadddr (cddddr x)))
(define (ninth x) (car (cddddr (cddddr x))))
(define (tenth x) (cadr (cddddr (cddddr x))))

(define map
  (begin
    (define (fold1 f z xs)
      (if (null? xs)
        z
        (f (car xs) (fold1 f z (cdr xs)))))
    (case-lambda
      "(map proc list ...) applies proc element-wise to the elements of
       list, in order, producing a list of the results. If more than one
       list is given, the result has the same length as the smallest of the
       arguments."
      ((f l) ; map
       (check-parameter f procedure? map)
       (let go ((l l))
         (if (null? l)
           '()
           (cons (f (car l))
                 (map f (cdr l))))))
      ((f l1 l2) ; zipWith
       (check-parameter f procedure? map)
       (let go ((l1 l1) (l2 l2))
         (cond
           ((null? l1) '())
           ((null? l2) '())
           (else (cons (f (car l1) (car l2))
                       (go (cdr l2) (cdr l2)))))))
      ((f l1 . ls)
       (check-parameter f procedure? map)
       (define len (fold1 (lambda (ls len)
                            (min (length ls) len))
                          (length l1)
                          ls))
       (let go ((l1 l1) (ls ls) (len len))
         (if (= len 0)
           '()
           (cons (apply f (cons (car l1) (map car ls)))
                 (go (cdr l1) (map cdr ls) (- len 1)))))))))

(define for-each
  (begin
    (define (fold1 f z xs)
      (if (null? xs)
        z
        (f (car xs) (fold1 f z (cdr xs)))))
    (case-lambda
      "(for-each proc list ...) applies proc element-wise to the
       elements of list, in order, discarding the results. If more than
       one list is given, the result has the same length as the smallest
       of the arguments."
      ((f l) ; map
       (check-parameter f procedure? map)
       (let go ((l l))
         (if (null? l)
           '()
           (begin
             (f (car l))
             (map f (cdr l))))))
      ((f l1 l2) ; zipWith
       (check-parameter f procedure? map)
       (let go ((l1 l1) (l2 l2))
         (cond
           ((null? l1) '())
           ((null? l2) '())
           (else
             (f (car l1) (car l2))
             (go (cdr l2) (cdr l2))))))
      ((f l1 . ls)
       (check-parameter f procedure? map)
       (define len (fold1 (lambda (ls len)
                            (min (length ls) len))
                          (length l1)
                          ls))
       (let go ((l1 l1) (ls ls) (len len))
         (if (= len 0)
           '()
           (begin
             (apply f (cons (car l1) (map car ls)))
             (go (cdr l1) (map cdr ls) (- len 1)))))))))

(define (reverse l)
  "Reverse the list ,l."
  (define (go acc l)
    (if (null? l)
      acc
      (go (cons (car l) acc)
          (cdr l))))
  (go '() l))

(define (number->string x)
  "Convert the number ,x to a string representation that can be read
  back."
  (call/native 'tostring x))

(define (string->number x)
  "Convert the string ,x to a number, returning #f on failure."
  (if (call/native 'tonumber x)
    (call/native 'tonumber x)
    #f))

(define (exact->inexact x)
  "Convert an exact number (integer or rational) to an inexact number
  (floating point). Note: Scheme 51 does *not* support inexact rational
  numbers."
  (define/native (over x y) "return _x / _y")
  (if (rational? x)
    (over (car x) (cdr x))
    x))

(define (symbol->string x)
  "Return the string contents of the symbol ,x."
  (car x))

(define (string->symbol x)
  "Return a symbol with string contents ,x. This symbol is guaranteed to
  be 'eq? to any other symbol with the same string contents. In
  addition, they are guaranteed to be the same object in memory."
  (call/native 'symbol x))

(define (hash-table-size table)
  "Return the number of entries in the hash-table ,table."
  (define x 1)
  (hash-for-each table (lambda (k v) (set! x (+ 1 x))))
  x)

(define (remainder x y)
  "Return the remainder of the division of ,x by ,y."
  (- x (* y (floor/ x y))))

(define (floor/ x y)
  "Flooring division. (floor/ ,x ,y) is the same thing as (floor (/ ,x ,y))."
  (floor (/ x y)))

(define (inexact->exact q)
  "If ,q is an inexact number, return a rational approximation ,r of ,q
  such that ,q and ,r differ by at most 10^-x, where -x is the number of
  decimal digits reported by (round (/ (log (floating-part ,r)) (log 10)))."
  (cond
    ((exact? q) q)
    ((inexact? q)
     (call-with-values
       (lambda () (call/native '(math modf) q))
       (lambda (int frac)
         (define digits (+ 1 (round (/ (log frac) (log 10)))))
         (call/native 'rational (+ int (* frac (expt 10 digits)))
                                (expt 10 digits)))))))

(define (exact q)
  "R7RS name for 'inexact->exact."
  (inexact->exact q))

(define (inexact q)
  "R7RS name for 'exact->inexact"
  (exact->inexact q))

(define (fractional-part r)
  "Return the fractional part of ,r."
  (call-with-values (lambda () (call/native '(math modf) r))
                    (lambda (x y) y)))

(define (numerator q)
  "Return the numerator of ,q, with ,q expressed as a fraction in
  simplest terms if it is an inexact number. If ,q is an exact integer,
  return ,q. For details of the conversion, see 'inexact->exact."
  (cond
    ((rational? q) (cdr q))
    ((exact? q) q)
    (else (numerator (inexact->exact q)))))

(define (denominator q)
  "Return the denominator of ,q, with ,q expressed as a fraction in
  simplest terms if it is an inexact number. If ,q is an exact integer,
  return 1. For details of the conversion, see 'inexact->exact."
  (cond
    ((rational? q) (cdr q))
    ((exact? q) 1)
    (else (denominator (inexact->exact q)))))

(define (floor x)
  "Return ,x rounded down. If x is an exact rational, interpret it as an
  inexact float first."
  (call/native '(math floor) (exact->inexact x)))

(define (ceiling x)
  "Return ,x rounded up. If x is an exact rational, interpret it as an
  inexact float first."
  (call/native '(math ceil) (exact->inexact x)))

(define (round x)
  "Return ,x rounded towards the nearest integer. If x is an exact
  rational, interpret it as an inexact float first."
  (set! x (exact->inexact x))
  (if (>= x 0)
    (call/native '(math floor) (+ x 0.5))
    (call/native '(math ceil) (- x 0.5))))

(define (truncate x)
  "Returns the integer closest to ,x whose absolute value is not greater
  than |,x|."
  (call-with-values (lambda () (call/native '(math modf) x))
                    (lambda (int frac) int)))

(define (nan? x)
  "Returns #t if x is the floating point value NaN."
  (not (= x x)))

(define-syntax (define-iterated-comparison operator defn pred docstring)
  (define op (gensym))
  (define go (gensym))
  (define a (gensym))
  (define b (gensym))
  (define c (gensym))
  `(define ,operator
     (begin
       (define (,op ,a ,b)
         ,(if (= pred #f)
            #f
            `(begin
               (check-parameter ,a ,pred ',operator)
               (check-parameter ,b ,pred ',operator)))
         ((call/native 'load ,defn) ,a ,b))
       (define ,go
         (case-lambda
           ,docstring
           (() #t)
           ((,a) #t)
           ((,a ,b) (,op ,a ,b))
           ((,a ,b . ,c) (and (,op ,a ,b)
                              (apply ,go (cons ,b ,c))))))
       ,go)))

(define-iterated-comparison string<? "local x, y = ...; return x < y" string?
  "Return #t if the sequence of strings given by the arguments is
   monotonically increasing.")
(define-iterated-comparison string<=? "local x, y = ...; return x <= y" string?
  "Return #t if the sequence of strings given by the arguments is
   monotonically nondecreasing.")
(define-iterated-comparison string>? "local x, y = ...; return x > y" string?
  "Return #t if the sequence of strings given by the arguments is
   monotonically decreasing.")
(define-iterated-comparison string>=? "local x, y = ...; return x >= y" string?
  "Return #t if the sequence of strings given by the arguments is
   monotonically nonincreasing.")
(define-iterated-comparison string=? "local x, y = ...; return x == y" string?
  "Return #t if all the strings given are equal.")

(define (exp c) "Return e^c."    (call/native '(math exp) (exact->inexact c)))
(define (log c) "Return ln(c)."  (call/native '(math log) (exact->inexact c)))
(define (sin c) "Return sin(c)." (call/native '(math sin) (exact->inexact c)))
(define (cos c) "Return cos(c)." (call/native '(math cos) (exact->inexact c)))
(define (tan c) "Return tan(c)." (call/native '(math tan) (exact->inexact c)))

(define (expt z1 z2)
  "Return z1^z2. More specifically, return e^z2 ln(z1)."
  (exp (* z2 (log z1))))

(define (abs x)
  "Return the absolute value of ,x."
  (if (< x 0)
    (- x)
    x))

(define gcd
  (begin
    (define (gcd2 a b)
     (if (= b 0)
       a
       (gcd2 b (remainder a b))))
    (case-lambda
      "Return the greatest common divisor of the given numbers."
      (() 0)
      ((x) x)
      ((x y) (gcd2 x y))
      ((x y . z) (gcd2 x (apply gcd (cons y z)))))))

(define lcm
  (begin
    (define (lcm2 a b)
     (if (and (= a 0)
              (= b 0))
       0
       (/ (abs (* a b)) (gcd a b))))
    (case-lambda
      "Return the least common multiple of the given numbers."
      (() 0)
      ((x) x)
      ((x y) (lcm2 x y))
      ((x y . z) (lcm2 x (apply lcm (cons y z)))))))

(define (newline)
  "Produces an end-of-line character on the standard output port."
  (write #\newline))

(define eqv?
  (begin
    (define (reference=? x y)
      ((call/native 'load "local x, y = ...; return x == y") x y))
    (lambda (x y)
      "Return #t if ,x and ,y are equivalent in the sense of the R7RS
      predicate of the same name."
      (cond
        ((null? x) (null? y))
        ((pair? x) (reference=? x y))
        ((procedure? x) (reference=? x y))
        (else (= x y))))))

(define equal? eq?)

(define (square x)
  "Returns the square of ,x. This is equivalent to ('* ,x ,x)."
  (* x x))

(define (sqrt z)
  (cond
    ((< z 0)
     (error "Scheme 51 does not support complex numbers. negative sqrt: " z))
    ((= z 0) 0)
    (else (expt z 0.5))))

(define (make-list k . init)
  "Return a newly-allocated list of size ,k. If the initial value
   ,init is not given, it is taken to be #f."
  (define init
    (if (null? init) #f (car init)))
  (let loop ((x 0))
    (if (>= x k)
      '()
      (cons init (loop (+ 1 x))))))
