(define max
  (case-lambda
    ((x) x)
    ((x y) (if (> x y) x y))
    ((x y . z)
     (if (> x y)
       (apply max (cons x z))
       (apply max (cons y z))))))

(define min
  (case-lambda
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

(define cons*
  (case-lambda
    (() '())
    ((x y) (cons x y))
    ((x y . r) (cons x (apply cons* (cons y r))))))

(define map
  (case-lambda
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
     (let go ((l1 l1) (rest rest) (len len))
       (if (= len 0)
         '()
         (cons (apply f (cons (car l1) (map car rest)))
               (go (cdr l1) (map cdr rest) (- len 1))))))))

(define for-each
  (case-lambda
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
     (let go ((l1 l1) (rest rest) (len len))
       (if (= len 0)
         '()
         (begin
           (apply f (cons (car l1) (map car rest)))
           (go (cdr l1) (map cdr rest) (- len 1))))))))

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
  (if (not (call/native 'tonumber x))
    #f
    (call/native 'tonumber x)))

(define (exact->inexact x)
  (define/native (over x y) "return _x / _y")
  (if (rational? x)
    (over (car x) (cdr x))
    x))
