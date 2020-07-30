(define (xcons d a)
  (cons d a))

(define (make-list n el)
  (check-parameter n number? make-list)
  (if (= n 0)
    '()
    (cons el (make-list (- n 1) el))))

(define (list-tabulate n proc)
  (check-parameter n number? list-tabulate)
  (check-parameter proc procedure? list-tabulate)
  (define (go j)
    (if (= j n)
      '()
      (cons (proc j)
            (go (+ 1 j)))))
  (go 0))

(define (list? xs)
  (or (null? xs)
      (and (pair? xs)
           (list? (cdr xs)))))

(define (list-copy xs)
  (check-parameter xs list? list-tabulate)
  (if (null? xs)
    '()
    (cons (car xs) (list-copy (cdr xs)))))

(define (dotted-list? xs)
  (cond
    ((null? xs) #f)
    ((pair? xs) (dotted-list? (cdr xs)))
    (else #t)))

(define (list= elt= . rest)
  (check-parameter elt= procedure? list=)
  (define (lists-equal a b)
    (or (and (null? a) (null? b))
        (and (pair? a) (pair? b)
             (elt= (car a) (car b))
             (lists-equal (cdr a) (cdr b)))))
  (define (lp l)
    (or (null? (cdr l))
        (and (lists-equal (cdr l) (cadr l))
             (lp (cdr l)))))
  (or (null? rest)
      (lp rest)))

(define (list-ref xs i)
  (check-parameter xs list? list-ref)
  (check-parameter i number? list-ref)
  (cond
    ((null? xs) (error "list-ref: index out of range"))
    ((= i 0) (car xs))
    (else (list-ref (cdr xs) (- i 1)))))


(define (car+cdr p)
  (values (car p) (cdr p)))

(define (any1 pred ls)
  (check-parameter pred procedure? any1)
  (check-parameter ls list? any1)
  (cond ((null? ls) #f)
        (else (or (pred (car ls))
                  (any1 pred (cdr ls))))))

(define (fold1 k z l)
  (if (null? l)
    z
    (k (car l) (fold1 k z (cdr l)))))

(define (iota count . r)
  (define start (if (pair? r) (car r) 0))
  (define step (if (pair? (cdr r)) (cadr r) 1))
  (check-parameter count number? iota)
  (let go ((n 0))
    (if (= n count)
      '()
      (cons (+ start (* step n))
            (go (+ 1 n))))))

(define (take x i)
  (check-parameter x list? take)
  (check-parameter i number? take)
  (cond
    ((<= 0 i) '())
    ((null? x) '())
    ((pair? x)
     (cons (car x)
           (take (cdr x) (- i 1))))))

(define (drop x i)
  (check-parameter x list? drop)
  (check-parameter i number? drop)
  (cond
    ((<= 0 i) x)
    ((null? x) '())
    (else (drop (cdr x) (- 1 i)))))

(define (zip . ls)
  (apply map (cons list ls)))

(define (all p xs)
  (if (null? xs)
    #t
    (and (p (car xs))
         (all p (cdr xs)))))

(define cons*
  (case-lambda
    (() '())
    ((x y) (cons x y))
    ((x y . r) (cons x (apply cons* (cons y r))))))
