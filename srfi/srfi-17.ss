;; SRFI-17 "Generalised set!"

(define *setter-table (make-hash-table))
(define (add-setter! proc setter-proc)
  (check-parameter proc procedure? add-setter!)
  (check-parameter setter-proc procedure? add-setter!)
  (hash-set! *setter-table proc setter-proc))

(define (setter proc)
  (check-parameter proc procedure? setter)
  (hash-ref *setter-table proc))

(define-syntax set!
  (case-lambda
    (((proc . args) expr)
     `((setter ,proc) ,@args ,expr))
    ((name expr) `((#:prim set!) ,name ,expr))))

(add-setter! car set-car!)
(add-setter! cdr set-cdr!)
(add-setter! hash-ref hash-set!)
