(define (case-pair? p) (pair? p))
(define *case-void (make-hash-table))

(define-syntax (*case empty expr head . cases)
  (define match-sym (gensym))
  (define (compile-match pattern match-sym body rest)
    (cond
      ((eq? 'else pattern) body)
      ((or (eq? pattern #t) (eq? pattern #f)
           (string? pattern) (number? pattern)
           (null? pattern)
           (and (pair? pattern)
                (= (car pattern) 'quote)))
       `(if (= ,pattern ,match-sym)
          ,body
          ,(rest)))
      ((symbol? pattern)
       `(let ((,pattern ,match-sym))
          ,body))
      ((and (pair? pattern)
            (pair? (cdr pattern))
            (pair? (cddr pattern))
            (eq? (cadr pattern) '#:when))
       (define join (gensym))
       `(let ((,join (lambda () ,(rest))))
          ,(compile-match
             (car pattern)
             match-sym
             `(if ,(car (cddr pattern))
                ,body
                (,join))
             (lambda () `(,join)))))
      ((pair? pattern)
       (define join (gensym))
       `(let ((,join (lambda () ,(rest))))
          (if (case-pair? ,match-sym)
            ,(compile-match
                (car pattern)
                `(car ,match-sym)
                (compile-match
                  (cdr pattern)
                  `(cdr ,match-sym)
                  body
                  (lambda () `(,join)))
                (lambda () `(,join)))
            (,join))))))
  (define (expand cases)
    (if (null? cases)
      empty
      (compile-match
        (caar cases)
        match-sym
        (cons 'begin (cdar cases))
        (lambda ()
          (expand (cdr cases))))))
  `(let ((,match-sym ,expr))
     ,(expand (cons head cases))))

(define-syntax (case expr case1 . cases)
  `(*case #f ,expr ,case1 . ,cases))

(define-syntax (case! expr case1 . cases)
  `(*case (error "no matching case for " ,expr) ,expr ,case1 . ,cases))

(define-syntax case-lambda
  (lambda args
    (define name (gensym))
    (case args
      [((docs case1 . cases) #:when (string? docs))
       `(lambda ,name
          ,docs
          (case! ,name ,case1 . ,cases))]
      [(case1 . cases)
       `(lambda ,name
          (case! ,name ,case1 . ,cases))])))

(define-syntax let-values
  (case-lambda
    [(() . body) `(begin . ,body)]
    [(((names expr) . rest) . body)
     `(call-with-values (lambda () ,expr)
                        (lambda ,names
                          (let-values ,rest . ,body)))]))

(define-syntax let
  (case-lambda
    [((name ((name1 init1) . vars) . body) #:when (symbol? name))
     `(begin
        (define ,name
          (lambda (,name1 . ,(map car vars))
            . ,body))
        (,name ,init1 . ,(map cadr vars)))]
    [(((name1 init1) . vars) . body)
     `((lambda (,name1 . ,(map car vars))
         . ,body)
       ,init1 . ,(map cadr vars))]
    [(() . body) `(begin . ,body)]))

(define-syntax (letrec vars . body)
  `((lambda (,(map car vars))
      (begin . ,(map (lambda (x) `(set! ,(car x) ,(cdr x))) vars))
      . ,body)
    . ,(map (lambda a #f) vars))) 

(define-syntax let-syntax
  (begin
    (define go
      (case-lambda
        [(((var macro) . vars) . body)
         (define saved (lookup-macro-in var macros))
         (push-macro! var (eval macro))
         (let ((b (apply go (cons vars body))))
           (push-macro! var saved)
           b)]
        [(() . body)
         `(begin . ,(expand body))]))))

(define-syntax letrec
  (case-lambda
    ((() . body) `(begin . ,body))
    ((((var1 exp1) . vars) . body)
     `((lambda (,var)
         (letrec ,vars . ,body))
       ,exp1))))
