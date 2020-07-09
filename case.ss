(define-syntax (case expr . cases)
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
      ((pair? pattern)
       (define join (gensym))
       `(let ((,join (lambda () ,(rest))))
         ,(compile-match
             (car pattern)
             `(car ,match-sym)
             (compile-match
               (cdr pattern)
               `(cdr ,match-sym)
               body
               (lambda () `(,join)))
             (lambda () `(,join)))))))
  (define (expand cases)
    (if (null? cases)
      #f
      (compile-match
        (caar cases)
        match-sym
        (cons 'begin (cdar cases))
        (lambda ()
          (expand (cdr cases))))))
  `(let ((,match-sym ,expr))
     ,(expand cases)))

(define-syntax (case-lambda . cases)
  (define name (gensym))
  `(lambda ,name (case ,@cases)))

(define-syntax (syntax-rules . rules)
  (define args (gensym))
  (define (expand-rules name rules)
    (if (null? rules)
      `(error "no matching syntax rules for" name)
      (let ((pattern (cdr (caar rules)))
            (body (cdar rules)))
        (if (eq? (car (caar rules)) name)
          #t
          (error "mismatch between rule name"
                 name
                 "and pattern"
                 (caar rules)))
        `(case ,args
           (,pattern . ,body)
           (else ,(expand-rules name (cdr rules)))))))
  (define name (car (caar rules)))
  `(define-syntax (,name . ,args)
     ,(expand-rules name rules)))

(syntax-rules
  ((or) #f)
  ((or x) x)
  ((or x y . z)
   (define xs (gensym))
   `(let ((,xs ,x))
      (if ,xs ,xs (or ,y . ,z)))))
