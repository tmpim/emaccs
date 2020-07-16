(define (not b)
  (if b #f #t))

(define = eq?)
(define char=? =)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define list (lambda x x))

(define macros '())

(define (push-macro! name expander)
  (set! macros (cons (cons name expander) macros)))

(define (lookup-macro-in symbol table)
  (if (null? table)
      #f
      (if (= (caar table) symbol)
        (cdar table)
        (lookup-macro-in symbol (cdr table)))))

(define (make-lambda args body)
  (cons 'lambda (cons args body)))

(define (map f x)
  (if (null? x)
      '()
      (cons (f (car x))
            (map f (cdr x)))))

(push-macro! 'let
  (lambda (macro-arguments)
    ((lambda (args vals body)
       (cons (make-lambda args body) vals))
     (map car (car macro-arguments))
     (map cadr (car macro-arguments))
     (cdr macro-arguments))))

(define (member x xs)
  (if (null? xs)
      #f
      (if (= (car xs) x)
          #t
          (member x (cdr xs)))))

(define (append xs ys)
  (if (if (null? xs)
        #t
        (not (pair? xs)))
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (expand/helper shadow s)
  (if (if (pair? s)
        (if (symbol? (car s))
          (not (member (car s) shadow))
          #f)
        #f)
   (if (eq? (car s) 'quote)
     s
     (if (eq? (car s) 'lambda)
       (make-lambda (cadr s)
                    (expand/helper
                      (append (if (symbol? (cadr s))
                                  (list (cadr s))
                                  (cadr s))
                              shadow)
                      (cddr s)))
       ((lambda (m-entry)
          (if m-entry
            (expand/helper shadow (m-entry (cdr s)))
            (cons (expand/helper shadow (car s))
                  (expand/helper shadow (cdr s)))))
        (lookup-macro-in (car s) macros))))
   (if (pair? s)
     (cons (expand/helper shadow (car s))
           (expand/helper shadow (cdr s)))
     s)))

(define (expand s)
  (expand/helper '() s))

(define (and-expander macro-arguments)
  (if (null? macro-arguments)
    #t
    (if (null? (cdr macro-arguments))
      (car macro-arguments)
      (list
        'if
        (car macro-arguments)
        (and-expander (cdr macro-arguments))
        #f))))

(define (or-expander macro-arguments)
  ((lambda (name)
     (if (null? macro-arguments)
       #f
       (list
         (list
           'lambda (list name)
           (list 'if name name
                 (or-expander (cdr macro-arguments))))
         (car macro-arguments))))
   (gensym)))

(push-macro! 'and and-expander)
(push-macro! 'or or-expander)

(define (quasiquote/helper do-quote expr)
  (if (and (pair? expr)
           (= 'unquote (car expr)))
    (cadr expr)
    (if (pair? expr)
      (if (and (pair? (car expr)) (= 'unquote-splicing (caar expr)))
        (list
          'append
          (car (cdar expr))
          (quasiquote/helper #t (cdr expr)))
        (if (= (car expr) 'unquote-splicing)
          (cadr expr)
          (list 'cons
            (quasiquote/helper #t (car expr))
            (quasiquote/helper #t (cdr expr)))))
      (if do-quote
        (list 'quote expr)
        expr))))

(push-macro! 'quasiquote
  (lambda (args)
    (if (and (pair? args) (null? (cdr args)))
      (quasiquote/helper #t (car args))
      (error "bad quasiquote"))))


(push-macro! 'define-syntax
  (lambda (macro-arguments)
    (if (and (pair? (car macro-arguments))
             (pair? (cdr macro-arguments)))
      (let ((name (caar macro-arguments))
            (args (cdar macro-arguments))
            (body (cdr macro-arguments))
            (macro-args (gensym)))
        `(push-macro! ',name
                      ,(make-lambda
                         (list macro-args)
                         `((apply ,(make-lambda args body) ,macro-args)))))
      (error "bad define-syntax"))))

(push-macro! 'begin (lambda (macro-arguments)
                      `(,(make-lambda '() macro-arguments))))

(define-syntax (cond . cases)
  (define (expand cases)
    (if (null? cases)
      #f
      `(if ,(caar cases)
         ,(cons 'begin (cdar cases))
         ,(expand (cdr cases)))))
  (expand cases))

(define else #t)
