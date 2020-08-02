(if (eq? platform 'computercraft)
  (if (call/native '(fs exists) "scheme51.lua")
    ((lambda ()
       (display "loading pre-built Scheme..." #\newline)
       (call/native 'dofile "scheme51.lua")
       (display "loading startup file..." #\newline)
       ((call/native 'load "return _load('emaccs/startup.ss')"))))))

(define (not b)
  "Negate the boolean ,b."
  (if b #f #t))

(define = eq?)
(define char=? =)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define list (lambda x x))

(define (make-hash-table)
  "Create a new hash table. This procedure is internal: Shouldn't this
  have arguments?"
  ((call/native 'load "return {}")))

(define (hash-set! table key value)
  "Associate the ,key in ,table with the given ,value."
  (call/native 'rawset table key value))

(define (copy-hash-table table)
  "Copy the hash table ,table. Since hash tables are mutable, this is
  important if you want to maintain persistence."
  (define x (make-hash-table))
  (hash-for-each table (lambda (k v)
                         (hash-set! x k v)))
  x)

(define macros (make-hash-table))

(define (push-macro! name expander)
  "Associate the macro ,name with the expansion function ,expander.
   Prefer 'define-syntax instead."
  (hash-set! macros (car name) expander)
  #t)

(define (lookup-macro-in symbol table)
  "Look up the macro called ,symbol in the table ,table."
  (hash-ref table (car symbol)))

(define (make-lambda args body)
  "Make a 'lambda expression with the given ,args and ,body."
  (cons 'lambda (cons args body)))

(define (map f x)
  "Map the procedure ,f over the list ,x, collecting the results in a
   new list that doesn't share any structure with ,x."
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
  "Test whether ,x is a member of ,xs, using '= as an equality
  predicate."
  (if (null? xs)
      #f
      (if (= (car xs) x)
          #t
          (member x (cdr xs)))))

(define (append xs ys)
  "Append the list ,xs with the list ,ys, such that the result shares
  structure with ,ys."
  (if (if (null? xs)
        #t
        (not (pair? xs)))
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (expand/helper shadow s)
  "Expand the expression ,s, without considering the symbols in ,shadow
  as possibly macros."
  (if (if (pair? s)
        (if (symbol? (car s))
          (not (member (car s) shadow))
          #f)
        #f)
   (if (eq? (car s) 'quote)
     s
     (if (if (eq? (car s) '#:prim)
           (symbol? (cadr s))
           #f)
       (cadr s)
       (if (eq? (car s) 'unquote)
         (eval (cadr s))
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
            (lookup-macro-in (car s) macros))))))
   (if (pair? s)
     (cons (expand/helper shadow (car s))
           (expand/helper shadow (cdr s)))
     s)))

(define (expand s)
  "Macro-expand the expression ,s"
  (expand/helper '() s))

(define (and-expander macro-arguments)
  "Macro expander for 'and expressions."
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
  "Macro expander for 'or expressions."
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
  "Macro expander for quasiquote expressions."
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
      (if (symbol? (car macro-arguments))
        (let ((arg (gensym)))
          `(push-macro! ',(car macro-arguments)
                         (lambda (,arg) (apply ,(cadr macro-arguments) ,arg))))
        (error "bad define-syntax")))))

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

(define (make-parameter . initval)
  "Make a parameter, or dynamic binding, with the initial value given by
  ,initval.
  A parameter is a binding that, while lexically bound (as everything in
  Scheme), has its value controlled dynamically. To change a parameter,
  call it a single argument: this will be the new value.
  If no parameters are given in the call, the stored value is returned."
  (let ((cell (or (car initval) #f)))
    (lambda args
      (if (null? args)
        cell
        (begin
          (define old cell)
          (set! cell (car args))
          old)))))

(define-syntax (parameterise vars . body)
  (if (null? vars)
    `(begin . ,body)
    (begin
      (define saved (gensym))
      (define rv (gensym))
      `(let ((,saved (,(caar vars))))
         (,(caar vars) (begin . ,(cdar vars)))
         (let ((,rv (parameterise ,(cdr vars) . ,body)))
           (,(caar vars) ,saved)
           ,rv)))))

(define make-hash-table
  (let ((empty-hash make-hash-table))
    (define (go table args)
      (if (null? args)
        table
        (begin
          (hash-set! table (caar args) (cdar args))
          (go table (cdr args)))))
    (lambda args
      "Make a hash table with the associations given as pairs in ,args."
      (define table (empty-hash))
      (go table args))))

(define (length l . a)
  "Compute the length of the list ,l. Optionally, use ('car ,a) as the
  length of the empty list. (Leave ,a empty for 0)."
  (cond
    ((null? a) (length l 0))
    ((null? l) (car a))
    ((pair? l) (length (cdr l) (+ 1 (car a))))))

(define else #t)

(define safe (make-parameter #t))

(define-syntax (check-parameter val pred? func)
  (if (safe)
    `(if (not (,pred? ,val))
       (error
         "In function " ',func ": the argument " ',val
         "was expected to be a " ',pred?))))

(define-syntax (when c . b)
  `(if ,c (begin . ,b) #f))

(define-syntax (unless c . b)
  `(if ,c #f (begin . ,b)))

(define (documentation-for-procedure f)
  "Return the documentation for ,f, if any exists.
   Documentation exists for procedures defined with a string as the
   first element of their bodies, given that the string is not the only
   expression."
  (cond
    ((procedure? f)
     (catch (lambda () (hash-ref f "doc"))
            (lambda (e) #f)))))

(define (call obj meth . args)
  "Call the method ,meth on the object ,obj, giving the ,args as
  variadic arguments."
  (apply (hash-ref obj meth) (cons obj args)))

(define (call* obj meth . args)
  "Call the method ,meth on the object ,obj, giving the ,args as
  variadic arguments."
  (apply (hash-ref obj meth) args))

(if (= platform "Scheme 51")
  (set! environment (lambda () ENV)))
