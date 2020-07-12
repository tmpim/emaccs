(if (not (eq? platform "Boot Scheme"))
  (load "case.ss") #t)

(define (expand-for-compilation expr)
  (expand '(let) expr))

(define (format . args)
  (apply ((call/native 'load "return string.format")) args))

(define (escape-symbol e)
  (if (not (symbol? e))
    (error "not a symbol" e))
  (format "_%s"
          (call/native '(string gsub)
             (car e)
             "[#+-?!=><*/]"
             ((call/native
                'load
                "return function(s) return 'S' .. string.byte(s) end")))))

(define (compile-simple-expression return e)
  (cond
    ((number? e) (return (format "%d" e)))
    ((string? e) (return (format "%q" e)))
    ((symbol? e) (return (format "var(%s, %q)" (escape-symbol e) (car e))))
    ((= e #t) (return "true"))
    ((= e #f) (return "false"))
    ((= e #eof) (return "scm_eof"))
    ((null? e) (return "scm_nil"))
    ((and (pair? e)
          (= 'quote (car e)))
     (compile-quote return (cadr e)))
    (else (error "not a simple expression" e))))

(define (compile-quote return e)
  (cond
    ((symbol? e) (return (format "symbol('%s')" (car e))))
    ((pair? e)
     (return (format "{%s,%s}"
                     (compile-quote (lambda (x) x) (car e))
                     (compile-quote (lambda (x) x) (cdr e)))))
    (else (compile-simple-expression return e))))

(define (list? x)
  (if (null? x)
    #t
    (if (pair? x)
      (list? (cdr x))
      #f)))

(define (simple-argument-list empty args)
  (if (null? args)
    empty
    (if (symbol? (car args))
      (format "%s%s%s"
         (escape-symbol (car args))
         (if (and (null? (cdr args)) (eq? empty "")) "" ", ")
         (simple-argument-list empty (cdr args)))
      (error "non-symbol in simple-argument-list " args))))

(define (complex-argument-list args)
  (cond
    ((symbol? args) (cons () args))
    ((pair? args)
     (let ((r (complex-argument-list (cdr args))))
       (cons (cons (car args) (car r)) (cdr r))))))

(define (compile-lambda return args body)
  (cond
    ((list? args)
     (return (format "(%s)\n %s\n end"
                     (simple-argument-list "" args)
                     (compile-body (lambda (x) (format "return %s" x)) body))))
    (else
      (let ((args-and-rest (complex-argument-list args)))
        (let ((proper (car args-and-rest))
              (rest (cdr args-and-rest)))
          (return (format "(%s)\n local %s = list(...);\n %s\n end"
                          (simple-argument-list "..." proper)
                          (escape-symbol rest)
                          (compile-body (lambda (x) (format "return %s" x))
                                        body))))))))

(define (atomic? p)
  (or (number? p) (string? p)
      (= p #t) (= p #f)
      (and (pair? p)
           (or (= (car p) 'lambda) (= (car p) 'quote)))))

(define (define? e) (and (pair? e) (= (car e) 'define)))

(define (compile-body return b)
  (cond
    ((null? b) (return "false"))
    ((and (pair? b) (null? (cdr b))) (compile-expr return (car b)))
    ((atomic? (car b) (compile-body return (cdr b))))
    ((define? (car b))
     (format "%s;\n %s"
             (apply compile-body-define (cdar b))
             (compile-body return (cdr b))))
    (else (format "%s;\n %s"
                 (compile-expr (lambda (x) x) (car b))
                 (compile-body return (cdr b))))))

(define (compile-body-define name . body)
  (if (pair? name)
    (compile-body-define (car name) `(lambda ,(cdr name) . ,body))
    (compile-expr (lambda (x) (format "local %s;\n %s = %s"
                                      (escape-symbol name)
                                      (escape-symbol name)
                                      x))
                  (car body))))

(define (compile-args alist)
  (if (null? alist)
    ""
    (if (define? (car alist))
      (error "illegal define expression" (car alist) "in argument list")
      (format "%s%s%s"
              (compile-expr (lambda (x) x) (car alist))
              (if (null? (cdr alist)) "" ", ")
              (compile-args (cdr alist))))))

(define (compile-expr return expr)
  (case expr
    [(e #:when (not (pair? e))) (compile-simple-expression return e)]
    [('quote e) (compile-quote return e)]
    [('lambda args . body)
     (let ((ret return))
       (compile-lambda (lambda (x) (ret (format "(function%s)" x))) args body))]
    [(('define name value) #:when (symbol? name))
     (format "%s;\n %s"
             (compile-expr
               (lambda (v) (format "%s = %s" (escape-symbol name) v))
               value)
             (return (escape-symbol name)))]
    [('define (name . args) . body)
     (compile-expr return `(define ,name (lambda ,args . ,body)))]
    [('set! name expr)
     (let ((ret return))
       (compile-expr
           (lambda (v)
             (ret (format
                   "(function()\n %s = scm_set_helper(%s, %s, %q);\n return %s\n end)()"
                   (escape-symbol name) (escape-symbol name) v (car name) (escape-symbol name))))
           expr))]
    [('if c t)
     (define it (escape-symbol (gensym)))
     (return
       (format "(function(%s)\n if %s ~= false and %s ~= scm_nil then\n %s else\n return false\n end\n end)(%s)"
               it it it
               (compile-expr (lambda (x) (format "return %s" x)) t)
               (compile-expr (lambda (x) x) c)))]
    [('if c t e)
     (define it (escape-symbol (gensym)))
     (return
       (format "(function(%s)\n if %s ~= false and %s ~= scm_nil then\n %s else\n %s\n end\n end)(%s)"
               it it it
               (compile-expr (lambda (x) (format "return %s" x)) t)
               (compile-expr (lambda (x) (format "return %s" x)) e)
               (compile-expr (lambda (x) x) c)))]
    [('cons a b)
     (return (format "{%s,%s}" (compile-expr (lambda (x) x) a) (compile-expr (lambda (x) x) b)))]
    [(fun . args)
     (return (format "%s(%s)"
               (compile-expr (lambda (x) x) fun)
               (compile-args args)))]))

(define-syntax (run/native r)
  `(begin
     (if booting
       (write #\newline ,r #\newline))
     (call/native 'load ,r)))

(run/native
   "function var(x, n)
      assert(x ~= nil, 'no binding for symbol ' .. n);
      return x
    end")

(run/native
  "function list(car, ...)
     if car then
       return { car, list(...) }
     else
       return scm_nil
     end
   end")

(run/native
  "function scm_set_helper(var, val, name)
     assert(var ~= nil, 'no previous binding for symbol ' .. name);
     return val
   end")

(define-syntax (define/native name body)
  (if (pair? name)
    `(run/native ,(format "function %s(%s) %s end"
                          (escape-symbol (car name))
                          (simple-argument-list "" (cdr name))
                          body))
    `(run/native ,(format "%s = %s" (escape-symbol (car name)) body))))

(define (compile e)
  (compile-expr (lambda (x) (format "return %s" x)) (expand e)))

(run/native "_G._S42globalS45environmentS42 = _ENV")

(define (compile-and-load e)
  (define name
    (if (define? e)
        (if (pair? (cadr e))
            (car (car (cadr e)))
            (car (cadr e)))
        "[expr]"))
  (call/native 'load (compile e) name "t" *global-environment*))

(define (compile-and-run e)
  ((compile-and-load e)))

(define (repl p i)
  (if p
    (cond
      ((eq? platform "Boot Scheme") (write "boot> "))
      ((eq? platform "Scheme 51") (write "> "))
      (else (write "load> "))))
  (let ((x (read)))
    (if (eq? x #eof)
      i
      (if booting
        (begin
          (write (compile-expr (lambda (x) (format "ignore(%s)\n" x)) (expand x)))
          (repl p (+ 1 i)))
        (catch (lambda ()
                 ((if p write (lambda (x) #f))
                  (compile-and-run x)
                  #\newline)
                 (repl p (+ 1 i)))
              (lambda (e)
                (write "Error in user code: " e #\newline)
                (repl p i)))))))

(define/native (set-car! cell val) "_cell[1] = _val; return true")
(define/native (car cell) "return _cell[1]")
(define/native (set-cdr! cell val) "_cell[2] = _val; return true")
(define/native (cdr cell) "return _cell[2]")

(define/native (null? p) "return _p == scm_nil or _p == nil")
(define/native (number? p) "return type(_p) == 'number'")
(define/native (string? p) "return type(_p) == 'string'")
(define/native (char? p) "return type(_p) == 'string' and #_p == 1")

(define/native (eq? a b)
  "if _a == _b then
     return true
   elseif _pairS63(_a) and _pairS63(_b) then
     return _eqS63(_a[1], _b[1]) and _eqS63(_a[2], _b[2])
   elseif type(_a) == 'table' and (_a[0] == eval or _a[0] == callproc) then
     return false
   elseif type(_a) == 'table' and _a.kw and type(_b) == 'table' and _b.kw then
     return _a[1] == _b[1]
   end
   return false
  ")
(define/native (defined? s) "return _symbolS63(s) and _ENV[s[1]]")

(if (= platform "Boot Scheme")
  (run/native "_platform = 'Scheme 51'")
  (run/native "_platform = 'Boot Scheme'"))

(define/native (with-input-from-file path thunk) "return redirect(_path, _thunk)")

(if (not booting)
  (run/native
    "do
      local h = assert(io.open('operators.lua', 'r'))
      assert(load(h:read'*a'))()
      h:close()
     end"))

(run/native "_booting = false") ; compiler is never used for booting

(define *loaded-modules* '())

(define (compiler-load path)
  (if (not (eq? (call/native 'rawget *loaded-modules* path)
                (call/native 'rawget *loaded-modules* *loaded-modules*)))
    (write "Already compiled " path #\newline)
    (begin
      (if (not booting)
        (write "Compiling " path #\newline))
      (with-input-from-file path (lambda () (repl #f 0)))
      (call/native 'rawset *loaded-modules* path #t)
      #t)))

(define (compile-file path)
  (define (loop)
    (let ((x (read)))
      (if (eq? x #eof)
        (write "end-of-file\n")
        (begin
          (write (compile-expr (lambda (x) (format "ignore(%s)" x)) x))
          (loop)))))
  (with-input-from-file path loop))

(if (= platform "Scheme 51")
  (define load compiler-load))

(define (run path)
  (with-input-from-file path (lambda () (repl #f 0))))