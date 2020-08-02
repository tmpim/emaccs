(define module-name (make-parameter 'main))

(define (path->string mod-path)
  (case mod-path
    [() (error "empty path")]
    [((x) #:when (symbol? x)) (compiler-format "%s.ss" (car x))]
    [((a . b) #:when (symbol? a))
     (compiler-format "%s/%s" (car a) (path->string b))]))

(define loaded-modules (make-parameter (make-hash-table)))

(define (dofile p env)
  (define h (call/native 'assert (call/native '(fs open) p "r")))
  (define c (call* h "readAll"))
  ((call/native 'load c p "t" env)))

(define do-load-module
  (if (hash-ref (environment) "fs")
    (lambda (path env)
      (unless (call/native '(fs exists) path)
        (error "no such module" path))
      (define module-aux (string-append path ".lua"))
      (define mod-mtime
        (hash-ref (call/native '(fs attributes) path) "modification"))
      (catch (lambda ()
               (cond
                 ((and (call/native '(fs exists) module-aux)
                       (>= (hash-ref (call/native '(fs attributes) module-aux) "modification")
                           mod-mtime))
                  (dofile module-aux env))
                 (else
                   (with-output-to-file
                     module-aux
                     (lambda () (compile-file path)))
                   (dofile module-aux env))))
             (lambda (e)
               (if (and (pair? e) (exit-error? (car e)))
                 (begin
                   (call/native '(fs delete) module-aux)
                   (load path env))
                 (error e)))))
    load))

(define load-mod
  (if (hash-ref ENV "fs")
    (lambda (module env)
      (define path (path->string module))
      (unless (call/native '(fs exists) path)
        (error "no such module: " module))
      (define mtime (hash-ref (call/native '(fs attributes) path) "modification"))
      (define (do-load)
        (hash-set! (loaded-modules) path 'loading)
        (let ((name (or (module-name) 'main)))
          (module-name module)
          (do-load-module path env)
          (module-name name))
        (hash-set! (loaded-modules) path mtime)
        #t)
      (case (hash-ref (loaded-modules) path)
        ['loading
         (error "Cycle in module dependency graph: " module
                " is already being loaded")]
        [#f (do-load)]
        [(x #:when (number? x))
         (if (> mtime x)
           (do-load)
           "already loaded")]))
    (lambda (module env)
      (define path (path->string module))
      (write path #\newline)
      (case (hash-ref (loaded-modules) path)
        ['loading (error "Cycle in module dependency graph: " module
                         " is already being loaded")]
        [#f
         (hash-set! (loaded-modules) path 'loading)
         (let ((name (or (module-name) 'main)))
           (module-name module)
           (load path env)
           (module-name name))
         (hash-set! (loaded-modules) path #f)
         #t]
        [#t #t]))))

; To be done: export control


(define-syntax (use-module path)
  ; To be done: only do this for syntax definitions
  ; Maybe run in a fresh environment with only 'push-macro!'?
  (define meta-env (call/native 'setmetatable (make-hash-table) (make-hash-table (cons "__index" ENV))))
  (let ((x (run-with-exit (lambda ()
                            (eval (load-mod path meta-env))
                            (hash-set! (loaded-modules) (path->string path) #f)))))
    #t)
  `(load-mod ',path ENV))

(define-syntax (use-modules . mods)
  (if (null? mods)
    #t
    `(begin
       (use-module ,(car mods))
       (use-modules . ,(cdr mods)))))
