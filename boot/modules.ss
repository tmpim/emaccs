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
          (load path env)
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
      (case (hash-ref (loaded-modules) path)
        ['loading (error "Cycle in module dependency graph: " module
                         " is already being loaded")]
        [#f
         (hash-set! (loaded-modules) path 'loading)
         (let ((name (or (module-name) 'main)))
           (module-name module)
           (load path env)
           (module-name name))
         (hash-set! (loaded-modules) path #f)]
        [#t #t]))))

; To be done: export control

(define-syntax (use-module path)
  `(load-mod ',path ENV))

(define-syntax (use-modules . mods)
  (if (null? mods)
    #t
    `(begin
       (use-module ,(car mods))
       (use-modules . ,(cdr mods)))))

(define-syntax (compile-modules . mods)
  (if (null? mods)
    #t
    `(begin
       (compile-file ,(path->string (car mods)))
       (compile-modules . ,(cdr mods)))))
