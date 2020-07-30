(define module-name (make-parameter 'main))

(define (path->string mod-path)
  (case mod-path
    [() (error "empty path")]
    [((x) #:when (symbol? x)) (format "%s.ss" (car x))]
    [((a . b) #:when (symbol? a))
     (format "%s/%s" (car a) (path->string b))]))

(define loaded-modules (make-hash-table))
(if (= platform "Scheme 51")
  (define/native (string-append x y) "return _x .. _y"))

(define do-load-module
  (if (hash-ref (environment) "fs")
    (lambda (path)
      (unless (call/native '(fs exists) path)
        (error "no such module" path))
      (define module-aux (string-append path ".lua"))
      (define mod-mtime
        (hash-ref (call/native '(fs attributes) path) "modification"))
      (cond
        ((and (call/native '(fs exists) module-aux)
              (>= (hash-ref (call/native '(fs attributes) module-aux) "modification")
                  mod-mtime))
         (call/native 'dofile module-aux))
        (else
          (with-output-to-file
            module-aux
            (lambda ()
              (compile-file path)))
          (call/native 'dofile module-aux))))
    load))

(define load-mod
  (if (hash-ref (environment) "fs")
    (lambda (module)
      (define path (path->string module))
      (unless (call/native '(fs exists) path)
        (error "no such module: " module))
      (define mtime (hash-ref (call/native '(fs attributes) path) "modification"))
      (define (do-load)
        (hash-set! loaded-modules path 'loading)
        (let ((name (or (module-name) 'main)))
          (module-name module)
          (do-load-module path)
          (module-name name))
        (hash-set! loaded-modules path mtime)
        #t)
      (case (hash-ref loaded-modules path)
        ['loading
         (error "Cycle in module dependency graph: " module
                " is already being loaded")]
        [#f (do-load)]
        [(x #:when (number? x))
         (if (> mtime x)
           (do-load)
           "already loaded")]))
    (lambda (module)
      (define path (path->string module))
      (case (hash-ref loaded-modules path)
        ['loading (error "Cycle in module dependency graph: " module
                         " is already being loaded")]
        [#f
         (hash-set! loaded-modules path 'loading)
         (let ((name (or (module-name) 'main)))
           (module-name module)
           (do-load-module path)
           (module-name name))
         (hash-set! loaded-modules path #t)]
        [#t #t]))))

; To be done: export control

(define-syntax (use-module path)
  `(load-mod ',path))

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
