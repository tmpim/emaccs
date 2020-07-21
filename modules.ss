(define (path->string mod-path)
  (case mod-path
    [() (error "empty path")]
    [((x) #:when (symbol? x)) (format "%s.ss" (car x))]
    [((a . b) #:when (symbol? a))
     (format "%s/%s" (car a) (path->string b))]))

(define loaded-modules (make-hash-table))

(define (load-mod module)
  (define path (path->string module))
  (case (hash-ref loaded-modules path)
    ['loading (error "Cycle in module dependency graph: " module
                     " is already being loaded")]
    [#f
     (hash-set! loaded-modules path 'loading)
     (load path)
     (hash-set! loaded-modules path #t)]
    [#t #t]))

; To be done: export control

(define-syntax (use-module path)
  `(load-mod ',path))

(define-syntax (use-modules . mods)
  (if (null? mods)
    #t
    `(begin
       (use-module ,(car mods))
       (use-modules . ,(cdr mods)))))
