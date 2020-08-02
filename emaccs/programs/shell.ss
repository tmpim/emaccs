(use-modules (emaccs programs repl))

;; line editor setup
(define (refresh-editor-shadow p)
  (when (= (cdr p) "")
    (set! shadow (complete-filename p))))

(define (write-highlighted line)
  (term-write line))

(repl-prompt "$ ")

;; shell api emulation

(define current-directory (make-parameter "/"))
(define running           (make-parameter #t))
(define shell-path        '("/" "/emaccs/programs/" "/rom/programs/"))
(define shell-aliases     (make-hash-table (cons "ls"   "/rom/programs/list.lua")
                                           (cons "rm"   "/rom/programs/delete.lua")
                                           (cons "mv"   "/rom/programs/move.lua")
                                           (cons "cp"   "/rom/programs/copy.lua")
                                           (cons "scm"  "/emaccs/programs/repl.ss")
                                           (cons "edit" "/emaccs/programs/editor.ss")))

(define (shell-resolve p)
  (if (= (string-ref p 1) "/")
    p
    (call/native '(fs combine) (current-directory) p)))

(define shell-setdir current-directory)
(define shell-dir    current-directory)
(define (shell-exit) (running #f))

(define (path/ a b)
  (call/native '(fs combine) a b))

(define (parse-shell-command s)
  (define len (string-length s))
  (define (outs a out)
    (or (and (= a "") out) (cons a out)))
  (let loop ((i 1)
             (in-str #f)
             (str-buf "")
             (out '()))
    (define c (string-ref s i))
    (cond
      ((> i len) (reverse (outs str-buf out)))
      ((and (= c #\") in-str)
       (loop (+ 1 i) #f "" (outs str-buf out)))
      ((and (= c #\") (not in-str))
       (loop (+ 1 i) #t "" (outs str-buf out)))
      ((and (= c #\space) (not in-str))
       (loop (+ 1 i) #f "" (outs str-buf out)))
      (else
        (loop (+ 1 i) in-str (string-append str-buf c) out)))))

(define shell
  (make-hash-table
    (cons "resolve"           shell-resolve)
    (cons "resolveProgram"    (lambda (p) (cadr (find-program p))))
    (cons "setDir"            shell-setdir)
    (cons "dir"               shell-dir)
    (cons "execute"           (lambda (prog . args)
                                (shell-execute prog args)))
    (cons "run"               (lambda args
                                (define line (apply string-append args))
                                (case (parse-shell-command line)
                                  [(prog . args) (shell-execute prog args)]
                                  [#f (error "malformed command line " line)])))
    (cons "openTab"           (lambda args
                                 (define line (apply string-append args))
                                 (case (parse-shell-command line)
                                   [(prog . args)
                                    (open-tab (lambda ()
                                                (shell-execute prog args)
                                                (close-tab))
                                              prog)]
                                   [#f (error "malformed command line " line)])))
    (cons "switchTab"         (lambda (x) (active-tab x)))
    (cons "getRunningProgram" (lambda (x) (car (command-line))))
    (cons "setAlias"          (lambda (alias prog) (hash-set! shell-aliases alias (cdr (find-program prog)))))
    (cons "clearAlias"        (lambda (alias prog) (hash-set! shell-aliases alias #f)))
    (cons "exit"              shell-exit)))

(define (make-environment cmd-line)
  (call/native 'setmetatable
    (make-hash-table
      (cons "shell" shell)
      (cons (escape-symbol 'command-line)   (lambda () cmd-line)))
    (make-hash-table
      (cons "__index" ENV))))

(define (path-name->language p)
  (if (= (string-chop p -2) "ss")
    'scheme 'lua))

(define (find-program name)
  (if (hash-ref shell-aliases name)
    (let ((p (hash-ref shell-aliases name)))
      `(,(path-name->language p) ,p))
    (begin
      (define scm-name (string-append name ".ss"))
      (define lua-name (string-append name ".lua"))
      (let loop ((p shell-path))
        (cond
          ((null? p) #f)
          ((call/native '(fs exists) (path/ (car p) scm-name))
           `(scheme ,(path/ (car p) scm-name)))
          ((call/native '(fs exists) (path/ (car p) lua-name))
           `(lua ,(path/ (car p) lua-name)))
          (else (loop (cdr p))))))))

(define make-lua-require (hash-ref (call/native 'dofile "rom/modules/main/cc/require.lua") "make"))

(define (run-program path cmd-line)
  (let ((env (make-environment cmd-line)))
    (case path
      [('scheme p)
       (parameterise ((loaded-modules (make-hash-table)))
         (load p env))]
      [('lua p)
       (let-values (((require package) (make-lua-require env (call/native '(fs getDir) p))))
          (hash-set! env "require" require)
          (hash-set! env "package" package))
       (apply (call/native 'loadfile p "t" env) (cdr cmd-line))])))

(define (shell-execute prog args)
  (case (find-program prog)
    [#f (error "no such command: " prog)]
    [p (title prog)
       (run-program p (cons prog args))
       (title "shell")]))

(define (run-shell)
  (set! shadow #f)
  (let loop ((input (interact-line)))
    (when (running)
      (newline)
      (catch (lambda ()
               (case (parse-shell-command input)
                 [(prog . args) (shell-execute prog args)]
                 [#f #f]))
             (lambda (e)
               (cond
                 ((pair? e) (apply display e))
                 ((string? e) (display e)))
               (newline)))
      (loop (interact-line)))))

(if (= (module-name) 'main)
  (begin
    (display "$ ")
    (run-shell)))
