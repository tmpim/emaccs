(use-modules (scm-51 string)
             (scm-51 input)

             (emaccs terminal)
             (programs repl))

(define lines        (make-hash-table))
(define (line-count) (call/native '(table getn) lines))

(load "/programs/editor/text-object.ss" ENV)

(define running     (make-parameter #t))
(define buffer-name (make-parameter "*scratch*"))
(define modified    (make-parameter #f))
(define file-type   (make-parameter 'scheme))
(define tab-stop    (make-parameter 2))

(define cursor (cons 1 1))
(define scroll (cons 0 0))

(define (open-file path)
  (if (and (not (modified)) (not (eq? (buffer-name) "*scratch*")))
    (begin
      (set! lines (make-hash-table))
      (catch
        (lambda ()
          (define h (call/native 'assert (call/native '(fs open) path "r")))
          (let loop ((n 1))
            (case (catch (lambda () (define x (call* h "readLine")) x)
                         (lambda () #f))
              ((x #:when (string? x))
               (call/native '(table insert) lines x)
               (loop #f))
              [e (buffer-name path)
                 (modified #f)
                 (if (= ".ss" (string-chop path -3))
                   (file-type 'scheme)
                   (file-type 'text))
                 #t])))
        (lambda (e)
          (error e)
          (call/native '(table insert) lines ""))))
    (begin
      (if (modified)
        (when (yes-or-no? "You have unsaved changes. Really open?")
          (modified #f)
          (open-file path))
        #f))))

(define save-file
  (case-lambda
    ((path)
     (catch
       (lambda ()
         (define h (call/native 'assert (call/native '(fs open) path "w")))
         (define lno (call/native '(table getn) lines))
         (let loop ((i 1))
           (if (> i lno)
             (call* h "close")
             (begin
               (call* h "writeLine" (hash-ref lines i))
               (loop (+ 1 i))))))
       (lambda (e) (error e))))
    (()
     (unless (= (string-ref (buffer-name 1)) "*")
       (save-file (buffer-name))))))

(define (write-line l)
  (if (= (file-type) 'scheme)
    (write-highlighted l)
    (term-write l)))

(define (redraw-text)
  (define cx (car cursor))
  (define cy (cdr cursor))
  (let loop ((y 1))
    (if (> y (- (height) 1))
      (term-cursor (- (car cursor) (car scroll))
                   (- (cdr cursor) (cdr scroll)))
      (begin
        (term-cursor (- 1 (car scroll)) y)
        (call/native '(term clearLine))
        (case (hash-ref lines (+ y (cdr scroll)))
          [#f #f]
          [line (write-line line)])
        (loop (+ 1 y))))))

(define (redraw-line y)
  (case (hash-ref lines y)
    [#f #f]
    [line
      (term-cursor (- 1 (car scroll)) (- y (cdr scroll)))
      (call/native '(term clearLine))
      (write-line line)
      (term-cursor (- (car cursor) (car scroll))
                   (- y (cdr scroll)))]))

(define (apply-to-line fun)
  (case (hash-ref lines (cdr cursor))
    [#f #f]
    [line (hash-set! lines (cdr cursor) (fun (car cursor) (cdr cursor) line))]))

(define (set-cursor! new-x new-y)
  (define x (car cursor))
  (define old-y (cdr cursor))
  (set-car! cursor new-x)
  (set-cdr! cursor new-y)
  (define screenx (- new-x (car scroll)))
  (define screeny (- new-y (cdr scroll)))
  (define redraw #f)
  (cond
    ((< screenx 1)
     (begin
       (set-car! scroll (- new-x 1))
       (set! screenx 1)
       (set! redraw #t)))
    ((> screenx (width))
     (begin
       (set-car! scroll (- new-x (width)))
       (set! screenx (width))
       (set! redraw #t))))
  (cond
    ((< screeny 1)
     (begin
       (set-cdr! scroll (- new-y 1))
       (set! screeny 1)
       (set! redraw #t)))
    ((> screeny (- (height) 1))
     (begin
       (set-cdr! scroll (- new-y (- (height) 1)))
       (set! screeny (- (height) 1))
       (set! redraw #t))))

  (cond
    (redraw (redraw-text))
    ((not (eq? (cdr cursor) old-y))
     (redraw-line old-y)
     (redraw-line (cdr cursor)))
    (#t (redraw-line (cdr cursor))))

  (term-cursor screenx screeny))

(define (modifier-mask)
  (+ (if (held? 'shift) #b100 0)
     (if (held? 'control) #b010 0)
     (if (held? 'meta) #b001 0)))

(define meta-mask    #b001)
(define control-mask #b010)
(define shift-mask   #b100)

(define key-with-mask
  (case-lambda
    (((key) #:when (number? key)) (+ (* key 8) (modifier-mask)))
    (((key mask) #:when (and (number? key)
                             (number? mask)))
     (+ (* key 8) mask))))

(define current-mode
  (begin
    (define mode 'normal)
    (case-lambda
      (() mode)
      ((x) (set! status-line-block 0)
           (set! mode x)))))

(define binding-table
  (make-hash-table `(#t ,(make-hash-table))
                   `(insert ,(make-hash-table))
                   `(normal ,(make-hash-table))))

(define bind-for-mode
  (case-lambda
    (((mode mask key action) #:when (and (number? mask)
                                         (symbol? key)
                                         (procedure? action)))
     (hash-set! (hash-ref binding-table mode)
                (key-with-mask (hash-ref keys (car key)) mask)
                action))
    ((mode key action)
     (bind-for-mode mode 0 key action))
    ((key action)
     (bind-for-mode #t 0 key action))))

(define (mode-has-binding key)
  (or (and (procedure? (hash-ref (hash-ref binding-table (current-mode))
                                 (key-with-mask key)))
           (hash-ref (hash-ref binding-table (current-mode))
                     (key-with-mask key)))
      (and (procedure? (hash-ref (hash-ref binding-table #t)
                                 (key-with-mask key)))
           (hash-ref (hash-ref binding-table #t)
                     (key-with-mask key)))))

(define (do-mode-binding key)
  (define x (mode-has-binding key))
  (if (procedure? x)
    (x (car cursor) (cdr cursor))))

(define (length-of-line i)
  (case (hash-ref lines i)
    [#f 0]
    [l (string-length l)]))

(load "/programs/editor/bindings.ss" ENV)

(define prompt-for-input
  (case-lambda
    ((prompt)
     (define x (list (term-cursor)))
     (term-cursor 1 (height))
     (call/native '(term clearLine))
     (display prompt)
     (parameterise ((repl-prompt prompt))
       (let ((r (interact-line)))
         (apply term-cursor x)
         r)))
    (() (prompt-for-input "> "))))

(define status-buffer-name-colour (make-parameter (hash-ref colours "white")))
(define status-file-type-colour   (make-parameter (hash-ref colours "white")))
(define status-insert-mode-colour (make-parameter (hash-ref colours "lightBlue")))
(define status-normal-mode-colour (make-parameter (hash-ref colours "lime")))

(define status-line-block 0)

(define (status-bar-message m)
  (set! status-line-block 5)
  (define x (list (term-cursor)))
  (term-cursor 1 (height))
  (call/native '(term clearLine))
  (display m)
  (apply term-cursor x))

(define (draw-status-line)
  (when (= status-line-block 0)
    (define x (list (term-cursor)))
    (term-cursor 1 (height))
    (call/native '(term clearLine))
    (case (current-mode)
      ('insert (term-set-text-colour (status-insert-mode-colour)))
      ('normal (term-set-text-colour (status-normal-mode-colour))))
    (display (current-mode))
    (term-set-text-colour (status-buffer-name-colour))
    (display " " (buffer-name))
    (let ((rhs (string-append (symbol->string (file-type))
                              " L" (cdr cursor))))
      (term-cursor (- (width) (string-length rhs)) (height))
      (term-set-text-colour (status-file-type-colour))
      (term-write rhs))
    (apply term-cursor x))
  (set! status-line-block (max 0 (- status-line-block 1))))

(define (yes-or-no? p)
  (let loop ((i (prompt-for-input (string-append p " (yes/no)"))))
    (cond
      ((or (= i "y") (= i "yes") (= i "Y")) #t)
      ((or (= i "n") (= i "no") (= i "N")) #f)
      (loop (prompt-for-input (string-append p " (yes/no)"))))))

(define (exit)
  (if (and (not (modified)) (not (eq? (buffer-name) "*scratch*")))
    (when (yes-or-no? "You have unsaved changes. Really quit?")
      (running #f))
    (running #f)))

(define (describe-proc proc)
  "Explain the procedure ,proc in a separate tab."
  (open-tab (lambda ()
              (explain proc)
              (display #\newline "Press any key to continue...")
              (call/native '(os pullEvent) "char")
              (close-tab))
            "pager"))

(call/native '(table insert) lines ";; This buffer is for text that is not saved.")
(call/native '(table insert) lines ";; To edit a file, visit it with")
(call/native '(table insert) lines ";;    M-x (open-file path) RET")
(call/native '(table insert) lines "")
(set-cursor! 1 4)

(define (editor-main . path)
  (call/native '(term setCursorBlink) #t)
  (call/native '(term clear))
  (if (not (null? path))
    (open-file (car path)))
  (redraw-text)
  (let loop ((ev (get-event-data)) (ignore #f))
    (cond
      ((running)
       (draw-status-line)
       (if ignore
         (loop (get-event-data) #f)
         (case ev
           [("key" k held)
            (cond
              [(mode-has-binding k)
               (do-mode-binding k)
               (loop (get-event-data) #t)]
              [else (loop (get-event-data) #f)])] 
           [(("char" k) #:when (= (current-mode) 'insert))
            (modified #t)
            (apply-to-line (lambda (x y line)
                             (string-append
                               (substring line 1 (- x 1))
                               k
                               (string-chop line x))))
            (set-cursor! (+ 1 (car cursor)) (cdr cursor))
            (loop (get-event-data) #f)]
           [else (loop (get-event-data) #f)])))
      (else
        (term-clear)
        (term-cursor 1 1)))))

(when (= (module-name) 'main)
  (editor-main))
