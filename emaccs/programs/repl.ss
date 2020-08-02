(use-modules (scm-51 string)
             (scm-51 input)
             (scm-51 control)

             (emaccs programs repl gap-buffer)
             (emaccs terminal))

(define (get-event-data)
  (call-with-values (lambda () (call/native '(os pullEvent)))
                    list))

(define repl-prompt             (make-parameter "> "))
(define repl-highlighting-limit (make-parameter 50))

(define string-colour        (make-parameter (hash-ref colours "green")))
(define prompt-colour        (make-parameter (hash-ref colours "white")))
(define identifier-colour    (make-parameter (hash-ref colours "white")))
(define macro-colour         (make-parameter (hash-ref colours "magenta")))
(define text-colour          (make-parameter (hash-ref colours "white")))
(define shadow-colour        (make-parameter (hash-ref colours "grey")))
(define comment-colour       (make-parameter (hash-ref colours "lightGrey")))
(define literal-colour       (make-parameter (hash-ref colours "yellow")))
(define defined-ident-colour (make-parameter (hash-ref colours "lightBlue")))

(define repl-key-bindings (make-hash-table))

(define (bind key action)
  (hash-set! repl-key-bindings (hash-ref keys (car key)) action))

(define identifier-pattern
  "[a-zA-Z%+%-%?%!%=%>%<%*%/%%ç][a-zA-Z0-9%+%-%?%!%=%>%<%*%/%%ç]*")

(define repl-history (make-hash-table))

(define shadow #f)

(define-syntax (stream-cons a b)
  `(cons ,a (lambda () ,b)))

(define stream-car car)
(define (stream-cdr c)
  (cond
    ((null? c) '())
    ((procedure? (cdr c))
     ((cdr c)))
    (else (cdr c))))

(define (stream-yield x)
  (shift1 k (stream-cons x (k #f))))

(define stream-append
  (begin
    (define (append2 xs ys)
      (cond
        ((null? xs) ys)
        ((pair? xs) (stream-cons (car xs) (append (stream-cdr xs) ys)))))
    (case-lambda
      (() '())
      ((as) as)
      ((as bs . cs) (append2 as (apply stream-append (cons bs cs)))))))

(define (complete-identifier point)
  (case (string-match (car point) (string-append identifier-pattern "$"))
    [#f #f]
    (ident
      (define already-typed (string-length ident))
      (reset
        ; Search macros
        (hash-for-each
          macros
          (lambda (name value)
            (if (= (substring name 1 (string-length ident)) ident)
              (stream-yield
                (string-chop name (+ 1 already-typed))))))
        (set! ident (escape-symbol (call/native 'symbol ident)))
        ; Search bindings
        (hash-for-each
          ENV
          (lambda (name value)
            (if (= (substring name 1 (string-length ident)) ident)
              (stream-yield
                (string-chop (unescape-symbol name)
                             (+ 1 already-typed))))))
        '()))))

(define (complete-filename point)
  (case (string-match (car point) "/[%w/]+$")
    [#f '()]
    [path
      (define names (call/native '(fs find) (string-append path "*")))
      (reset (hash-for-each names
               (lambda (num name)
                 (if (call/native '(fs isDir) name)
                   (stream-yield (string-append (string-chop name (string-length path)) "/"))
                   (stream-yield (string-chop name (string-length path)))))))]))

(define (refresh-editor-shadow point)
  (when (= (cdr point) "")
    (set! shadow (stream-append (complete-filename point)
                                (complete-identifier point)))))

(define (write-token line colour pattern)
  (case (string-find line pattern)
    [(start . end)
     (term-set-text-colour (colour))
     (term-write (substring line start end))
     (term-set-text-colour (text-colour))
     (list (string-chop line (+ 1 end)) end)]
    [else #f]))

(define (make-token line colour pattern)
  (case (string-find line pattern)
    [(start . end)
     (list (string-chop line (+ 1 end)) (substring line start end) end (colour))]
    [else #f]))

(define (write-ident-token line)
  (case (string-find line (string-append "^" identifier-pattern))
    [(start . end)
     (define s (substring line start end))
     (if (string->number s)
       (term-set-text-colour (literal-colour))
       (term-set-text-colour
         ((case (substring line start end)
            ["lambda" macro-colour]
            ["if" macro-colour]
            ["quote" macro-colour]
            ["set!" macro-colour]
            ["define" macro-colour]
            [(x #:when (hash-ref macros x)) macro-colour]
            [(x #:when (hash-ref ENV (escape-symbol
                                       (string->symbol x))))
             defined-ident-colour]
            [else identifier-colour]))))
     (term-write (substring line start end))
     (term-set-text-colour (text-colour))
     (list (string-chop line (+ 1 end)) end)]
    [else #f]))

(define (write-highlighted line)
  (let loop ((line line))
    (if (= line "")
      #t
      (apply loop (or (write-token line comment-colour    "^;.*")
                      (write-token line string-colour     "^\".-[^\\]\"")
                      (write-token line literal-colour    "^-?[0-9/%.]+")
                      (write-token line literal-colour    "^#[tf]")
                      (write-token line string-colour     "^#\\%w+")
                      (write-ident-token line)
                      (write-token line string-colour     "^\".-[^\\]$")
                      (write-token line text-colour       "^.")
                      (list "no match"))))))

(define delimiters
  (make-hash-table (cons " " #t)
                   (cons "(" #t)
                   (cons ")" #t)))

(define (repaint)
  #f)

(define repaint-tasks '())

(define (draw-editor-shadow)
  (term-set-text-colour (shadow-colour))
  (cond
    ((pair? shadow)
     (term-write (car shadow)))
    ((string? shadow)
     (term-write shadow))
    ((null? shadow) #f)
    (else (display shadow)))
  (term-set-text-colour (text-colour)))

(define (interact-line)
  (define buffer (make-gap-buffer "" 1))
  (define y (call-with-values (lambda () (term-get-cursor-pos))
                              (lambda (x y) y)))
  (define needs-repaint #t)
  (define (out-of-bounds)
    (define usable (- (width) (string-length (repl-prompt)) 1))
    (define len (string-length (string-append (car buffer) (cdr buffer))))
    (> len usable))

  (set! history-pos #f)
  (set! repaint (case-lambda (() (set! needs-repaint #t))
                             ((x) (set! needs-repaint #f))))

  (call/native '(term setCursorBlink) #t)
  (let loop ((ev (get-event-data)))
    ; clear the line and write the prompt
    (when needs-repaint
      (term-clear-line)
      (term-set-cursor-pos 1 y)
      (write-token (repl-prompt) prompt-colour ".*")
      (when (not (null? repaint-tasks))
        (map (lambda (p) (p)) repaint-tasks)
        (set! repaint-tasks '())
        (loop ev))
      (let ((str (string-append (car buffer) (cdr buffer))))
        (if (out-of-bounds)
          (begin
            (define usable (- (width) (string-length (repl-prompt)) 1))
            (set! needs-repaint #t)
            (define front (string-chop (car buffer) (- usable)))
            (write-highlighted
              (string-append front (cdr buffer)))
            (term-set-cursor-pos
               (min (+ 1
                      (string-length (repl-prompt))
                      (string-length front))
                    (width))
               y))
          (begin
            (set! needs-repaint #f)
            (write-highlighted str)
            (when shadow (draw-editor-shadow))
            (term-set-cursor-pos
                (+ 1
                   (string-length (repl-prompt))
                   (string-length (car buffer)))
                y)))))
    (case ev 
      [(("char" ch) #:when (hash-ref repl-key-bindings ch))
       (set! needs-repaint #t)
       (set! buffer ((hash-ref repl-key-bindings key) buffer))
       (refresh-editor-shadow buffer)
       (loop (get-event-data))]
      [("char" ch)
       (when (or (hash-ref delimiters ch)
                 (out-of-bounds)
                 (not (= (cdr buffer) ""))
                 shadow)
         (set! needs-repaint #t))
       (set! buffer (gap-buffer-insert buffer ch))
       (unless needs-repaint
         (term-write ch))
       (refresh-editor-shadow buffer)
       (loop (get-event-data))]
      [("key" key ignored)
       (cond
         ((= key (hash-ref keys "enter"))
          (call/native '(table insert) repl-history buffer)
          (set! shadow #f)        ; Dismiss the shadow
          (set! needs-repaint #t) ; Repaint the editor
          (loop 'exit))           ; and draw one last time so it actually goes away
         ((hash-ref repl-key-bindings key)
          (set! needs-repaint #t)
          (set! buffer ((hash-ref repl-key-bindings key) buffer))
          (loop (get-event-data)))
         (else (loop (get-event-data))))]
      ['exit (string-append (car buffer) (cdr buffer))]
      [else (loop (get-event-data))])))

(bind 'backspace (lambda (buffer)
                   (let ((buf (if (held? 'control)
                                (gap-buffer-kill-backwards buffer 1)
                                (gap-buffer-delete-backwards buffer 1))))
                     (refresh-editor-shadow buf)
                     buf)))
(bind 'delete (lambda (buffer) (gap-buffer-delete buffer 1)))
(bind 'home gap-buffer-home)
(bind 'end gap-buffer-end)

(define history-pos #f)

; Dismiss editor shadow when moving the point
; (otherwise: the completion would be rendered inside the word)

(bind 'left (lambda (buffer)
              (set! shadow #f)
              (gap-buffer-left buffer 1)))
(bind 'right (lambda (buffer)
               (case shadow
                 [(x . y)
                  (let ((buffer (gap-buffer-insert buffer x)))
                    (refresh-editor-shadow buffer)
                    buffer)] ; accept completion
                 [else
                   (set! shadow #f)
                   (gap-buffer-right buffer 1)])))

(bind 'up (lambda (buffer)
            (set! shadow #f)
            (if (not history-pos)
              (set! history-pos (call/native '(table getn) repl-history))
              (set! history-pos (- history-pos 1)))
            (hash-ref repl-history history-pos (cons "" ""))))

(bind 'down (lambda (buffer)
              (set! shadow #f)
              (cond
                ((= history-pos (call/native '(table getn) repl-history))
                 (set! history-pos #f))
                (history-pos (set! history-pos (+ history-pos 1))))
              (hash-ref repl-history history-pos (cons "" ""))))

(bind 'w (lambda (buffer)
           (if (held? 'control)
             (let ((buf (gap-buffer-kill-backwards buffer)))
               (refresh-editor-shadow buf)
               buf)
             buffer)))

; cycle through shadows
(bind 'tab (lambda (buffer)
             (case shadow
               [() (refresh-editor-shadow buffer)]
               [(a . b)
                (set! shadow (b))])
             buffer))

(define (deep-seq s)
  (if (null? s)
    s
    (cons (car s) (deep-seq (stream-cdr s)))))

(define (unescape-symbol s)
  (call/native
    '(string gsub)
    (string-chop s 2)
    "S([0-9][0-9])"
    (lambda (c) (call/native '(string char) (call/native 'tonumber c)))))

(define (clamp i limit)
  (if (> i limit)
    (- limit 1) i))

(define (scheme-interaction line)
  "Run the fancy REPL."
  (set! shadow #f)
  (let ((input (interact-line)))
    (display #\newline)
    (catch
      (lambda ()
        (write (eval (with-input-from-file
                       (input-from-string (string-append input "\n"))
                       read)
                     ENV))
        (newline))
      (lambda (e)
        (write "Error:")
        (if (string? e)
          (case (string-find e ": ")
            [(start . end)
             (write (string-chop e (+ 1 start)))]
            [else (write e)])
          (write e))
        (display #\newline)))
    (scheme-interaction (+ 2 line))))

(define (explain proc)
  "Nicely format and write the documentation for ,proc. ,proc must be a
  procedure object with associated documentation, i.e., of the form

    (lambda args \"docs\" ...)"
  (define (first-token line)
    (or (make-token line literal-colour       "^[%s]+,%w+")
        (make-token line defined-ident-colour "^[%s]+'%w+")
        (make-token line literal-colour       "^[%s]+#%w+")
        (make-token line (lambda () 'spaces)  "^%s+")
        (make-token line text-colour          "^[%w']+")
        (make-token line text-colour          "^[^%w_]+")))
  (case (documentation-for-procedure proc)
    [#f #f]
    [s
      (display "Procedure arguments: " #\newline)
      (display "  " (hash-ref proc "args") "\n\n")
      (display "Documentation:\n  ")
      (let loop ((token (first-token s)) (written 2) (lines 0))
        (case token
          [#f
           (display #\newline)
           (+ 4 lines)]
          [((r t size c) #:when (>= (+ size written) (width)))
           (display #\newline "  ")
           (loop (list r t size c) 2 (+ 1 lines))]
          [(rest t s 'spaces)
           (display " ")
           (loop (first-token rest) (+ written 1) lines)]
          [(rest text size colour)
           (term-set-text-colour colour)
           (term-write text)
           (term-set-text-colour (text-colour))
           (loop (first-token rest) (+ written size) lines)]))]))

(when (= (module-name) 'main)
  ; (term-clear)
  ; (term-set-cursor-pos 1 1)
  (display ";; Scheme interaction\n")
  (display (repl-prompt))
  ; the line editor waits for an event to paint the first time.
  ; if we write the prompt here, it'll be painted over on the first event.
  ; no harm done

  (scheme-interaction 1))
