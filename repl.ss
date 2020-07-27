(use-modules (scm-51 string)
             (scm-51 input)
             (scm-51 control)
             (scm-51 gap-buffer)
             (srfi srfi-17))

(define (get-event-data)
  (call-with-values (lambda () (call/native '(os pullEvent)))
                    list))

(define keys (hash-ref (environment) "keys"))
(define colours (hash-ref (environment) "colours"))

(define repl-prompt (make-parameter "> "))
(define repl-highlighting-limit (make-parameter 50))

(define string-colour     (make-parameter (hash-ref colours "green")))
(define identifier-colour (make-parameter (hash-ref colours "white")))
(define macro-colour      (make-parameter (hash-ref colours "magenta")))
(define text-colour       (make-parameter (hash-ref colours "white")))
(define shadow-colour     (make-parameter (hash-ref colours "grey")))
(define comment-colour    (make-parameter (hash-ref colours "lightGrey")))
(define literal-colour    (make-parameter (hash-ref colours "yellow")))

(define repl-key-bindings (make-hash-table))

(define (bind key action)
  (hash-set! repl-key-bindings (hash-ref keys (car key)) action))

(define (width) (call/native '(term getSize)))

(define (height) (call-with-values (lambda () (call/native '(term getSize)))
                                   (lambda (x y) y)))

(define/native (over x y) "return _x / _y")
(define/native (modulo x y) "return _x % _y")

(define identifier-pattern "[a-zA-Z%+%-%?%!%=%>%<%*%/%%]+")

(define max
  (case-lambda
    ((x) x)
    ((x y) (if (> x y) x y))
    ((x y . z)
     (if (> x y)
       (apply max (cons x z))
       (apply max (cons y z))))))

(define min
  (case-lambda
    ((x) x)
    ((x y) (if (> x y) y x))
    ((x y . z)
     (if (> x y)
       (apply min (cons y z))
       (apply min (cons x z))))))

(define repl-history (make-hash-table))

(define/native (string-match str pattern)
  "return _str:match(_pattern) or false")

(define/native (string-find str pattern)
  "local start, fin = string.find(_str, _pattern)
   return start and {start,fin} or false")

(define held (make-hash-table '("shift" . #f) '("meta" . #f) '("control" . #f)))
(define (modifier? k)
  (hash-ref (make-hash-table
              (cons (hash-ref keys "leftShift") "shift")
              (cons (hash-ref keys "rightShift") "shift")
              (cons (hash-ref keys "leftControl") "control")
              (cons (hash-ref keys "rightControl") "control")
              (cons (hash-ref keys "leftAlt") "meta")
              (cons (hash-ref keys "rightAlt") "meta"))
            k))
(define (held? s)
  (hash-ref held (car s)))

(define shadow #f)

(define (refresh-editor-shadow point)
  (case (complete-identifier point)
    [(complete . rest)
     (set! shadow complete)]
    [t (set! shadow #f)]))

(define (write-token line colour pattern)
  (case (string-find line pattern)
    [(start . end)
     (call/native '(term setTextColour) (colour))
     (call/native '(term write) (string-slice line start end))
     (call/native '(term setTextColour) (text-colour))
     (string-chop line (+ 1 end))]
    [else #f]))

(define (write-ident-token line)
  (case (string-find line (string-append "^" identifier-pattern))
    [(start . end)
     (call/native '(term setTextColour)
       ((case (string-slice line start end)
          ["lambda" macro-colour]
          ["if" macro-colour]
          ["quote" macro-colour]
          ["set!" macro-colour]
          ["define" macro-colour]
          [(x #:when (hash-ref macros x)) macro-colour]
          [else identifier-colour])))
     (call/native '(term write) (string-slice line start end))
     (call/native '(term setTextColour) (text-colour))
     (string-chop line (+ 1 end))]
    [else #f]))

(define (write-highlighted line)
  (if (> (string-length line) (repl-highlighting-limit))
    (call/native '(term write) line)
    (let loop ((line line))
      (if (= line "")
        #t
        (loop (or (write-token line comment-colour    "^;.*")
                  (write-token line string-colour     "^\".-[^\\]\"")
                  (write-ident-token line)
                  (write-token line literal-colour    "^[0-9]+")
                  (write-token line literal-colour    "^#[tf]")
                  (write-token line string-colour     "^#\\%w+")
                  (write-token line text-colour       "^[^%w_]")
                  "no match"))))))

(define (interact-line)
  (define buffer (make-gap-buffer "" 1))
  (define y (call-with-values (lambda () (call/native '(term getCursorPos)))
                              (lambda (x y) y)))

  (call/native '(term setCursorBlink) #t)
  (let loop ((ev (get-event-data)))
    ; clear the line and write the prompt
    (call/native '(term clearLine))
    (call/native '(term setCursorPos) 1 y)
    (write-highlighted (repl-prompt))
    (let ((str (string-append (car buffer) (cdr buffer))))
      (define usable (- (width) (string-length (repl-prompt))))
      (define len (string-length str))
      (if (> len usable)
        (begin
          (define front (string-chop (car buffer) (- usable)))
          (write-highlighted
            (string-append front (string-chop (cdr buffer) 1 usable)))
          (call/native '(term setCursorPos)
                       (min (+ 1
                              (string-length (repl-prompt))
                              (string-length front))
                            (width))
                       y))
        (begin
          (write-highlighted str)
          (if shadow
            (begin
              (call/native '(term setTextColour) (shadow-colour))
              (call/native '(term write) shadow)
              (call/native '(term setTextColour) (text-colour))))
          (call/native '(term setCursorPos)
                        (+ 1
                           (string-length (repl-prompt))
                           (string-length (car buffer)))
                        y))))
    (case ev
      [("char" ch)
       (set! buffer (gap-buffer-insert buffer ch))
       (refresh-editor-shadow buffer)
       (loop (get-event-data))]
      [("key_up" key)
       (if (modifier? key)
         (hash-set! held (modifier? key) #f))
       (loop (get-event-data))]
      [("key" key ignored)
       (cond
         ((= key (hash-ref keys "enter"))
          (call/native '(table insert) repl-history buffer)
          (set! shadow #f) ; Dismiss the shadow
          (loop 'exit)) ; and draw one last time so it actually goes away
         ((modifier? key)
          (hash-set! held (modifier? key) #t)
          (loop (get-event-data)))
         ((hash-ref repl-key-bindings key)
          (set! buffer ((hash-ref repl-key-bindings key) buffer))
          (loop (get-event-data)))
         (else (loop (get-event-data))))]
      ['exit (string-append (car buffer) (cdr buffer))]
      [else (loop (get-event-data))])))

(bind 'backspace (lambda (buffer)
                   (let ((buf (gap-buffer-delete-backwards buffer 1)))
                     (refresh-editor-shadow buf)
                     (gap-buffer-delete-backwards buffer 1))))
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
               (set! shadow #f)
               (gap-buffer-right buffer 1)))

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

(define-syntax (stream-cons a b)
  `(cons ,a (lambda () ,b)))

(define stream-car car)
(define (stream-cdr c)
  (if (null? c)
    '()
    ((cdr c))))

(define (stream-yield x)
  (shift1 k (stream-cons x (k #f))))

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

(define (complete-identifier point)
  (let ((ident (string-match (car point) (string-append identifier-pattern "$"))))
    (cond
      ((not ident) '())
      (else
        (define already-typed (string-length ident))
        (reset
          ; Search macros
          (hash-for-each
            macros
            (lambda (name value)
              (if (= (string-slice name 1 (string-length ident)) ident)
                (stream-yield
                  (string-chop name (+ 1 already-typed))))))
          (set! ident (escape-symbol (call/native 'symbol ident)))
          ; Search bindings
          (hash-for-each
            (environment)
            (lambda (name value)
              (if (= (string-slice name 1 (string-length ident)) ident)
                (stream-yield
                  (string-chop (unescape-symbol name)
                               (+ 1 already-typed))))))
          '())))))

(define (clamp i limit)
  (if (> i limit)
    (- limit 1) i))

(define (scheme-interaction line)
  (set! shadow #f)
  (let ((input (interact-line)))
    (write #\newline)
    (catch
      (lambda ()
        (write (eval (with-input-from-file
                       (input-from-string (string-append input "\n"))
                       read))
            #\newline))
      (lambda (e)
        (write "Scheme error: ")
        (write e)
        (write #\newline)))
    (scheme-interaction (+ 2 line))))

(call/native '(term clear))
(call/native '(term setCursorPos) 1 1)
(write ";; Scheme interaction\n")

(scheme-interaction 1)
