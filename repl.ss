(use-modules (scm-51 string)
             (scm-51 input)
             (scm-51 control)
             (scm-51 gap-buffer))


(define (get-event-data)
  (call-with-values (lambda () (call/native '(os pullEvent)))
                    list))

(define keys (hash-ref (environment) "keys"))
(define repl-prompt (make-parameter "scheme> "))

(define repl-key-bindings (make-hash-table))

(define (bind key action)
  (hash-set! repl-key-bindings (hash-ref keys (car key)) action))

(define width (call-with-values (lambda () (call/native '(term getSize)))
                                (lambda (x y) x)))

(define height (call-with-values (lambda () (call/native '(term getSize)))
                                 (lambda (x y) y)))

(define (interact-line)
  (define buffer (make-gap-buffer "" 1))
  (define y (call-with-values (lambda () (call/native '(term getCursorPos)))
                              (lambda (x y) y)))
  (call/native '(term setCursorBlink) #t)
  (let loop ((ev (get-event-data)))
    ; clear the line and write the prompt
    (call/native '(term clearLine))
    (call/native '(term setCursorPos) 1 y)
    (write (repl-prompt))
    (write (car buffer) (cdr buffer))
    (call/native '(term setCursorPos)
                 (+ 1
                    (string-length (repl-prompt))
                    (string-length (car buffer)))
                 y)

    (case ev
      [("char" ch)
       (set! buffer (gap-buffer-insert buffer ch))
       (loop (get-event-data))]
      [("key" key held)
       (cond
         ((= key (hash-ref keys "enter"))
          (string-append (car buffer) (cdr buffer)))
         ((hash-ref repl-key-bindings key)
          (set! buffer ((hash-ref repl-key-bindings key) buffer))
          (loop (get-event-data)))
         (else (loop (get-event-data))))]
      [else (loop (get-event-data))])))

(bind 'left (lambda (buffer) (gap-buffer-left buffer 1)))
(bind 'right (lambda (buffer) (gap-buffer-right buffer 1)))
(bind 'backspace (lambda (buffer) (gap-buffer-delete-backwards buffer 1)))
(bind 'delete (lambda (buffer) (gap-buffer-delete buffer 1)))
(bind 'home gap-buffer-home)
(bind 'end gap-buffer-end)

(call/native '(term clear))
(call/native '(term setCursorPos) 1 1)
(define (clamp i limit)
  (if (> i limit)
    (- limit 1) i))

(define (scheme-interaction line)
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

(scheme-interaction 1)
