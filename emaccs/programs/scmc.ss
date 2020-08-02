(define (dissect-startup-file)
  (let ((handle (call/native 'assert (call/native '(io open) "boot/runtime.lua" "r"))))
    (define (loop l w)
      (if (catch
            (lambda () l)
            (lambda (e) #f))
        (cond
          ((= l "--{{{")
           (loop (call handle "read") (+ w 1)))
          ((= l "--}}}")
           (loop (call handle "read") (- w 1)))
          ((= w 0)
           (display l #\newline)
           (loop (call handle "read") 0))
          (else (display "-- " w #\newline)
                (loop (call handle "read") w)))))
    (loop (call handle "read") 0)))

(define output-file     (make-parameter #f))
(define include-runtime (make-parameter #f))
(define input-files     '())

(define (interp-cmdline s)
  (case s
    [("-o" out . s) (output-file out) (interp-cmdline s)]
    [("--no-rt" . s) (include-runtime #f) (interp-cmdline s)]
    [((p . s) #:when (and (string? p) (= #\- (string-ref p 1))))
     (error "Unrecognised option " p)]
    [((p . s) #:when (string? p))
     (set! input-files (cons p input-files))
     (interp-cmdline s)]))

(case (command-line)
  ['booting #t] ; scmc functionality is used for booting as well.
  [(program . opts)
   (interp-cmdline opts)
   (when (or (= (output-file) #f)
             (= input-files '()))
     (display "Usage: scmc -o path paths..." #\newline)
     (error))
   (with-output-to-file (output-file)
     (lambda ()
       (display "assert(load([==[" #\newline)
       (when (include-runtime)
         (display "  if not _phase then" #\newline)
         (dissect-startup-file)
         (display "  end" #\newline))
       (display "local function ignore(x) end" #\newline)
       (for-each compile-file (reverse input-files))
       (display "]==], nil, nil, setmetatable({},{__index=_ENV})))()")))])
