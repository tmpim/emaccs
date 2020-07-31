(define booting #t)

(define (dissect-startup-file)
  (let ((handle (call/native '(io open) "startup.lua" "r")))
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

(load "boot/case.ss")
(load "boot/r5rs.ss")
(load "boot/modules.ss")

(with-output-to-file
  "scheme51.lua"
  (lambda ()
    (begin
      (dissect-startup-file)
      (load "boot/compiler.ss")
      (let ((file (call/native '(io open) "operators.lua" "r")))
        (display (call file "read" "*a") #\newline)
        (call file "close"))
      (display "local function ignore(x) end" #\newline)
      (compile-file "boot/boot.ss")
      (compile-file "boot/case.ss")
      (compile-file "boot/r5rs.ss")
      (compile-file "boot/compiler.ss")
      (compile-file "boot/modules.ss")
      (if (and (hash-ref (hash-ref (environment) "os") "getenv")
               (call/native '(os getenv) "SCM_EXTRA"))
        (compile-file (call/native '(os getenv) "SCM_EXTRA"))))))
