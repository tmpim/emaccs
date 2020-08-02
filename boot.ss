(define booting #t)
(define (command-line) 'booting)

(load "boot/case.ss")
(load "boot/r5rs.ss")
(load "boot/modules.ss")

(load "emaccs/programs/scmc.ss") ; for dissect-startup-file

(with-output-to-file
  "scheme51.lua"
  (lambda ()
    (begin
      (dissect-startup-file)
      (load "boot/compiler.ss")
      (display "local function ignore(x) end" #\newline)
      (compile-file "boot/boot.ss")
      (compile-file "boot/case.ss")
      (compile-file "boot/r5rs.ss")
      (compile-file "boot/compiler.ss")
      (compile-file "boot/modules.ss")
      (if (and (hash-ref (hash-ref (environment) "os") "getenv")
               (call/native '(os getenv) "SCM_EXTRA"))
        (compile-file (call/native '(os getenv) "SCM_EXTRA"))))))
