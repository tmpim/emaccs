(define-syntax (with-file file path mode . body)
  (define r (gensym))
  `(let ((,file
          (call/native 'assert (call/native '(fs open) ,path ,mode))))
     (let ((,r (begin . ,body)))
       (call* ,file "close")
       ,r)))

(define current-version
  (if (call/native '(fs exists) ".version")
    (string->number (with-file handle ".version" "r"
                      (or (call* handle "readLine") -1)))
    -1))

(display "Current version: " current-version #\newline)

(define (get p)
  (let ((r (call/native 'assert (call/native '(http get) p))))
    (let ((s (call* r "readAll")))
      (call* r "close")
      s)))

(define (latest-version)
  (string->number
    (get "https://shamiko.amulet.works/job/Scheme%2051/lastSuccessfulBuild/artifact/version/*view*/")))

(define (get-event-data) (list (call/native '(os pullEvent))))

(define shade (call/native '(string char) 127))

(define (install)
  (define file-listing
    (get "https://shamiko.amulet.works/job/Scheme%2051/lastSuccessfulBuild/artifact/listing/*view*/"))
  (define iter (call/native '(string gmatch) file-listing "[^\n]+"))

  (define download-queue '())
  (define download-locs (make-hash-table))

  (catch (lambda ()
           (let loop ((line (or (iter) #f)))
             (when line
               (case (string-find line "[^%s]+")
                 [(start . end)
                  (define url (substring line 1 end))
                  (set! download-queue (cons url download-queue))
                  (hash-set! download-locs url (string-chop line (+ 2 end)))
                  (loop (or (iter) #f))]
                 [#f (error "Not a file listing line: " line)]))))
         (lambda (e) (unless (string? e) (error e))))

  (define to-dl (length download-queue))

  (display "To download: " to-dl " files" #\newline)
  (display "Installing...\n")

  (define (fly in-flight)
    (if (and (< in-flight 4) (pair? download-queue))
      (begin
        (define url (car download-queue))
        (call/native '(http request) url)
        (set! download-queue (cdr download-queue))
        (fly (+ 1 in-flight)))
      in-flight))

  (fly 0)

  (define timer (call/native '(os startTimer) 0.2))

  (let loop ((down 0) (ev (get-event-data)))
    (unless (and (= down to-dl) (null? download-queue))
      (define progress (floor (* 100 (inexact (/ down to-dl)))))
      (receive (x y) (call/native '(term getCursorPos))
        (call/native '(term setCursorPos) 1 y)
        (call/native '(term blit)
                     (call/native '(string rep) shade progress)
                     (call/native '(string rep) "f" progress)
                     (call/native '(string rep) "d" progress)))
      (case ev
        [("http_failure" url
          (error))]
        [("http_success" url http-h)
         (define path (hash-ref download-locs url))
         (define handle
           (call/native '(fs open) path "w"))
         (call* handle "writeLine" (call* http-h "readAll"))
         (call* handle "close")
         (call* http-h "close")
         (fly 3)
         (loop (+ 1 down) (get-event-data))]
        [("timer" id)
         (when (= id timer)
           (set! timer (call/native '(os startTimer) 0.2)))
         (loop down (get-event-data))]
        [else (loop down (get-event-data))])))
  (with-file h ".version" "w"
    (call* h "writeLine" latest)
    #t)
  (display "Recompiling... ")
  (load "/boot.ss")
  (display "done" #\newline))

(define latest (latest-version))

(cond
  ((= current-version -1)
   (install)
   (display "Rebooting..." #\newline)
   (call/native '(os reboot)))
  ((< current-version latest)
   (install)
   (newline))
  (else (display "up-to-date." #\newline)))
