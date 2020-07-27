(use-modules (scm-51 string))

(define (input-from-string s)
  (define size (string-length s))
  (define cursor 0)
  (define (read handle i)
    (if (> (+ cursor i) size)
      (call/native 'rawget (make-hash-table) "oops! all nils")
      (begin
        (set! cursor (+ cursor i))
        (let ((x (string-slice s 1 i)))
          (set! s (string-chop s (+ 1 i)))
          x))))
  (make-hash-table
    (cons "read" read)
    (cons "close" (lambda () #t))))

(define (read-char port)
  ((hash-ref port "read") port 1))

(define (with-input-from-string x thunk)
  (with-input-from-file (input-from-string x) thunk))
