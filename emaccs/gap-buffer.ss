(use-modules (scm-51 string))

(define (make-gap-buffer str init-cursor)
  (cons (substring str 1 init-cursor)
        (string-chop str (+ 1 init-cursor))))

(define (gap-buffer-insert buf s)
  (cons (string-append (car buf) s)
        (cdr buf)))

(define (gap-buffer-delete buf i)
  (cons (car buf)
        (string-chop (cdr buf) (+ 1 i))))

(define (gap-buffer-delete-backwards buf i)
  (cons (substring (car buf) 1 (- (string-length (car buf)) i))
        (cdr buf)))

(define (gap-buffer-left buf i)
  (let ((first (car buf))
        (second (cdr buf)))
    (cons (substring first 1 (- (string-length first) i))
          (string-append
            (string-chop first (+ 1 (- (string-length first) i)))
            second))))

(define (gap-buffer-right buf i)
  (let ((first (car buf))
        (second (cdr buf)))
    (cons (string-append
            first
            (substring second 1 i))
          (string-chop second (+ 1 i)))))

(define (gap-buffer-home buf)
  (cons "" (string-append (car buf) (cdr buf))))

(define (gap-buffer-end buf)
  (cons (string-append (car buf) (cdr buf)) ""))

(define (gap-buffer-kill-backwards buf)
  (case (string-find (car buf) "%w+%s*$")
    [(start . end)
     (cons (substring (car buf) 0 (- start 1)) (cdr buf))]
    [false buf]))
