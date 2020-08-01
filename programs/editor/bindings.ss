(bind-for-mode 'up
  (lambda (x y)
    (if (> y 1)
      (set-cursor! (min x (+ 1 (length-of-line (- y 1))))
                   (- y 1)))))

(bind-for-mode 'down
  (lambda (x y)
    (if (< y (line-count))
      (set-cursor! (min x (+ 1 (length-of-line (+ y 1))))
                   (+ y 1)))))

(bind-for-mode 'left
  (lambda (x y) (set-cursor! (max 1 (- x 1)) y)))

(bind-for-mode 'right
  (lambda (x y) (set-cursor! (min (+ 1 (length-of-line y)) (+ x 1)) y)))

(bind-for-mode 'home (lambda (x y) (set-cursor! 1 y)))
(bind-for-mode 'end (lambda (x y) (set-cursor! (length-of-line y) y)))

(bind-for-mode 'pageUp
  (lambda (x y)
    (define jump-y (max 1 (- y (inexact (/ (height) 2)))))
    (set-cursor! (min x (+ 1 (length-of-line jump-y))) jump-y)))

(bind-for-mode 'pageDown
  (lambda (x y)
    (define jump-y (max 1 (+ y (inexact (/ (height) 2)))))
    (set-cursor! (min x (+ 1 (length-of-line jump-y))) jump-y)))

(bind-for-mode 'normal 'i (lambda () (current-mode 'insert)))

(bind-for-mode 'normal 'a
  (lambda (x y)
    (set-cursor! (min (+ 1 (length-of-line y)) (+ x 1)) y)
    (current-mode 'insert)))

(bind-for-mode 'normal #b100 'a
  (lambda (x y)
    (set-cursor! (+ 1 (length-of-line y)) y)
    (current-mode 'insert)))

(bind-for-mode 'normal 'o
  (lambda (x y)
    (call/native '(table insert) lines (+ y 1) "")
    (set-cursor! 1 (+ y 1))
    (current-mode 'insert)))

(bind-for-mode 'normal #b100 'o
  (lambda (x y)
    (call/native '(table insert) lines y "")
    (set-cursor! 1 y)
    (redraw-text)
    (current-mode 'insert)))

(bind-for-mode 'normal #b100 'c
  (lambda (x y)
    (hash-set! lines y (substring (hash-ref lines y) 1 (- x 1)))
    (redraw-line y)
    (current-mode 'insert)))

(bind-for-mode 'normal #b010 'c
  (lambda (x y)
    (status-bar-message "Use M-x (exit) to exit the editor.")))

(bind-for-mode 'normal 'c
  (lambda (x y)
    (case (get-text-object "c" x y)
      [(dir start . end)
       (hash-set! lines y (string-append
                            (substring (hash-ref lines y) 0 (- start 1))
                            (string-chop (hash-ref lines y) end)))
       (set-cursor! start y)
       (current-mode 'insert)]
      [#f #f])))

(bind-for-mode 'normal 'd
  (lambda (x y)
    (case (get-text-object "d" x y)
      [(dir start . end)
       (hash-set! lines y (string-append
                            (substring (hash-ref lines y) 0 (- start 1))
                            (string-chop (hash-ref lines y) end)))
       (set-cursor! start y)
       (redraw-line y)]
      [#f #f])))

(bind-for-mode 'insert #b010 'c (lambda () (current-mode 'normal) #t))

(bind-for-mode 'insert 'enter
  (lambda (x y)
    (define line (hash-ref lines y))
    (hash-set! lines y (substring line 1 (- x 1)))
    (call/native '(table insert)
                 lines
                 (+ y 1)
                 (string-chop line x))
    (set-cursor! 1 (+ y 1))
    (redraw-text)))

(bind-for-mode 'insert 'backspace
  (lambda (x y)
    (cond
      ((> x 1)
       (hash-set! lines y
                  (string-append (substring (hash-ref lines y) 1 (- x 2))
                                 (string-chop (hash-ref lines y) x)))
       (set-cursor! (max 1 (- x 1)) y))
      ((> y 1)
       (define prevlen (length-of-line (- y 1)))
       (hash-set! lines (- y 1) (string-append (hash-ref lines (- y 1))
                                               (hash-ref lines y)))
       (call/native '(table remove) lines y)
       (set-cursor! (+ 1 prevlen) (- y 1))
       (redraw-text)))))

(bind-for-mode 'normal #b001 'x
  (lambda (x y)
    (call/native '(os pullEvent) "char")
    (define cmd
      (with-input-from-string
        (string-append (prompt-for-input "M-x ") " ")
        read))
    (catch (lambda () (status-bar-message (eval cmd ENV)))
           (lambda e  (status-bar-message e)))
    (redraw-text)))

(bind-for-mode 'insert #b010 't
  (lambda (x y)
    (hash-set! lines y (string-append
                         (call/native '(string rep) " " (tab-stop))
                         (hash-ref lines y)))
    (set-cursor! (+ x (tab-stop)) y)))

(bind-for-mode 'insert #b010 'd
  (lambda (x y)
    (define indent (call/native '(string rep) " " (tab-stop)))
    (define line (hash-ref lines y))
    (if (= indent (substring line 1 (tab-stop)))
      (begin
        (hash-set! lines y (string-chop line (+ 1 (tab-stop))))
        (set-cursor! (- x (tab-stop)) y))
      #t)))

(begin
  (define (key-downcase c)
    (or (and (= c "$") "four")
        (string-downcase c)))

  (hash-for-each text-object-table
    (lambda (obj proc)
      (define mask (if (eq? (string-upcase obj) obj) shift-mask #b000))
      (bind-for-mode
        'normal
        mask
        (string->symbol (key-downcase obj))
        (lambda (x y)
          (catch
            (lambda ()
              (define it (proc x y))
              (status-bar-message it)
              (if it
                (if (= (car it) 'fore)
                  (set-cursor! (cddr it) y)
                  (set-cursor! (cadr it) y))))
            (lambda e #f)))))))
