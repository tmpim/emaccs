;; (emaccs wm)
;; a tabbed window manager for computercraft.

(use-modules (emaccs terminal)
             (emaccs thread)
             (srfi srfi-8))

(define (get-event-data) (list (call/native '(os pullEventRaw))))

(define native-terminal (call/native '(term native)))
(define (native-height)
  (call-with-values (lambda () ((hash-ref native-terminal "getSize")))
                    (lambda (x y) y)))

(define active-tabs (make-hash-table))

(define active-tab
  (begin
    (define current #f)
    (case-lambda
      "Either set the current tab to the given number or return the current tab."
      (((x) #:when (number? x))
       (when (and current (not (eq? current x)) (hash-ref active-tabs current))
         ((hash-ref (hash-ref (hash-ref active-tabs current) "window") "setVisible") #f))
       (set! current x)
       (draw-tab-line))
      (('num) current)
      (() (hash-ref active-tabs current)))))

(define (tab-count) (call/native '(table getn) active-tabs))

(define (draw-tab-line)
  (define x (call* native-terminal "getCursorBlink"))
  (call* native-terminal "setCursorBlink" #f)
  (call* native-terminal "setCursorPos" 1 (native-height))
  (call* native-terminal "clearLine")
  (hash-for-each active-tabs (lambda (k v)
                               (if (= v (active-tab))
                                 (call* native-terminal "setTextColour"
                                              (hash-ref colours "red")))
                               (call* native-terminal "write" (hash-ref v "title"))
                               (call* native-terminal "write" " ")
                               (call* native-terminal "setTextColour" 1)))
  (call* native-terminal "setCursorBlink" x))

(define (make-environment)
  (define env (make-hash-table `("_tab" ,(tab-count))))
  (call/native 'setmetatable
               env
               (make-hash-table
                 (cons "__index" (environment))))
  env)

(define (open-tab . args)
  (define prog
    (if (null? args)
      "/programs/repl.ss"
      (car args)))
  (define env (make-environment))
  (define thread
    (coroutine-create (lambda () (load prog env))))
  (define window
    (call/native '(window create)
                 native-terminal
                 1 1
                 (width) (- (native-height) 1) #t))

  (call/native '(table insert)
               active-tabs
               (make-hash-table (cons "thread" thread)
                                (cons "title" (or (and (null? args) "scheme")
                                                  (car args)))
                                (cons "window" window)))
  (active-tab (tab-count)))

(define held 
  (make-hash-table '("shift" . #f) '("meta" . #f) '("control" . #f)))

(define (modifier? k)
  (hash-ref (make-hash-table
              (cons (hash-ref keys "leftShift") "shift")
              (cons (hash-ref keys "rightShift") "shift")
              (cons (hash-ref keys "leftCtrl") "control")
              (cons (hash-ref keys "rightCtrl") "control")
              (cons (hash-ref keys "leftAlt") "meta")
              (cons (hash-ref keys "rightAlt") "meta"))
            k))

(define (held? s)
  (hash-ref held (car s)))

(define (resume ev)
  (let ((x (active-tab)))
    (call* (hash-ref x "window") "setVisible" #t)
    (call* (hash-ref x "window") "redraw")
    (call/native '(term redirect) (hash-ref x "window"))
    (case (list (apply coroutine-resume (cons (hash-ref x "thread") ev)))
      [(#t . ev)]
      [(#f . err)
       (call/native '(table remove) active-tabs (active-tab 'num))
       (prev-tab)])))

(define (next-tab)
  (if (> (+ 1 (active-tab 'num)) (tab-count))
    (active-tab 1)
    (active-tab (+ 1 (active-tab 'num))))
  (draw-tab-line))

(define (prev-tab)
  (if (< (- (active-tab 'num) 1) 0)
    (active-tab (tab-count))
    (active-tab (- (active-tab 'num) 1)))
  (draw-tab-line))

(define (resume* ev)
  (define lim (tab-count))
  (define dead '())
  (let loop ((x 1))
    (if (> x lim)
      #t
      (let ((tab (hash-ref active-tabs x)))
        (call/native '(term redirect) (hash-ref tab "window"))
        (case (list (apply coroutine-resume (cons (hash-ref tab "thread") ev)))
          [(#t . ev)]
          [(#f . err)
           (set! dead (cons x dead))])
        (loop (+ 1 x)))))
  (map (lambda (x)
         (hash-set! active-tabs x #f))
       dead))

(define (run)
  (let loop ((ev (get-event-data)))
    (if (<= (tab-count) 0)
      (open-tab))
    (case ev
      [("key" key . h)
       (if (modifier? key)
         (hash-set! held (modifier? key) #t))
       (resume ev)]
      [("key_up" key)
       (cond 
         ((modifier? key)
          (hash-set! held (modifier? key) #f)
          (resume ev))
         ((and (= key (hash-ref keys "tab"))
               (held? 'control))
          (if (held? 'shift)
            (prev-tab)
            (next-tab)))
         ((and (= key 49)
               (held? 'control))
          (open-tab))
         (else (resume ev)))]
      [("char" c)
       (cond
         ((string->number c)
          (define tn (string->number c))
          (if (and (<= 1 tn (tab-count))
                   (held? 'control))
            (begin
              (active-tab tn)
              (draw-tab-line))
            (resume ev)))
         (else (resume ev)))]
      [("terminate") (resume ev)]
      [else (resume* ev)])
    (loop (get-event-data))))

(open-tab)

(call* native-terminal "clear")
(call* native-terminal "setCursorPos" 1 1)

(active-tab 1)
(draw-tab-line)

(run)
