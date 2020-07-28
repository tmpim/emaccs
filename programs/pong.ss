(use-modules (srfi srfi-1)
             (scm-51 string)
             (emaccs terminal))

(define (get-event-data)
  (call-with-values (lambda () (call/native '(os pullEvent)))
                    list))

(define (write-score player score)
  (case player
    ['left
     (term-cursor 1 1)
     (term-write "P1: ")
     (term-write (number->string score))]
    ['right
     (let ((score (string-append "P2: " (number->string score))))
       (term-cursor (- (width) (string-length score)) 1)
       (term-write score))]))

(define (draw-paddle side center)
  (term-set-background-colour (hash-ref colours "white"))
  (let loop ((i (- center 2)))
    (if (= i (+ center 2))
      (term-set-background-colour (hash-ref colours "black"))
      (begin
        (term-cursor (if (= side 'left) 1 (width))
                     i)
        (term-write " ")
        (loop (+ 1 i))))))

(define (draw-ball ball)
  (term-cursor (caar ball) (cdar ball))
  (term-set-background-colour (hash-ref colours "white"))
  (term-write " ")
  (term-set-background-colour (hash-ref colours "black")))

(define (clamp low high x)
  (cond ((< x low) low)
        ((> x high) high)
        (else x)))

(define (draw-point-screen player)
  (define x (string-append player " SCORE!"))
  (term-clear)
  (term-cursor (exact->inexact (/ (- (width) (string-length x)) 2))
               (exact->inexact (/ (height) 2)))
  (term-write x)
  (call/native 'sleep 2))

(define init-paddle (call/native '(math floor)
                                 (exact->inexact (/ (height) 2))))

(define ai-player-left  (make-parameter #f))
(define ai-player-right (make-parameter #f))
(define ai-player-uncertainty (make-parameter #f))
(define ball-speed-modifier (make-parameter 1))

(define (quit-pong-and-die)
  (term-clear)
  (term-cursor 1 1)
  (write "Thank you for playing!\n"))

(define (pong timer left right ball)
  (term-clear)
  (term-cursor 1 1)
  (write-score 'left (car left))
  (write-score 'right (car right))
  (term-cursor 1 2)

  (when (ai-player-left) (set-cdr! left (cdar ball)))
  (when (ai-player-right) (set-cdr! right (cdar ball)))

  (draw-paddle 'left (cdr left))
  (draw-paddle 'right (cdr right))

  (draw-ball ball)

  (define step
    (case-lambda
      [((left-score . left-paddle)
        (right-score . right-paddle)
        ((ball-x . ball-y) . (ball-vx . ball-vy)))
       (let ((new-x (+ ball-x ball-vx))
             (new-y (+ ball-y ball-vy)))
         (cond
           ((and (>= new-x (- (width) 1))
                 (< (- right-paddle 2) new-y (+ right-paddle 2)))
            (pong
              (call/native '(os startTimer) 0.1)
              (cons left-score left-paddle)
              (cons right-score right-paddle)
              (cons (cons new-x new-y)
                    (cons (- ball-vx) ball-vy))))
           ((and (<= new-x 2)
                 (< (- left-paddle 2) new-y (+ left-paddle 2)))
            (pong
              (call/native '(os startTimer) 0.1)
              (cons left-score left-paddle)
              (cons right-score right-paddle)
              (cons (cons new-x new-y)
                    (cons (- ball-vx) ball-vy))))
           ((>= new-x (width))
            (draw-point-screen "PLAYER 1")
            (pong
              (call/native '(os startTimer) 0.1)
              (cons (+ 1 right-score) init-paddle)
              (cons left-score init-paddle)
              (init-ball)))
           ((<= new-x 0)
            (draw-point-screen "PLAYER 2")
            (pong
              (call/native '(os startTimer) 0.1)
              (cons (+ 1 right-score) init-paddle)
              (cons left-score init-paddle)
              (init-ball)))
           (else
             (define new-vy (if (or (> new-y (height))
                                    (< new-y 1))
                              (- ball-vy)
                              ball-vy))
             (pong
               (call/native '(os startTimer) 0.1)
               (cons left-score left-paddle)
               (cons right-score right-paddle)
               (cons
                 (cons new-x (clamp 1 (height) new-y))
                 (cons ball-vx new-vy))))))]
      [e (write e #\newline)]))

  (let ((ev (get-event-data)))
    (case ev
      [("key" x held)
       (case ((hash-ref keys "getName") x)
         ["w" (pong timer (cons (car left) (- (cdr left) 1))
                    right ball)]
         ["s" (pong timer (cons (car left) (+ 1 (cdr left)))
                    right ball)]

         ["up" (pong timer left
                     (cons (car right) (- (cdr right) 1))
                     ball)]
         ["down" (pong timer left
                       (cons (car right) (+ (cdr right) 1))
                       ball)]
         ["q" (quit-pong-and-die)]
         [else (pong timer left right ball)])]
      [(("timer" id) #:when (= id timer))
       (step left right ball)]
      [else (pong timer left right ball)])))

(define (init-ball)
  (cons
    (cons (exact->inexact (/ (width) 2))
          (exact->inexact (/ (height) 2)))
    (cons (* (ball-speed-modifier) (call/native '(math random)))
          (* (ball-speed-modifier) (call/native '(math random))))))

(define (run-pong)
  (call/native '(term setCursorBlink) #f)
  (pong (call/native '(os startTimer) 0.1)
        (cons 0 init-paddle)
        (cons 0 init-paddle)
        (init-ball)))

(define (pong-against-the-machine)
  (ai-player-left #f)
  (ai-player-right #t)
  (run-pong))

(pong-against-the-machine)
