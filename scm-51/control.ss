(define (yield x) (call/native '(coroutine yield) x))

(define *abort (make-hash-table `(1 . "aborted")))

(define/native (thread? x) "return type(_x) == 'thread'")

(define (call-with-prompt tag thunk handler . v)
  (define co
    (if (thread? thunk)
      thunk
      (call/native '(coroutine create) thunk)))
  (define (loop acc v)
    (call-with-values
      (lambda ()
        (call/native '(coroutine resume) co (if (null? v) #f (car v))))
      (lambda (ok . results)
        (cond
          ((= (call/native '(coroutine status) co) "dead")
           (if (not acc)
             (apply values results)
             (apply values acc)))
          ((not ok) (apply error rest))
          ((and (pair? results) (= (car results) *abort))
           (if (= (cadr results) tag)
             (apply handler (cons (lambda (v)
                                    (call-with-prompt tag co handler v))
                                  (car (cddr results))))
             (apply abort-to-prompt (cons tag results))))
          (else (loop results))))))
  (loop #f v))

(define (abort-to-prompt tag . args)
  (call/native '(coroutine yield) *abort tag args))

(define make-prompt-tag make-hash-table)
(define *reset-tag (make-prompt-tag))
(define default-prompt-tag (make-prompt-tag))

(define-syntax (reset . body)
  `(call-with-prompt *reset-tag
                     (lambda () . ,body)
                     (lambda (k v) (v k))))

(define-syntax (shift1 k . body)
  `(abort-to-prompt *reset-tag (lambda (,k) . ,body)))

(define-syntax (reset/t tag . body)
  `(call-with-prompt ,tag
                     (lambda () . ,body)
                     (lambda (k v) (v k))))

(define-syntax (shift1/t tag k . body)
  `(abort-to-prompt ,tag (lambda (,k) . ,body)))

(define-syntax (let/ec k . body)
  (define r (gensym))
  (define i (gensym))
  (define t (gensym))
  `(let ((,t (make-prompt-tag)))
     (reset/t ,t
       (let ((,k (lambda ,r (shift1/t ,t ,i (apply values ,r)))))
         . ,body))))
