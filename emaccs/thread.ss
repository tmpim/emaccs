(define (coroutine-create x)
  "Create a coroutine for the function ,x."
  (call/native '(coroutine create) x))

(define (coroutine-resume thread . vals)
  "Resume the thread ,thread with the values ,vals."
  (apply (hash-ref (hash-ref (environment) "coroutine") "resume")
         (cons thread vals)))

(define (coroutine-status t)
  "Return the status of the thread ,t."
  (call/native '(coroutine status) t))
