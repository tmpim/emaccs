(define-syntax (receive vars expr . body)
  `(call-with-values (lambda () ,expr)
                     (lambda ,vars . ,body)))
