(define (call-with-current-continuation f)
        (_call-with-unprotected-continuation
           (lambda (cont)
             (f (_continuation args
                  (do ((base (_dynamic-wind-base cont)))
                      ((eqv? (_dynamic-wind-current) base))
                    ((cdr (_wind-down))))
                  (do ((winders (_dynamic-winders cont) (cdr winders)))
                      ((null? winders) (cont (_make-values args)))
                    ((car (car winders)))
                    (_wind-up (car (car winders)) (cdr (car winders)))))))))

(define call/cc call-with-current-continuation)

(define (f ret)
  (ret 42)
  44)

(_dbg (call/cc f))
(_dbg (f (lambda (x) x)))
