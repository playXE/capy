(define p (make-parameter "hello" 42))

(print (p))
(p 0)
(print (p))

(print (parameter? p))