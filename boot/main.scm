(define (make-adder x)
  (define (inner) 
    (lambda (y) (+ x y)))
  (inner))

(define add3 (make-adder 3))

(add3 4)