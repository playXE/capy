; Arithmetic functions

(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (abs n)
  (cond 
    [(< n 0) (- n)]
    [(flonum? n) (+ n 0.0)]
    [else n]))