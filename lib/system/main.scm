(define (foo x y z w r)
  (- (+ x (- y (/ (* z w) r)))))

(print-raw (foo 1 2 3 4 5))