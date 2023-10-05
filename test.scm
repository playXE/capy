
(define (- arg . args)
    (let loop ([diff arg] [args args])
        (if (null? args)
            diff
            (loop (- diff (car args)) (cdr args)))))
(define (foo x y z w r)
  (- (+ x (- y (/ (* z w) r)))))

(print-raw (foo 1 2 3 4 5))