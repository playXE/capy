(define (+ . args)
  (let loop ([args args] [sum 0])
    (if (null? args)
        sum
        (loop (cdr args) (+ sum (car args))))))

(define plus +)
(let ([myvar 42])
  (call-with-values 
    (lambda () (values 1 2 3 4))
    (lambda (x y . rest)
      (plus x (car rest) (car (cdr rest)) myvar))))