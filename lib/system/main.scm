(define (fac n)
    (let loop ((n n) (acc 1))
        (if (= n 0)
            acc
            (loop (- n 1) (* acc n)))))

(display (fac 5))
(newline)
