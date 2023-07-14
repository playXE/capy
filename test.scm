(define (rec-fib n)
    (let loop ((n n) (a 0) (b 1))
        (if (= n 0)
            a
            (loop (- n 1) b (+ a b)))))

(display (rec-fib 1000))