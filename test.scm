(display "Hello from loaded file!\n")

(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))