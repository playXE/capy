(define (f x . rest)
    (display (format "x: ~a, rest: ~a\n" x rest)))

(apply f 1 (list 1 2 3))