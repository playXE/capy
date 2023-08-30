(define (foobar x y)
    (let loop ([z (+ x y)])
        (let ([w (+ x z)])
            (lambda ()
                (print w z x y)
            )
        )))


((foobar 1 2))