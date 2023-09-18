(eval-core 
    '(define (interpreter-loop)
        (let loop ([i 0])
            (if (< i 10000000)
                (loop (+ i 1))))))

(define (bytecode-loop)
    (let loop ([i 0])
        (if (< i 10000000)
            (loop (+ i 1)))))

(let ([start (current-millis)])
    (interpreter-loop)
    (print (- (current-millis) start)))

(let ([start (current-millis)])
    (bytecode-loop)
    (print (- (current-millis) start)))