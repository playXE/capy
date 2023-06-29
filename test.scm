(define (make-parameter init . rest)
    (let ([converter (if (pair? rest) (car rest) (lambda (x) x))])
        (let ([value (converter init)])
            (lambda args 
                (if (null? args) value 
                    (let ([new-value (converter (car args))])
                        (set! value new-value)
                        new-value))))))

(define x (make-parameter 1))


(x 4)
(x)