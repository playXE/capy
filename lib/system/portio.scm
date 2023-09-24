(define (make-transcoder codec . rest)
    (cond 
        [(null? rest) 
            (io/make-transocder codec (native-eol-style) 'replace)]
        [(null? (cdr rest))
            (io/make-transcoder codec (car rest) 'replace)]
        [(null? (cddr rest))
            (io/make-transcoder codec (car rest) (cadr rest))]
        [else 
            (assertion-violation 
                'make-transcoder 
                "too many arguments"
                (cons codec rest))]))