(define-syntax and 
    (syntax-rules ()
        ((_) #t)
        ((_ a) a)
        ((_ a b ...) (if a (and b ...) #f))))

(define (list-ref ls i)
    (let loop ((ls ls) (i i))
        (if (= i 0)
            (car ls)
            (loop (cdr ls) (- i 1)))))

(define (list-head ls n)
    (if (= n 0)
        '()
        (cons (car ls) (list-head (cdr ls) (- n 1)))))

(define (list-tail ls n)
    (if (= n 0)
        ls
        (list-tail (cdr ls) (- n 1))))

(define (split-at ls n)
    (list (list-head ls n) (list-tail ls n)))

(print (split-at '(1 2 3 4) 3))