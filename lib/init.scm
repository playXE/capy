(define eval '())
(define load '())
(define dynamic-wind '())
(define call/cc '())
(define call-with-current-continuation '())
(define command-line-arguments '())

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))


(lambda (argv) 
    (define *winders* '())
    (define (read-to-eof port)
        (define val (read port))
        (if (eof-object? val)
            (begin 
                (close-port port)
                '())
            (cons val (read-to-eof port))))

    (define (common-tail x y)
        (define (common-tail-rec x y)
            (if (eq? x y)
                x
                (common-tail-rec (cdr x) (cdr y))))
        (common-tail-rec 
            (if (> (length x) (length y))
                (list-tail x (- (length x) (length y)))
                x)
            (if (> (length y) (length x))
                (list-tail y (- (length y) (length x)))
                y)))

    (define (do-wind new)
        (define (do-wind-rec ls t)
            (if (not (eq? ls t))
                (begin 
                    (set! *winders* (cdr ls))
                    ((cdar ls))
                    (do-wind-rec (cdr ls) t))))
        (define (do-wind-rec2 ls t)
            
            (if (not (eq? ls t))
                (begin 
                    (set! *winders* (cdr ls))
                    ((caar ls))
                    (do-wind-rec2 (cdr ls) t))))

        (define tail (common-tail *winders* new))
        (do-wind-rec *winders* tail)
        (do-wind-rec2 new tail))
    (set! eval (lambda (x)
        (apply (%compile x))))


    (set! load (lambda (filename)
        (eval (cons 'begin (read-to-eof (make-file-input-port filename))))))

    (set! call/cc (lambda (f)
        (%call/cc (lambda (k)
            (f (let ([save *winders*])
                (lambda (x)
                    (if (not (eq? *winders* save))
                        (do-wind save))
                    (k x))))))))
    (set! call-with-current-continuation call/cc)

    (set! command-line-arguments (lambda () argv))
    (set! dynamic-wind (lambda (before thunk after)
        (before)
        (set! *winders* (cons (cons before after) *winders*))
        (let ([x (thunk)])
            (set! *winders* (cdr *winders*))
            (after)
            x)))
            
    (load (vector-ref argv 0)))

    