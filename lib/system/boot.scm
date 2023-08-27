(define (identity x) x)
(define (find proc list)
    (let loop ([rest list])
        (if (null? rest)
            #f
            (let ([car (car rest)])
                (if (proc car)
                    car
                    (loop (cdr rest)))))))
                    
(define-syntax define-quantifier
  (syntax-rules ()
    ((_ <name> <base-value> <terminating-value?>)
     (define (<name> proc list1 . lists)
       (define (length-error)
         (error "Lists don't have the same length." (cons list1 lists)))
       (if (null? list1)
           ;; Careful with recursion on for-all.
           (if (or (null? lists) (for-all null? lists))
               <base-value>
               (length-error))
           (let loop ((car1 (car list1))
                      (cars (map car lists))
                      (cdr1 (cdr list1))
                      (cdrs (map cdr lists)))
             (if (null? cdr1)
                 (if (for-all null? cdrs)
                     (apply proc car1 cars)
                     (length-error))
                 (let ((value (apply proc car1 cars)))
                   (if (<terminating-value?> value)
                       value
                       (loop (car cdr1) (map car cdrs)
                             (cdr cdr1) (map cdr cdrs)))))))))))

(define-quantifier for-all #t not)
(define-quantifier exists #f identity)

(define (cons* . args)
    (if (null? args)
        '()
        (cons (car args) (cons* (cdr args)))))

; 0: kind, 1: message, 2: stack-trace
(define exn-vtable (make-vtable (string-append standard-vtable-fields "phphph")))

(define (exn? x)
    (if (struct? x)
        (eq? (struct-vtable x) exn-vtable)
        #f))

(define (exn-message exn)
    (if (exn? exn)
        (struct-ref exn 1)
        #f))

(define (exn-stack-trace exn)
    (if (exn? exn)
        (struct-ref exn 2)
        #f))

(define (exn-kind exn)
    (if (exn? exn)
        (struct-ref exn 0)
        #f))

(define (make-exn kind message stacktrace)
    (make-struct/simple exn-vtable kind message stacktrace))

(define (make-parameter init . o)
  (let ((converter
           (if (pair? o) (car o) (lambda (x) x))))
    (lambda args
      (if (null? args)
        (converter init)
        (if (eq? (car args) '<param-set!>)
            (set! init (converter (car (cdr args))))
            (if (eq? (car args) '<param-convert>)
                converter)
                (set! init (converter (car args))))))))

(define (parent-exception-handler) (make-parameter #f))
(define (current-exception-handler) (wind-up-raise #f #f))

(define (raise c)
    (let ([eh (current-exception-handler)])
        (if eh 
            (eh c)
            (let ([parent-eh (parent-exception-handler)])
                (if parent-eh 
                    (parent-eh c)
                    (%raise c)))
            (%raise "Uncaught exception"))))


(define (call-with-current-continuation proc)
    (%call/cc (lambda (k)
        (proc 
            (lambda args
                (let loop ([base (%dynamic-wind-base k)])
                    (if (eqv? (%dynamic-wind-current) base)
                        #f  
                        (begin 
                            ((cdr (%wind-down)))
                            (loop base))))
                (let loop ([winders (%dynamic-winders k)])
                    (if (null? winders)
                        (apply k args)
                        (begin 
                            ((car (car winders)))))
                            (%wind-up (car (car winders)) (cdr (car winders)))
                            (loop (cdr winders))))))))

(define call/cc call-with-current-continuation)

(define (dynamic-wind before thunk after)
    (before)
    (%wind-up before after)
    (let ([result (thunk)])
        ((cdr (%wind-down)))
        result))


(define (make-parameter init . o)
  (let ((converter
           (if (pair? o) (car o) (lambda (x) x))))
    (lambda args
      (if (null? args)
        (converter init)
        (if (eq? (car args) '<param-set!>)
            (set! init (converter (car (cdr args))))
            (if (eq? (car args) '<param-convert>)
                converter
                (set! init (converter (car args)))))))))

(define (current-exception-handler) (%wind-up-raise undefined undefined))
(define (undefined) (if #f #f))
(define (raise c)
    (let ([eh (current-exception-handler)])
        (if (not eh)
            (%raise "Uncaught exception")
            (begin 
                (print "eh" eh)
                (eh c)
                (%raise c)))))

(define (with-exception-handler handler thunk)
    (print "with-exception-handler" handler)
    (%wind-up undefined undefined handler)
    (let ([result (thunk)])
        (%wind-down)
        result))

(with-exception-handler 
    (lambda (val)
        (print "Exception" val)
        (raise val))
    (lambda ()
        (print "Hello")
        (raise "World")))