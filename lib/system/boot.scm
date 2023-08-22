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

(for-all print (list 1 2 3 4))