(define (make-parameter arg1 . rest)
  (let* ([srfi39-style? (or (null? rest)
                            (and (null? (cdr rest))
                                 (procedure? (car rest))))]
         [converter (if (and srfi39-style? (pair? rest))
                        (car rest)
                        values)]
         [ok? (if (or (null? rest) (null? (cdr rest)))
                  (lambda (x) #t) 
                  (cadr rest))]
         [name (if srfi39-style? #f arg1)]
         [value (if srfi39-style? (converter arg1) (car rest))])
    (define (complain-argcount)
      (assertion-violation name "too many arguments" (cons arg1 rest))
      #t)
    (define (complain-bad-value x)
      (assertion-violation name "invalid value for parameter" x)
      #t)
    (if srfi39-style?
        (lambda args
          (if (pair? args)
              (cond [(null? (cdr args))
                     (let ((new-value (converter (car args))))
                       (set! value new-value)
                       value)]
                    [(eq? (cadr args) 'no-conversion)
                     (let ((new-value (car args)))
                       (set! value new-value)
                       value)]
                    [else
                     (complain-argcount)])
              value))
        (lambda args
          (if (pair? args)
              (if (or (null? (cdr args))
                      (eq? (cadr args) 'no-conversion))
                  (let ([new-value (car args)])
                    (if (ok? new-value)
                        (begin (set! value new-value)
                               value)
                        (complain-bad-value (car args))))
                  (complain-argcount))
              value)))))

(define *r7rs-parameter-prototype* (make-parameter 0))
(define *old-style-parameter-prototype* (make-parameter "p" 0))

(define (parameter? p)
    (and (procedure? p)
        (or (procedure=? p *r7rs-parameter-prototype*)
            (procedure=? p *old-style-parameter-prototype*))))

(define (make-env-parameter name . rest)
  (let ((*name* name)
        (ok?    (if (null? rest)
                  (lambda (x) #t)
                  (car rest))))
    (lambda args
      (cond
        ((not (pair? args))
         (getenv *name*))
        ((not (null? (cdr args)))
         (error (string->symbol *name*) "too many arguments."))
        ((ok? (car args))
         (setenv *name* (car args)))
        (else
         (error (string->symbol *name*) "Invalid value " (car args)))))))