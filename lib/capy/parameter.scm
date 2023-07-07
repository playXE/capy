(select-module capy)
;(define make-parameter #f)

#|
(define-syntax parameterize 
    (syntax-rules ()
        [(_ ([p v] ...) e ...)
            (let ([old (p)] ...) 0
                (dynamic-wind (lambda () (p v) ...)
                    (lambda () e ...)
                    (lambda () (p old) ...)))]))
|#

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
                   ((param value p old new) ...)
                   ()
                   body ...)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new ((p '<param-convert>) value)) ...)
        (dynamic-wind
          (lambda () (p '<param-set!> new) ...)
          (lambda () body ...)
          (lambda () (p '<param-set!> old) ...)))))
    ((parameterize ("step")
                   args
                   ((param value) . rest)
                   body ...)
     (parameterize ("step")
                   ((param value p old new) . args)
                   rest
                   body ...))
    ((parameterize ((param value) ...) body ...)
     (parameterize ("step")
                   ()
                   ((param value) ...)
                   body ...))))
                   
(define (make-parameter init . o)
  (let* ((converter
           (if (pair? o) (car o) (lambda (x) x))))
    (lambda args
      (cond
        ((null? args)
         (converter init))
        ((eq? (car args) '<param-set!>)
         (set! init (cadr args)))
        ((eq? (car args) '<param-convert>)
         converter)
       (else
         (error "bad parameter syntax"))))))
    