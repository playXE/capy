(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))

(define-syntax when 
    (er-macro-transformer
        (lambda (expr rename compare)
            (let ((test (car (cdr expr)))
                  (body (cdr (cdr expr))))
                (list (rename 'if) test (cons (rename 'begin) body))))))

(when 1 (format "hello"))