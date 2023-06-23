(define-syntax let 
    (syntax-rules ()
        ((_ ((var val) ...) body ...)
            ((lambda (var ...) body ...) val ...))
        ((_ f ((x v) ...) e1 e2 ...)
            (letrec ((f (lambda (x ...) e1 e2 ...)))
                   ((lambda (x ...) (f x ...))
                    v ...)))))

(define-syntax letrec 
    (syntax-rules ()
        ((_ ((x v) ...) e1 e2 ...)
            (let ((x 0) ...) 
                (set! x v) ...
                e1 e2 ...))))

(let name () (name))