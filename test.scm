(define-syntax let1-values 
    (syntax-rules ()
        ((_ ((id ...) val-expr) body ...)
            (call-with-values (lambda () val-expr)
                (lambda (id ...) body ...)))
        ((_ id val-expr body ...)
            (call-with-values (lambda () val-expr)
                (lambda id body ...)))))

(define-syntax let-values
    (syntax-rules ()
        ((let-values (binding ...) body0 body1 ...)
         (let-values "bind"
            (binding ...) () (begin body0 body1 ...)))
        ((let-values "bind" () tmps body)
         (let tmps body))
        ((let-values "bind" ((b0 e0)
            binding ...) tmps body)
         (let-values "mktmp" b0 e0 ()
            (binding ...) tmps body))
        ((let-values "mktmp" () e0 args
            bindings tmps body)
         (call-with-values
            (lambda () e0)
            (lambda args
                (let-values "bind"
                    bindings tmps body))))
        ((let-values "mktmp" (a . b) e0 (arg ...)
            bindings (tmp ...) body)
         (let-values "mktmp" b e0 (arg ... x)
            bindings (tmp ... (a x)) body))
        ((let-values "mktmp" a e0 (arg ...)
            bindings (tmp ...) body)
        (call-with-values
            (lambda () e0)
            (lambda (arg ... . x)
                (let-values "bind"
                    bindings (tmp ... (a x)) body))))))


(let-values ((a (values 1 2)))
    a)