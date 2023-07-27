
(define-syntax .call
    (syntax-rules (r5rs +)
        ((_ r5rs + (+))
            0)
        ((_ r5rs + (+ ?e))
            (+ ?e 0))
        ((_ r5rs + (+ ?e1 ?e2 ?e3 ?e4 ...))
            (let* ((t1 ?e1)
                (t2 (+ ?e2 ?e3 ?e4 ...)))
            (+ t1 t2)))
        ((_ ?anything ?proc ?exp) ?exp)))


(define-syntax let* 
    (syntax-rules ()
        ((_ ((?x1 ?e1) (?x ?e) ...) ?body)
            (let ((?x1 ?e1))
                (let* ((?x ?e) ...) ?body)))
        ((_ () ?body)
            ?body)))

(.call r5rs + (+ 1 2))