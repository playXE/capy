(define (assertion-violation who message . irritants)
    (error 'nyi message))

(define (undefined-violation who . message)
    (error 'nyi "NYI"))

(define (lexical-violation who . message)
    (error 'nyi message))

(define (syntax-violation who message from . subform)
    (error 'nyi message))

(define error 
    (let ([err error])
        (lambda (who message . irritants)
            (err message))))
