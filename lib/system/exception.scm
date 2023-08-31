(define (assertion-violation who message . irritants)
    (error 'nyi "NYI"))

(define (undefined-violation who . message)
    (error 'nyi "NYI"))

(define (lexical-violation who . message)
    (error 'nyi "NYI"))

(define (syntax-violation who message from . subform)
    (error 'nyi "NYI"))

(define error 
    (let ([err error])
        (lambda (who message . irritants)
            (err "NYI"))))
