
(define syntax->datum unwrap-syntax)
(define datum->syntax #f)
(define identifier? #f)
(define identifier->symbol #f)
(define symbol->identifier #f)
(define outermost-identifier #f)

(let ()
    (define current-renamer (make-parameter (lambda (x) x)))
    (define (rename id) ((current-renamer) id))
    (define (ellipsis-identifier? id)
        (free-identifier=? id (rename '...)))
    
    (set! symbol->identifier 
        (lambda (id symbol)
            (cond 
                [(symbol? id)
                    symbol]
                [else (rename id)]))))