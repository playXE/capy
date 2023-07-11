(select-module capy)
(define (call-with-values generator receiver)
    (apply-with-values receiver (generator)))

(define-syntax syntax-length 
    (er-macro-transformer 
        (lambda (expr rename compare)
            (length (cdr expr)))))


(define unspecified undefined)
(define void undefined)
(define void? undefined?)
(define unspecified? undefined?)

(define (newline)
    (display "\n"))

