(select-module capy)
(define (call-with-values generator receiver)
    (apply-with-values receiver (generator)))

(define-syntax syntax-length 
    (er-macro-transformer 
        (lambda (expr rename compare)
            (length (cdr expr)))))


(define unspecified undefined)
(define unspecified? undefined?)

(define (newline)
    (display "\n"))

(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (uint8? x)
    (and (exact-integer? x)
        (and (>= x 0)
            (<= x 255))))