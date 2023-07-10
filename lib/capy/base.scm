(select-module capy)
(define (call-with-values generator receiver)
    (apply-with-values receiver (generator)))