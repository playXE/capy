(select-module capy)

(define (syntax->datum syntax) (unwrap-syntax syntax #f))
(define (bound-identifier=? id1 id2) (eq? id1 id2))