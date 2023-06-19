
(define-syntax-rules lam () ((_ args body ...) (lambda args body ...)))

(lam () 1)