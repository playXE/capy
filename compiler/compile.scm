(select-module capy.internal)

(define (cenv-module cenv) (vector-ref cenv 1))
(define (cenv-frames cenv) (vector-ref cenv 2))
(define (make-cenv module frames) 
    (vector 'cenv module frames '() '() '()))

(define (make-cenv-bottom module)
    (make-cenv (if module module (vm-current-module)) '()))

(define (cenv-module cenv) (vector-ref cenv 1))
(define (cenv-frames cenv) (vector-ref cenv 2))
(define (cenv-exp-name cenv) (vector-ref cenv 3))
(define (cenv-current-proc cenv) (vector-ref cenv 4))
(define (cenv-source-path cenv) (vector-ref cenv 5))
(define (cenv-set-module cenv module) (vector-set! cenv 1 module))
(define (cenv-set-frames cenv frames) (vector-set! cenv 2 frames))
(define (cenv-set-exp-name cenv exp-name) (vector-set! cenv 3 exp-name))
(define (cenv-set-current-proc cenv proc) (vector-set! cenv 4 proc))
(define (cenv-set-source-path cenv path) (vector-set! cenv 5 path))

(define (cenv-copy cenv)
    (make-cenv (cenv-module cenv)
               (cenv-frames cenv)))

(define (cenv-extend cenv frame typ)
    (make-cenv (cenv-module cenv)
        (cons (cons typ frame) (cenv-frames cenv))))
