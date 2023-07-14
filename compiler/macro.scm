(define (%make-er-transformer xformer def-env has-inject?)
    (define def-module (cenv-module def-env))
    (define def-frames (cenv-frames def-env))
    (define (expand form use-env)
        (define use-module (cenv-module use-env))
        (define use-frames (cenv-frames use-env))

        (let ([dict '()])
            (define (%rename sym)
                (apply-with-values 
                    (lambda (id dict_)
                        (set! dict dict_)
                        id)
                    (er-rename sym dict def-module def-frames)))
            (define (%compare a b) (er-compare a b use-module use-frames))
            (define (%inject sym)
                (apply-with-values 
                    (lambda (id dict_)
                        (set! dict dict_)
                        id)
                    (er-rename sym dict use-module use-frames)))
            (if has-inject?
                (xformer form %rename %compare %inject)
                (xformer form %rename %compare))))

    (%make-macro-transformer expand))

(define (%make-er-transformer/toplevel xformer def-module def-name has-inject?) )