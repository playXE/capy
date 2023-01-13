(define (make-tree depth)
    (if (> depth 0)
        ;(let ([node (cons '() '())])
        ;    (set-car! node (make-tree (- depth 1)))
        ;    (set-cdr! node (make-tree (- depth 1)))
        ;    node)
        (cons (make-tree (- depth 1)) (make-tree (- depth 1)))
        (cons '() '())))

(define (check-tree-old tree)
    (if (null? (car tree))
        1
        (+ 1 (check-tree (car tree)) (check-tree (cdr tree)))))
(define (check-tree tree)
    (let loop ([tree tree] [acc 0])
        (if (null? (car tree))
            (+ 1 acc)
            (loop (car tree) (+ 1 (loop (cdr tree) acc))))))

(define (bench min-depth max-depth)
    (_dbg "stretch tree of depth" (+ 1 max-depth) "\tcheck:" (check-tree (make-tree (+ 1 max-depth))))
    (let ([long-lived-tree (make-tree max-depth)])
        (do ((depth min-depth (+ depth 2)))
            ((> depth max-depth))
            (let ([iterations (arithmetic-shift 1 (+ (- max-depth depth) min-depth))]
                  [check 0])
                (do ((i 1 (+ i 1)))
                    ((> i iterations))
                    (set! check (+ check (check-tree (make-tree depth)))))
                (_dbg iterations "\ttrees of depth" depth "check:" check)    ))
                        
        (_dbg "long lived tree of depth" max-depth "\tcheck:" (check-tree long-lived-tree))))

(bench 4 21)