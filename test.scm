(define (make-tree depth)
    (if (> depth 0)
        (cons (make-tree (- depth 1)) (make-tree (- depth 1)))
        (cons '() '())))

(define (check-tree tree)
    (if (null? (car tree))
        1
        (+ 1 (check-tree (car tree)) (check-tree (cdr tree)))))


(do ((i 0 (+ i 1)))
    ((= i 1) i)
    (check-tree (make-tree 21)))