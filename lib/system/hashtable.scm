(define (make-eq-hashtable . args)
    (let ([k 
        (cond 
            [(null? args) 0]
            [(null? (cdr args)) (car args)]
            [else (error 'make-eq-hashtable "too much arguments (expected 1)")])])
        (make-core-hashtable 'eq? k)))

(define (make-eqv-hashtable . args)
    (let ([k 
        (cond 
            [(null? args) 0]
            [(null? (cdr args)) (car args)]
            [else (error 'make-eqv-hashtable "too much arguments (expected 1)")])])
        (make-core-hashtable 'eqv? k)))

(define (make-equal-hashtable . args)
    (let ([k 
        (cond 
            [(null? args) 0]
            [(null? (cdr args)) (car args)]
            [else (error 'make-equal-hashtable "too much arguments (expected 1)")])])
        (make-core-hashtable 'equal? k)))

(define (make-string-hashtable . args)
    (let ([k 
        (cond 
            [(null? args) 0]
            [(null? (cdr args)) (car args)]
            [else (error 'make-string-hashtable "too much arguments (expected 1)")])])
        (make-core-hashtable 'string=? k)))

(define hashtable? core-hashtable?)
(define hashtable-set! core-hashtable-set!)
(define hashtable-ref core-hashtable-ref)
(define hashtable-delete! core-hashtable-delete!)
(define hashtable-contains? core-hashtable-contains?)
(define hashtable-copy core-hashtable-copy)
(define hashtable-clear! core-hashtable-clear!)
(define hashtable-equivalence-function core-hashtable-equivalence-function)
(define hashtable-hash-function core-hashtable-hash-function)
(define hashtable-mutable? core-hashtable-mutable?)
(define hashtable->alist core-hashtable->alist)
(define (hashtable-keys ht)
    (list->vector (map car (hashtable->alist ht))))
(define (hashtable-values ht)
    (list->vector (map cdr (hashtable->alist ht))))

(define (hashtable-entries ht)
    (let ([lst (hashtable->alist ht)])
        (values (list->vector (map car lst))
                (list->vector (map cdr lst)))))

(define (hashtable-update! ht key proc default)
    (or (core-hashtable-mutable? ht)
        (assertion-violation 'hashtable-update! "expected mutable hashtable" (list ht key proc default)))
    (core-hashtable-set! ht key (proc (core-hashtable-ref ht key default))))