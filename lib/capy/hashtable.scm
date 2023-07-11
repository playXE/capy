; R6RS Hash Tables

(define-module capy.hashtable

    (define (eqv-hash x)
        (cond
            [(number? x) (object-hash x)]
            [(char? x) (object-hash x)]
            [(symbol? x) (object-hash x)]
            [else (eq-hash x)]))
    

    ; these global variables are assigned new values later.
    (define make-eq-hashtable (lambda args '*))
    (define make-eqv-hashtable (lambda args '*))
    (define make-r6rs-hashtable     (lambda args '*))
    (define make-oldstyle-hashtable (lambda args '*))
    (define hashtable?              (lambda (arg) #f))
    (define hashtable-contains?     (lambda (ht key) #f))
    (define hashtable-ref           (lambda (ht key flag) flag))
    (define hashtable-set!          (lambda (ht key val) '*))
    (define hashtable-delete!       (lambda (ht key) '*))
    (define hashtable-clear!        (lambda (ht . rest) '*))
    (define hashtable-size          (lambda (ht) 0))
    (define hashtable-keys          (lambda (ht) '()))
    (define hashtable-entries       (lambda (ht) (values '() '())))
    (define hashtable-copy          (lambda (ht . rest) ht))

    (define hashtable-equivalence-function (lambda (ht) equal?))
    (define hashtable-hash-function        (lambda (ht) equal-hash))
    (define hashtable-mutable?             (lambda (ht) #t))

    (define hashtable-reset!        (lambda (ht) (undefined))))