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

    (define hashtable-reset!        (lambda (ht) (undefined)))
    
    (struct *hashtable*
        ((count #:mutable)
         hash-function
         safe-hash-function
         equivalence-predicate
         bucket-searched
         hashtable-type 
         (buckets #:mutable)
         (mutable-flag #:mutable)))
    
    (let ((%hashtable? *hashtable*?)
          [make-raw-ht 
            (let ([make-safe-hasher-caching
                (lambda (hf)
                    (let ((cache #f))
                        (lambda (key)
                            (let ((keyhash cache))
                                (if (and keyhash (eq? key (car keyhash)))
                                    (cdr keyhash)
                                    
                                (let ([h (hf key)])
                                    (set! cache (cons key h))
                                    h))))))]
                   [make-safe-hasher 
                    (lambda (hf)
                        (lambda (key) (hf key)))])
                        
                (lambda (hf equiv searcher size type)
                    (let* ([n (if (> size 1) size 1)]
                           [n (if (eq? type 'usual)
                                n
                                (+ 1 (quotient n 2)))]
                           [b (make-vector n '())])
                           
                        (*hashtable* 
                            0
                            hf 
                            (if (eq? type 'usual)
                                (make-safe-hasher-caching hf)
                                hf)
                            equiv 
                            searcher
                            type
                            b 
                            #t))
                ))]
             [count *hashtable*-count]
             [count! set-*hashtable*-count!]
             [hasher *hashtable*-hash-function]
             [safe-hasher *hashtable*-safe-hash-function]
             [equiv *hashtable*-equivalence-predicate]
             [searcher *hashtable*-bucket-searched]
             [htype *hashtable*-hashtable-type]
             [buckets *hashtable*-buckets]
             [buckets! set-*hashtable*-buckets!]
             [mutable? *hashtable*-mutable-flag]
             [immutable! (lambda (ht) (set-*hashtable*-mutable-flag! ht #f))]
             [defaultn 20])
             
             0)
)