; R6RS Hash Tables

(define-module capy.hashtable
    (export 
        eqv-hash
        make-eq-hashtable
        make-eqv-hashtable
        make-r6rs-hashtable
        make-oldstyle-hashtable
        hashtable?
        hashtable-contains?
        hashtable-ref
        hashtable-set!
        hashtable-delete!
        hashtable-clear!
        hashtable-size
        hashtable-keys
        hashtable-entries
        hashtable-copy
        hashtable-equivalence-function
        hashtable-hash-function
        hashtable-mutable?
        hashtable-reset!
    )
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
    
    (struct hashtable
        ((count #:mutable)
         hash-function
         safe-hash-function
         equivalence-predicate
         bucket-searched
         hashtable-type 
         (buckets #:mutable)
         (mutable-flag #:mutable)))
    
    (let ((%hashtable? hashtable?)
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
                           
                        (hashtable 
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
             [count hashtable-count]
             [count! set-hashtable-count!]
             [hasher hashtable-hash-function]
             [safe-hasher hashtable-safe-hash-function]
             [equiv hashtable-equivalence-predicate]
             [searcher hashtable-bucket-searched]
             [htype hashtable-hashtable-type]
             [buckets hashtable-buckets]
             [buckets! set-hashtable-buckets!]
             [mutable? hashtable-mutable-flag]
             [immutable! (lambda (ht) (set-hashtable-mutable-flag! ht #f))]
             [defaultn 20])

    (let ([hashtable-error (lambda (who x mut?)
        (error procedure (if mut? "hashtable is immutable ~a" "bad hash table ~a") x)
    )])
        (define (guarantee-hashtable proceudre object)
            (if (not (hashtable? object))
                (hashtable-error procedure object #f)))
        (define (guarantee-mutable proceudre object)
            (if (not (hashtable-mutable? object))
                (hashtable-error procedure object #t)))

        (define (make-oldstyle-ht hashfun searcher size)
            (make-raw-ht hashfun 
                        (lambda (x y) (if (searcher x (list (list y))) #t #f))
                        searcher
                        size
                        'usual))
        (define (make-ht hashfun equiv searcher size)
            (make-raw-ht hashfun equiv searcher size 'usual))
        (define (make-ht-eq size)
            (make-raw-ht eq-hash eq? assq size 'eq?))
        (define (make-ht-eqv size)
            (make-raw-ht eqv-hash eqv? assv size 'eqv?))

        (define (remq1 x y)
            (cond ((eq? x (car y))
                    (cdr y))
                    (else
                    (cons (car y)
                        (remq1 x (cdr y))))))
        
        ; Resizes and rehashes the hashtable.
        (define (resize ht) 
            (let ([n (+ defaultn (* 2 (count ht)))]
                  [b (buckets ht)]
                  [type (htype ht)])
                (case type
                    [(usual)
                        (let ([v (make-vector n '())])
                            (rehash-buckets! b v (safe-hasher ht))
                            (buckets! ht v))]
                    [(eq? eqv?)
                        (let* ([n/2 (+ 1 (quotient n 2))]
                               [b (buckets ht)]
                               [v (make-vector n/2 '())]
                               [hf (if (eq? type 'eq?) eq-hash eqv-hash)])
                            (rehash-buckets! b v hf)
                            (buckets! ht v))]
                    [else
                        (error 'resize "bad hashtable type ~a" type)])))
        (define (maybe-resize! ht)
            (let* ([k (count ht)] [v (buckets ht)] [n (vector-length v)])
                (if (or (< n k)
                        (< (* 3 (+ defaultn k)) n))
                        (resize ht))))
        ; Copies all entries in the src vector to the dst vector,
        ; rehashing each key using the hash function hf.
        (define (rehash-buckets! src dst hf)
            (let ((m (vector-length src))
                    (n (vector-length dst)))
                (do ((i 0 (+ i 1)))
                    ((= i m))
                (do ((bucket (vector-ref src i) (cdr bucket)))
                    ((null? bucket))
                    (let* ((entry (car bucket))
                        (key (car entry))
                        (h (hf key))
                        (j (mod h n)))
                    (vector-set! dst j (cons entry (vector-ref dst j))))))))
        (define (ht-entries ht)
            (guarantee-hashtable 'hashtable-entries ht)
            (let* ([v (buckets ht)]
                   [k (count ht)]
                   [keys (make-vector k '())]
                   [vals (make-vector k '())])
                (define (collect-entries v j)
                    (loop v 0 (vector-length v) '() j))
                (define (loop v i n bucket j)
                    (cond [(pair? bucket)
                           (let ([entry (car bucket)])
                            (vector-set! keys j (car entry))
                            (vector-set! vals j (cdr entry))
                            (loop v i n (cdr bucket) (+ j 1)))]
                          [(null? bucket)
                            (if (= i n)
                                j
                                (loop v (+ i 1) n (vector-ref v i) j))]
                          [else (error 'ht-entries "weird hashtable")]))
                (let ([j (collect-entries v 0)])
                    (if (= j k)
                        (values keys vals)
                        (error 'ht-entries "weird hashtable")))))
        (define (ht-keys ht)
            (apply-with-values (lambda (keys vals) keys) (ht-entries ht)))
        
        (define (contains? ht key)
            (guarantee-hashtable 'hashtable-contains? ht)
            (let* ([type (htype ht)]
                   [bucket-search (searcher ht)]
                   [hf (safe-hasher ht)]
                   [h  (hf key)])
                (define (tablet-contains? v)
                    (let* ([n (vector-length v)]
                           [i (abs (remainder h n))]
                           [b (vector-ref v i)])
                        (if (bucket-search key b) #t #f)))

                (tablet-contains? (buckets ht))))
        
        (define (fetch ht key flag)
            (guarantee-hashtable 'hashtable-ref ht)
            (let* ([type (htype ht)]
                   [bucket-search (searcher ht)]
                   [hf (safe-hasher ht)]
                   [h (hf key)])
                (define (tablet-search v)
                    (let* ((n (vector-length v))
                            (i (abs (remainder h n)))
                            (b (vector-ref v i)))
                        (bucket-search key b)))
                (let ([entry (tablet-search (buckets ht))])
                    (if entry (cdr entry) flag))))
        
        (define (put! ht key val)
            (guarantee-mutable 'hashtable-set! ht)
            (let* ([type (htype ht)]
                   [bucket-search (searcher ht)]
                   [hf (safe-hasher ht)]
                   [h  (hf key)])
                (define (tablet-search v)
                    (let* ((n (vector-length v))
                            (i (abs (remainder h n)))
                            (b (vector-ref v i)))
                        (bucket-search key b)))
                (let* ([v (buckets ht)]
                       [entry (tablet-search v)])
                    (if entry 
                        (set-cdr! entry val)
                        (let* ([n (vector-length v)]
                            [i (abs (remainder h n))]
                            [b (vector-ref v i)])
                            (vector-set! v i (cons (cons key val) b))
                            (count! ht (+ 1 (count ht)))
                            (maybe-resize! ht))))))

        (define (remove! ht key)
            (guarantee-mutable 'hashtable-delete! ht)
            (let* ([type (htype ht)]
                   [bucket-search (searcher ht)]
                   [hf (safe-hasher ht)]
                   [h  (hf key)])
                (define (tablet-remove! v)
                    (let* ((n (vector-length v))
                            (i (abs (remainder h n)))
                            (b (vector-ref v i))
                            (probe (bucket-search key b)))
                        (if probe
                            (begin (vector-set! v i (remq1 probe b)) #t)
                            #f)))
                (if (tablet-remove! (buckets ht))
                    (begin 
                        (count! ht (- (count ht) 1))
                        (maybe-resize! ht)))))
        (define (clear! ht n)
            (guarantee-mutable 'hashtable-clear! ht)
            (count! ht 0)
            (buckets! ht (make-vector (+ defaultn n) '())))
        (define (size ht)
            (guarantee-hashtable 'hashtable-size ht)
            (count ht))
        
        (define (ht-copy ht mutable)
            (guarantee-hashtable 'ht-copy ht)
            (let* ([type (htype ht)] [k (count ht)]
                   [newtable
                    (case type
                        [(usual)
                            (make-r6rs-hashtable (hasher ht) (equiv ht) k)]
                        [(eq?)
                            (make-eq-hashtable k)]
                        [(eqv?)
                            (make-eqv-hashtable k)])])
                (let-values ([(keys vals) (hashtable-entries ht)])
                    (vector-for-each (lambda (key val)
                        (hashtable-set! newtable key val)
                    ) keys vals)
                    (if (not mutable)
                        (immutable! newtable))
                    newtable)))

        (set! make-oldstyle-hashtable
            (lambda args
                (let* ((hashfun (if (null? args) eqv-hash (car args)))
                    (searcher (if (or (null? args) (null? (cdr args)))
                                assv
                                (cadr args)))
                    (size (if (or (null? args)
                                (null? (cdr args))
                                (null? (cddr args)))
                            defaultn
                            (caddr args))))
                (make-oldstyle-ht hashfun searcher size))))

        (set! make-eq-hashtable
          (lambda rest
            (make-ht-eq (if (null? rest) defaultn (car rest)))))

        (set! make-eqv-hashtable
            (lambda rest
                (make-ht-eqv (if (null? rest) defaultn (car rest)))))
        (set! make-r6rs-hashtable
          (lambda (hashfun equiv . rest)
            (define (search-bucket key bucket)
              (cond ((null? bucket) #f)
                    ((equiv key (caar bucket))
                     (car bucket))
                    (else (search-bucket key (cdr bucket)))))
            (make-ht hashfun
                     equiv
                     (lambda (key bucket) (search-bucket key bucket))
                     (if (null? rest) defaultn (car rest)))))

        (set! hashtable-contains? (lambda (ht key)      (contains? ht key)))
        (set! hashtable-ref       (lambda (ht key flag) (fetch ht key flag)))
        (set! hashtable-set!      (lambda (ht key val)  (put! ht key val)))
        (set! hashtable-delete!   (lambda (ht key)      (remove! ht key)))
        (set! hashtable-clear!    (lambda (ht . rest)
                                    (clear! ht
                                            (if (null? rest)
                                                defaultn
                                                (car rest)))))
        (set! hashtable-size      (lambda (ht)          (size ht)))
        (set! hashtable-keys      (lambda (ht)          (ht-keys ht)))
        (set! hashtable-entries   (lambda (ht)          (ht-entries ht)))
        (set! hashtable-copy      (lambda (ht . rest)
                                    (ht-copy ht (if (null? rest) #f (car rest)))))

        (set! hashtable-equivalence-function (lambda (ht) (equiv ht)))
        (set! hashtable-hash-function        (lambda (ht)
                                            (if (eq? (htype ht) 'usual)
                                                (hasher ht)
                                                #f)))
        (set! hashtable-mutable?             (lambda (ht) (mutable? ht))))))