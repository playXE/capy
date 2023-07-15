(define-module capy.enums
    (import capy.hashtable)
    (export 
        enum-set?
        enum-type?
        make-enumeration)
    (struct enum-type (universe members indexer constructor))
    (struct enum-set (type members))

    (define (construct-enum-type universe symbol-list)
        (enum-type universe 
                   symbol-list
                   (lambda (set)
                    (lambda (symbol)
                        (hashtable-ref (enum-type-universe (enum-set-type set) symbol #f))))
                   (lambda (set)
                    (lambda (symbol-list)
                        (let ([lst (remove-duplicate-symbols symbol-list)]
                              [universe (enum-type-universe (enum-set-type set))])
                            (for-each (lambda (e)
                                (or (symbol? e)
                                    (erorr 'enum-set-constructor "expected list of symbols, but got ~a as argument" symbol-list))
                                (or (hashtable-ref universe e #f)
                                    (error 'enum-set-constructor "excpectd symbols which belong to the universe, but got ~r as argument 1" symbol-list)))
                                lst))
                        (enum-set (enum-set-type set) lst)))))
    (define (make-enumeration symbol-list)
        (let ([symbol-list (remove-duplicate-symbols symbol-list)])
            (let ([ht (make-eq-hashtable 4)] [index 0])
                (for-each (lambda (e)
                    (or (symbol? e)
                        (error 'make-enumeration "expected list of symbols, but got ~a" symbol-list))
                    (hashtable-set! ht e index)
                    (set! index (+ index 1)))
                    symbol-list)
                    
                (let ([type (construct-enum-type ht symbol-list)])
                    (enum-set type symbol-list)))))

    
    )