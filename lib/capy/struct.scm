(select-module capy)

(define-syntax struct 
    (er-macro-transformer 
        (lambda (expr rename compare)
            (define (config-has-name? config)
                (cond 
                    [(pair? config) (or (compare (car config) (rename '#:constructor-name))
                                        (compare (car config) (rename '#:extra-constructor-name))
                                        (config-has-name? (cdr config)))]
                    [else #f]))

                (match expr 
                    [(_ (? identifier? id) (? identifier? super-id) fields . config)
                        (if (not (config-has-name? config))
                            `(define-struct/derived ,expr (,id ,super-id) ,fields #:constructor-name ,id . ,config)
                            `(define-struct/derived ,expr (,id ,super-id) ,fields . ,config))]
                    [(_ (? identifier? id) fields . config) 
                        (if (not (config-has-name? config))
                            `(define-struct/derived ,expr ,id ,fields #:constructor-name ,id . ,config)
                            `(define-struct/derived ,expr ,id ,fields . ,config))]
                    [_ (erorr 'struct "invalid syntax, expected (struct <id> <super-id> (<field> ...))")]))))

(define (%make-struct-field-index fields)
    (lambda (id)
        (if (identifier? id)
            (let loop ([pos 0] [fields fields])
                (cond 
                    [(null? fields)
                        (error 'struct "no such field ~a" id)]
                    [(free-identifier=? id (car fields))
                        pos]
                    [else (loop (+ pos 1) (cdr fields))])))))
(define-syntax define-struct/derived 
    (er-macro-transformer
        (lambda (expr rename compare)
            (define make-field list)
            (define field-id car)
            (define field-default-value cadr)
            (define field-auto? caddr)
            (define field-mutable? cadddr)
            (define (build-name . parts)
                (string->symbol 
                    (apply string-append
                        (map (lambda (p)
                                (cond 
                                    [(identifier? p) (symbol->string (unwrap-syntax p #f))]
                                    [else p]))
                            parts))))
            (define (parse-field f)
                (match f 
                    [(? identifier? id)
                        (make-field id #f #f #f)]
                    [_ (error 'struct "bad syntax;\n expected a field identifier or a paranthesized identifier and field-specification sequence ~a" expr)]))

            (define (lookup config s)
                (cdr (assq s config)))

            (define (extend-config config s val)
                (cond
                    [(null? config) (error 'struct "internal error: can't find config element: ~s" s)]
                    [(eq? (caar config) s) (cons (cons s val) (cdr config))]
                    [else (cons (car config) (extend-config (cdr config) s val))]))

             (define (parse-props fm p super-id)
                (let loop ([p p]
                           [config '((#:super . #f)
                                     (#:auto-value . #f)
                                     (#:props . ())
                                     (#:mutable . #f)
                                     (#:guard . #f)
                                     (#:constructor-name . #f)
                                     (#:only-constructor? . #f)
                                     (#:name . #f)
                                     (#:only-name? . #f))]
                                     
                            [nongen? #f])
                                     
                    (cond
                        [(null? p) config]
                        [(eq? '#:super (car p))
                            (when (lookup config '#:super)
                                (error 'struct "duplicate #:super in ~a" expr))
                            (when super-id
                                (error 'struct "bad syntax;\n #:super specification disallowed because a struct subtype id was\n supplied with the struct id ~a" (cadr expr)))
                            (loop (cddr p)
                                (extend-config config '#:super (cadr p)) nongen?)]
                        [(memq (car p) '(#:guard #:auto-value))
                            (let ([key (car p)])
                                (when (lookup config key)
                                    (error 'struct "duplicate ~s in ~a" key expr))
                                (when (and nongen?
                                            (eq? key '#:guard))
                                        (error 'struct "cannot provide ~a for prefab structure type" key))
                                (loop (cddr p) (extend-config config key (cadr p)) nongen?))]
                        [(eq? '#:property (car p))
                            (when nongen?
                                (error 'struct "cannot use ~a for prefab structure type" (car p)))
                            (loop
                                (cdddr p)
                                (extend-config config 
                                                '#:props
                                                (cons (cons (cadr p) (caddr p)) (lookup config '#:props)))
                                nongen?)]
                        [(or (eq? '#:constructor-name (car p))
                             (eq? '#:extra-constructor-name (car p)))
                            (when (lookup config '#:constructor-name)
                                (error 'struct "multiple #:constructor-name or #:extra-constructor-name(s): ~a" (car p)))
                            (unless (identifier? (cadr p))
                                (error 'struct "bad syntax;\n expected an identifier for ~a, but found ~s" (car p) (cadr p)))
                            (loop (cddr p)
                                (extend-config (extend-config config '#:constructor-name (cadr p))
                                            '#:only-constructor?
                                            (eq? '#:constructor-name (car p)))
                                nongen?)]
                        [(or (eq? '#:name (car p)) (eq? '#:extra-name (car p)))
                            (when (lookup config '#:name)
                                (error 'struct "multiple" "#:name or #:extra-name: ~a" (car p)))
                            (unless (identifier? (cadr p))
                                (error 'struct "need an identifier after ~a ~a" (car p) (cadr p)))
                            (loop (cddr p)
                                    (extend-config (extend-config config '#:name (cadr p))
                                                '#:only-name?
                                                (eq? '#:name (car p)))
                                    nongen?)]
                        [(memq (car p)
                                '(#:mutable))
                            (let ([key (car p)])
                                (when (lookup config key)
                                    (error 'struct "redundant ~a" (car p)))
                                (loop (cdr p)
                                    (extend-config config key #t)
                                    nongen?))]
                        [else (error 'struct "bad syntax: ~a" expr)]
                    )))
           
            (match expr 
                [(_ (fm . _) id (field ...) prop ...)
                    (let ([def (let-values ([(id super-id)
                        (match id 
                            [(? identifier?) (values id #f)]
                            [((? identifier? id) (? identifier? super-id)) (values id super-id)]
                            [_ (error 'struct "bad syntax;\n expected <id> for structure-type name or (<id> <id>) for name and supertype\n name ~s" id)]
                        )
                    ])
                        (let* ([fields (map parse-field field)]
                               [dup (not (list-of-unique-symbols? (map field-id fields)))])
                            (when dup 
                                (error 'struct "duplicate field identifier: ~a" expr))
                            (let ([auto-count 
                                    (let loop ([fields fields] [auto? #f])
                                        (cond 
                                            [(null? fields) 0]
                                            [(field-auto? (car fields))
                                                (+ 1 (loop (cdr fields) #t))]
                                            [auto? 
                                                (error 'struct "non-auto field '~a' follows auto field in ~a" expr (car fields))]
                                            [else (loop (cdr fields) #f)])
                                        )])
                                        (let*-values ([(super-expr props auto-val guard ctor-name ctor-only? mutable? info-name name-only?)
                                            (let ([config (parse-props fm prop super-id)])
                                                (values (lookup config '#:super)
                                                        (lookup config '#:props)
                                                        (lookup config '#:auto-value)
                                                        (lookup config '#:guard)
                                                        (lookup config '#:constructor-name)
                                                        (lookup config '#:only-constructor?)
                                                        (lookup config '#:mutable)
                                                        (lookup config '#:name)
                                                        (lookup config '#:only-name?)))]
                                                        
                                                     [(self-ctor?)
                                                        (and ctor-name or (and (not name-only?)
                                                            (bound-identifier=? id ctor-name)))])
                                            
                                            (let ([struct: (build-name "struct:" id)]
                                                  [make- (if ctor-name 
                                                            ctor-name
                                                            (build-name "make-" id))]
                                                  [? (build-name id "?")]
                                                  [sels (map (lambda (f)
                                                                (build-name id "-" (field-id f))) fields)]
                                                  [super-struct: 
                                                        (and super-expr `(let ([the-super ,super-expr])
                                                            (if (,(rename 'struct-type?) the-super)
                                                                the-super 
                                                                (check-struct-type ,fm the-super))))])
                                                (let-values ([(sets sets-auto-count)
                                                    (let loop ([fields fields])
                                                        (cond 
                                                            [(null? fields) (values '() 0)]
                                                            [(not (or mutable? (field-mutable? (car fields))))
                                                                (loop (cdr fields))]
                                                            [else 
                                                                (let-values ([(other-sets count)
                                                                    (loop (cdr fields))])
                                                                    (let* ([count* (if (field-auto? (car fields)) (+ count 1) count)]
                                                                           [this-set (build-name "set-" id "-" (field-id (car fields)) "!")])
                                                                           
                                                                        (values (cons this-set other-sets) count*)))]
                                                        ))
                                                ])
                                                    0
                                                    `(,(rename 'define-values) (,struct: ,make- ,? ,@sels ,@sets)
                                                        (let-values ([(struct: make- ? -ref -set!)
                                                            (make-struct-type ',struct:
                                                                              ,super-struct:
                                                                              ,(- (length fields) auto-count)
                                                                              ,auto-count
                                                                              ,auto-val
                                                                              ,(if (null? props)
                                                                                '()
                                                                                `(list ,@(map (lambda (p) `(cons ,(car p) ,(cdr p))) props)))
                                                                              ,guard
                                                                              ',(if ctor-only? ctor-name id))
                                                        
                                                        ])
                                                            (values 
                                                                struct: make- ?
                                                                ,@(let loop ([i 0] [fields fields])
                                                                    (if (null? fields)
                                                                        '()
                                                                        (cons `(make-struct-field-accessor -ref ,i ',(field-id (car fields)))
                                                                                (loop (+ i 1) (cdr fields)))))
                                                                ,@(let loop ([i 0] [fields fields])
                                                                    (if (null? fields)
                                                                        '()
                                                                        (if (not (or mutable? (field-mutable? (car fields))))
                                                                            (loop (+ i 1) (cdr fields))
                                                                            (cons `(make-struct-field-mutator -set! ,i ',(field-id (car fields)))
                                                                                (loop (+ i 1) (cdr fields)))
                                                                        )
                                                                    )))))))))))]) def)]
                        
                [_ (error 'struct "bad syntax")])
        )
    )
)