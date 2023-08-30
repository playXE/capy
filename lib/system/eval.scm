; Implementation of eval-core. This function interprets
; R5RS-like Scheme code. Before interpreting the code is
; compiled using closure-generating compiler. We do not
; use our bytecode pipeline for this because it is 
; quite expensive to compile code to bytecode and then
; load it into the VM. 

(let ()
    (define (interpret/preprocess expr env find-global)
        (let ([node (vector-ref expr 0)])
            (cond 
                [(eq? node '$gref) (interpret/global (vector-ref expr 1) find-global)]
                [(eq? node '$gset) \
                    (interpret/set-global 
                        (vector-ref expr 1) 
                        (interpret/preprocess (vector-ref expr 2) env find-global) 
                        find-global)]
                [(eq? node '$lref) 
                    (let ([addr (interpret/var-address (vector-ref expr 1) env)])
                        (if addr 
                            (interpret/lexical (car addr) (cdr addr))
                            (error "undefined variable" (vector-ref expr 1))))]
                [(eq? node '$lset)
                    (let ([addr (interpret/var-address (vector-ref expr 1) env)])
                        (if addr 
                            (interpret/set-lexical 
                                (car addr) 
                                (cdr addr) 
                                (interpret/preprocess 
                                    (vector-ref expr 2) 
                                    env find-global))
                            (error "undefined variable" (vector-ref expr 1))))]
                [(eq? node '$if)
                    (interpret/if 
                        (interpret/preprocess (vector-ref expr 1) env find-global)
                        (interpret/preprocess (vector-ref expr 2) env find-global)
                        (interpret/preprocess (vector-ref expr 3) env find-global))]
                [(eq? node '$const)
                    (let ([v (vector-ref expr 1)])
                        (lambda (renv) v))]
                [(eq? node '$it)
                    (lambda (renv) (undefined))]
                [(eq? node '$lambda)
                    (interpret/make-proc expr env find-global)]
                [else #f])))
    (define (interpret/var-address name env)
        (print env)
        (let r-loop ([env env] [i 0])
            (if (null? env)
                #f 
                (let a-loop ([rib (car env)] [j 1])
                    (cond 
                        [(null? rib) (r-loop (cdr env) (+ i 1))]
                        [(eq? (car rib) name) (cons i j)]
                        [else 
                            (a-loop (cdr rib) (+ j 1))])))))
    (define (interpret/extend-env env names)
        (cons names env))

    (define (interpret/global name find-global)
        (let ([cell (find-global name)])
            (lambda (renv)
                (let ([v (variable-ref-value cell)])
                    (if (undefined? v)
                        (error "undefined global variable" name)
                        v)))))
    (define (interpret/set-global name expr find-global)
        (let ([cell (find-global name)])
            (lambda (renv)
                (variable-ref-set-value! cell (expr renv)))))

    ; Fetches local variable from runtime environment
    ; we have multiple variants to speed-up execution
    ; if rib index is small enough
    (define (interpret/lexical0 offset)
        (lambda (renv)
            (vector-ref (car renv) offset)))
    (define (interpret/lexical1 offset)
        (lambda (renv)
            (vector-ref (cadr renv) offset)))
    (define (interpret/lexical2 offset)
        (lambda (renv)
            (vector-ref (caddr renv) offset)))
    (define (interpret/lexical3 offset)
        (lambda (renv)
            (vector-ref (cadddr renv) offset)))
    (define (interpret/lexical-n rib offset)
        (lambda (env0)
            (let loop ([rib rib] [env env0])
                (if (= rib 0)
                    (vector-ref (car env) offset)
                    (loop (- rib 1) (cdr env))))))

    (define (interpret/lexical rib offset)
        (cond 
            [(= rib 0) (interpret/lexical0 offset)]
            [(= rib 1) (interpret/lexical1 offset)]
            [(= rib 2) (interpret/lexical2 offset)]
            [(= rib 3) (interpret/lexical3 offset)]
            [else (interpret/lexical-n rib offset)]))
    (define (interpret/set-lexical rib offset expr)
        (lambda (env0)
            (let loop ([rib rib] [env env0])
                (if (= rib 0)
                    (vector-set! (car env) offset (expr env0))
                    (loop (- rib 1) (cdr env))))))
    (define (interpret/sequence exprs)
        (lambda (renv)
            (let loop ([exprs exprs])
                (if (null? (cdr exprs))
                    ((car exprs) renv)
                    (begin 
                        ((car exprs) renv)
                        (loop (cdr exprs)))))))
    (define (interpret/if test consequent alternative)
        (lambda (renv)
            (if (test renv)
                (consequent renv)
                (alternative renv))))
    
    (define (interpret/lambda0 body)
        (lambda (renv)
            (letrec ([self (lambda () 
                (body (cons (vector self) renv)))]))
                self))
    (define (interpret/lambda1 body)
        (lambda (renv)
            (letrec ([self (lambda (a0) 
                (body (cons (vector self a0) renv)))])
                self)))
    (define (interpret/lambda2 body)
        (lambda (renv)
            (letrec ([self (lambda (a0 a1) 
                (body (cons (vector self a0 a1) renv)))])
                self)))
    (define (interpret/lambda3 body)
        (lambda (renv)
            (letrec ([self (lambda (a0 a1 a2) 
                (body (cons (vector self a0 a1 a2) renv)))])
                self)))
    (define (interpret/lambda4 body)
        (lambda (renv)
            (letrec ([self (lambda (a0 a1 a2 a3) 
                (body (cons (vector self a0 a1 a2 a3) renv)))])
                self)))

    (define (interpret/lambda-n n body)
        (lambda (renv)
            (letrec ([self (lambda args
                (if (< (length args) n)
                    (error "too few arguments" n)
                    (body (cons (list->vector (cons self args)) env)))
            )])
            self)))

    (define (interpret/lambda-dot n body)
        (lambda (renv)
            (letrec (
                [self 
                    (lambda args
                        (let ([v (make-vector (+ n 2) (undefined))])
                              [limit (+ n 1)]
                            (vector-set! v 0 self)
                            (let loop ([argnum 1] [argtail args])
                                (cond 
                                    [(= argnum limit)
                                        (vector-set! v argunum argtail)
                                        (body (cons v env))]
                                    [(pair? argtail)
                                        (vector-set! v argnum (car argtail))
                                        (loop (+ argnum 1) (cdr argtail))]
                                    [else (error "too few arguments" n)]))))])
                self)))

    (define (interpret/make-proc expr env find-global)
        (let (
            [args (vector-ref expr 1)]
            [optarg (vector-ref expr 2)]
            [body (interpret/preprocess (vector-ref expr 3) (interpret/extend-env env (vector->list (vector-ref expr 1))) find-global)])
                (print expr)
                (if optarg 
                    (interpret/lambda-dot (vector-length args) body)
                    (cond 
                        [(= (vector-length args) 0) (interpret/lambda0 body)]
                        [(= (vector-length args) 1) (interpret/lambda1 body)]
                        [(= (vector-length args) 2) (interpret/lambda2 body)]
                        [(= (vector-length args) 3) (interpret/lambda3 body)]
                        [(= (vector-length args) 4) (interpret/lambda4 body)]
                        [else (interpret/lambda-n (vector-length args) body)]))))



    (let ([tree-il (%core-preprocess '(lambda (x y) y))])
        (print interpret/preprocess)
        (let ([closure (interpret/preprocess tree-il '() (lambda (name) #f))])
            (print ((closure '()) 1 2))))
)