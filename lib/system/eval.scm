; Implementation of eval-core. This function interprets
; R5RS-like Scheme code. Before interpreting the code is
; compiled using closure-generating compiler. We do not
; use our bytecode pipeline for this because it is 
; quite expensive to compile code to bytecode and then
; load it into the VM. 

(define eval-core #f)
(let ()
    (define (interpret/preprocess expr env find-global)
        (let ([node (vector-ref expr 0)])
            (cond 
                [(eq? node '$gref) (interpret/global (vector-ref expr 1) find-global)]
                [(eq? node '$gset) 
                    (interpret/set-global 
                        (vector-ref expr 1) 
                        (interpret/preprocess (vector-ref expr 2) env find-global) 
                        find-global)]
                [(eq? node '$lref) 
                    (let ([addr (interpret/var-address (vector-ref expr 1) env)])
                        (if addr 
                            (interpret/lexical (car addr) (cdr addr))
                            (error 'interpret/preprocess "undefined variable" (vector-ref expr 1))))]
                [(eq? node '$lset)
                    (let ([addr (interpret/var-address (vector-ref expr 1) env)])
                        (if addr 
                            (interpret/set-lexical 
                                (car addr) 
                                (cdr addr) 
                                (interpret/preprocess 
                                    (vector-ref expr 2) 
                                    env find-global))
                            (error 'interpret/preprocess "undefined variable" (vector-ref expr 1))))]
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
                [(eq? node '$call)
                    (interpret/invoke expr env find-global)]
                [(eq? node '$let)
                    (interpret/let expr env find-global)]
                [(eq? node '$fix)
                    (interpret/let expr env find-global)]
                [(eq? node '$seq)
                    (let loop ([i 1] [len (vector-length expr)] [exprs '()])
                        (if (< i len)
                            (loop (+ i 1) len (cons (interpret/preprocess (vector-ref expr i) env find-global) exprs))
                            (interpret/sequence (reverse exprs))))]
                [else #f])))

    
    (define (interpret/let expr env find-global)
        (let ([bindings (vector-ref expr 1)] [lens (vector-length (vector-ref expr 1))])
             
            (let loop ([names '()] [inits '()] [i 0])
                (if (< i lens)
                    (loop (cons (vector-ref (vector-ref bindings i) 0) names) 
                        (cons (cons (vector-ref (vector-ref bindings i) 0) (vector-ref (vector-ref bindings i) 1)) inits) 
                        (+ i 1))
                (begin 
                    (let ([nenv (interpret/extend-env env names)]) (let (
                        [init-seq (map (lambda (binding)
                            (let ([assignment 
                                (interpret/preprocess (vector '$lset (car binding) (cdr binding)) nenv find-global)])
                                assignment)) inits)])
                        (let ([body (interpret/preprocess (vector-ref expr 2) nenv find-global)])
                            (lambda (renv)
                                (let ([renv (cons (make-vector (+ 1 lens) (undefined)) renv)])
                                (let loop ([init init-seq])
                                    (if (null? init)
                                        (body renv)
                                        (begin 
                                            ((car init) renv)
                                            (loop (cdr init)))))))))))))))

    (define (interpret/invoke expr env find-global)
        (let (
            [rands (vector-ref expr 2)] 
            [rator (interpret/preprocess (vector-ref expr 1) env find-global)] 
            [len (vector-length (vector-ref expr 2))])
            (let loop ([i 0])
                (if (< i len)
                    (begin 
                        (vector-set! rands i (interpret/preprocess (vector-ref rands i) env find-global))
                        (loop (+ i 1)))))

            (cond 
                [(eq? len 0)
                    (lambda (renv)
                        ((rator renv)))]
                [(eq? len 1)
                    (lambda (renv)
                        ((rator renv) ((vector-ref rands 0) renv)))]
                [(eq? len 2)
                    
                    (lambda (renv)
                        ((rator renv) ((vector-ref rands 0) renv) ((vector-ref rands 1) renv)))]
                [(eq? len 3)
                    (lambda (renv)
                        ((rator renv) ((vector-ref rands 0) renv) ((vector-ref rands 1) renv) ((vector-ref rands 2) renv)))]
                [(eq? len 4)
                    (lambda (renv)
                        ((rator renv) ((vector-ref rands 0) renv) ((vector-ref rands 1) renv) ((vector-ref rands 2) renv) ((vector-ref rands 3) renv)))]
                [else 
                    (lambda (renv)
                        ; evaluate arguments
                        (let loop ([i 0] [proc (rator renv)])
                            (if (< i len)
                                (begin 
                                    (vector-set! rands i ((vector-ref rands i) renv))
                                    (loop (+ i 1) proc)))
                            (apply (proc renv) (vector->list rands))))])))
    (define (interpret/var-address name env)
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
                (body (cons (vector self) renv)))])
                self)))
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
                    (error 'interpret/lambda-n "too few arguments" n)
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
                                    [else (error 'interpret/lambda-dot "too few arguments" n)]))))])
                self)))

    (define (interpret/make-proc expr env find-global)
        (let (
            [args (vector-ref expr 1)]
            [optarg (vector-ref expr 2)]
            [body (interpret/preprocess (vector-ref expr 3) (interpret/extend-env env (vector->list (vector-ref expr 1))) find-global)])
                (if optarg 
                    (interpret/lambda-dot (vector-length args) body)
                    (cond 
                        [(= (vector-length args) 0) (interpret/lambda0 body)]
                        [(= (vector-length args) 1) (interpret/lambda1 body)]
                        [(= (vector-length args) 2) (interpret/lambda2 body)]
                        [(= (vector-length args) 3) (interpret/lambda3 body)]
                        [(= (vector-length args) 4) (interpret/lambda4 body)]
                        [else (interpret/lambda-n (vector-length args) body)]))))

    (define default-find-global (lambda (name)
        (environment-get-cell (interaction-environment) name)))
 
    ; (eval-core expr #:optional env)
    (set! eval-core (lambda (x . rest)
        (let ([env (if (null? rest) (interaction-environment) (car rest))])

            (let ([clos (interpret/preprocess (%core-preprocess x) '() (lambda (name) (environment-get-cell env name)))])
                (clos '()))))))