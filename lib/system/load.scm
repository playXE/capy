; The 'load' procedure.
;
;
; It is a generic procedure. First definition simply reads and evaluates code or
; loads bytecode image (ends with `.capy`). After macro-expander is installed
; `load-evaluator` is replaced with it. If we see `.capy` we instantly load it.

(define load-evaluator 
    (make-parameter "load-evaluator"
        (lambda (expr env)
            (let ([old-env (interaction-environment)])
                (dynamic-wind 
                    (lambda () (interaction-environment env))
                    (lambda () 
                        (eval expr env))
                    (lambda () 
                        (if (eq? (interaction-environment) env)
                            (interaction-environment old-env))
                    ))))
                    procedure?))

(define load-print
  ;; If not #f, print the return value(s) of each form as it is
  ;; loaded.
  (make-parameter "load-print" #f))

(define load-verbose
  ;; If not #f, print the file name before loading.
  (make-parameter "load-verbose" #f))

(define (compiled-file-name file)
    (define (compiled-extension)
        (cond 
            [(or (null? %load-compiled-extensions)
                 (= (length %load-compiled-extensions) 0))
                ".capy"]
            [else (car %load-compiled-extensions)]))
    (and %compile-fallback-path 
        (let ([f (string-append 
                    %compile-fallback-path
                    (canonicalize-path file)
                    (compiled-extension))]) f)))

(define macro-expander (make-parameter "macroexpander" (lambda (x) x) procedure?))

(define (compile-tree-il x)
    (%core-compile ((macro-expander) x)))

(define (compile-file file output-file)
    (define (read-file p)
        (let loop ([acc '()])
            (let ([expr (read p)])
                (cond 
                    [(eof-object? expr) 
                        (close-input-port p)
                        (reverse acc)]
                    [else (loop (cons expr acc))]))))
    (let* ([comp (or output-file (compiled-file-name file))]
           [in (open-input-file file)]
           [src (read-file in)])
        
        (ensure-directory (dirname comp))
        (let ([out (open-file-output-port comp)])
            (write-bytevector (compile-tree-il src) out)
            (close-output-port out))))

; The second argument is a thunk, usually interaction-environment.
;
; The optional third argument is a read procedure.
(define (load-from-port p get-environment . maybe-read)
    (let ([read (if (null? maybe-read) read (car maybe-read))])
        ;; The environment must be recomputed for each expression evaluation --
        ;; the loaded expressions may change the interaction environment, and
        ;; when the environment is implicit, that change should be reflected in
        ;; subsequent evaluations.

        (do ([expr (read p) (read p)])
            [(eof-object? expr)]
            (call-with-values 
                (lambda () ((load-evaluator) expr (get-environment)))
                (lambda vals 
                    (if (load-print) (for-each (lambda (value)
                        (newline (current-output-port))
                        (write-string ";    " (current-output-port))
                        (write value (current-output-port))
                        (flush-output-port (current-output-port)))))
                    vals))))
    (undefined))


(define (load filename . rest)
    (let ((get-environment
        (cond 
            ((null? rest)
                interaction-environment)
            ((null? (cdr rest))
                (let ((env (car rest)))
                    (lambda () env)))
            (else
                (error 'load "too many arguments")
                #t))))
        (define (load-file)
            (cond 
                [(string-ends-with? filename ".capy")
                    ((load-image-from-file filename))
                    #t]
                [else 
                    (call-with-port
                        (open-input-file filename)
                        (lambda (p)
                            (load-from-port p get-environment)))]))
        (load-file)))

