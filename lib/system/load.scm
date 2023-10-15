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

(define (read-code r)
    (let lp ([acc '()])
        (let ([c (read-annotated r)])
            (cond 
                [(eof-object? c) (reverse acc)]
                [(and (annotation? c) (eof-object? (annotation-expression c))) (reverse acc)]
                [else (lp (cons (strip-annotations c #t) acc))]))))


(define (compile-file file output-file)
    
    (let* ([comp (or output-file (compiled-file-name file))]
           [in (open-input-file file)]
           [src (cons 'begin (read-code (get-port-reader in file)))])
        (close-input-port in)
        (ensure-directory (dirname comp))
        (let ([out (open-file-output-port comp)])
            (write-bytevector (compile-tree-il src) out)
            (close-output-port out))))

(define %warn-auto-compilation-enabled 
    (let ([warned? #f])
        (lambda ()
            (unless warned?
                (display ";;; note: auto-compilation is enabled, set CAPY_AUTO_COMPILE=0 to disable\n" (current-error-port))
                (set! warned? #t)))))

; loads (in-vicinity dir file-name) and evaluates it in `eval-core`.
(define (primitive-load path)
    (let ([in (open-input-file path)])
        (let ([src (read-code (get-port-reader in #f))])
            (close-input-port in)
            (for-each eval src))))

(define (load-in-vicinity dir file-name)
    (define compiled-extension
        ;; File name extension of compiled files.
        (cond ((or (null? %load-compiled-extensions)
                (string-null? (car %load-compiled-extensions)))
            (warn "invalid %load-compiled-extensions"
                    %load-compiled-extensions)
            ".go")
            (else (car %load-compiled-extensions))))
    (define (more-recent? stat1 stat2)
        (mtime-newer? stat1 stat2))

    (define (fallback-file-name canon-file-name)
        ;; Return the in-cache compiled file name for source file
        ;; CANON-FILE-NAME.
        (and %compile-fallback-path
            (string-append %compile-fallback-path
                            canon-file-name
                            compiled-extension)))

    (define (compile file)
        (let ([cfn (compiled-file-name file-name)])
            (compile-file file cfn)
            cfn))

    (define (fresh-compiled-thunk name scmstat bc-file-name)
        (false-if-exception 
            (let ([bcstat (and (not %fresh-auto-compile) (file-modification-time bc-file-name))])
                
                (if (and bcstat (more-recent? bcstat scmstat))
                    (load-thunk-from-file bc-file-name)
                    (begin
                        (when bcstat 
                            (display (format ";;; note: source file ~a~%;;;        newer than compiled ~a~%" name bc-file-name)))
                        (cond 
                            [%load-should-auto-compile 
                                (%warn-auto-compilation-enabled)
                                (display (format ";;; compiling ~a~%" name) (current-error-port))
                                (let ([cfn (compile name)])
                                    (display (format ";;; compiled ~a~%" cfn) (current-error-port))
                                    (load-thunk-from-file cfn))]
                            [else #f]))))))
    (define (load-absolute abs-file-name)
        (define scmstat (file-modification-time abs-file-name))

        (define (pre-compiled)
            (or-map
                (lambda (dir)
                    (or-map
                        (lambda (ext)
                            (let ((candidate (string-append (in-vicinity dir file-name) ext)))
                                (let ((gostat (file-modification-time candidate)))
                                    (and gostat
                                        (more-recent? gostat scmstat)
                                        (false-if-exception
                                            (load-thunk-from-file candidate)
                                            candidate)))))
                    %load-compiled-extensions))
                %load-compiled-path))

        (define (fallback)
            (cond 
                [(false-if-exception (canonicalize-path abs-file-name))
                => (lambda (canon)
                    (let ([fn (fallback-file-name canon)])
                        (if fn 
                            (fresh-compiled-thunk abs-file-name scmstat fn)
                            #f)))]
                [else #f]))
        
        (let ([compiled (and scmstat (or (pre-compiled) (fallback)))])
            (if compiled 
                (compiled)
                (primitive-load abs-file-name))))
    (cond 
        [(absolute-path-string? file-name)
            (load-absolute file-name)]
        [(absolute-path-string? dir)
            (load-absolute (in-vicinity dir file-name))]
        [else 
            
            (load-absolute (in-vicinity dir file-name))]))

(define (load x)
    (let ([dir (dirname x)])
        (load-in-vicinity (or dir (getcwd)) x)))