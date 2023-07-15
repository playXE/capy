(define-module capy.io.support
    (export port-type
            port-direction
            port-lookup-file-option-code
            port-lookup-buffer-mode-code
            port-lookup-codec-code
            port-lookup-eol-style-code
            port-lookup-error-handling-mode-code
            port-reverse-lookup-codec-code
            port-reverse-lookup-eol-style-code
            port-reverse-lookup-error-handling-mode-code
            file-options)



    (define direction-codes
        '((input . 1) (output . 2) (input/output . 3 )))

    (define type-codes
        '((file . 1) (bytevector . 2) (custom . 3)))
    (define file-option-codes 
        '((no-create . 1) (no-fail . 2) (no-truncate . 4)))

    (define buffer-mode-codes 
        '((none . 1) (line . 2) (block . 3)))
    (define eol-style-codes
        '((none . 1) (lf . 2) (cr . 3) (crlf . 4) (nel . 5) (crnel . 6) (ls . 7)))

    (define codec-codes
        '((latin-1 . 1) (utf-8 . 2) (utf-16 . 3)))


    (define error-handling-mode-codes
        '((ignore . 1) (raise . 2) (replace . 3)))
        
    (define flip (lambda (lst) (map (lambda (e) (cons (cdr e) (car e))) lst)))

    (define flipped-codec-codes (flip codec-codes))

    (define flipped-eol-style-codes (flip eol-style-codes))

    (define flipped-error-handling-mode-codes (flip error-handling-mode-codes))

    (define lookup (lambda (obj alist) (cond ((assq obj alist) => cdr) (else #f))))
    
    (define-syntax file-options 
        (er-macro-transformer
            (lambda (expr rename compare)
                (match expr 
                    [(_ options ...)
                        (or (and (list-of-unique-symbols? options) (for-all port-lookup-file-option-code options))
                            (error 'file-options "invalid option ~a" options))]
                    [else (error 'file-options "invalid syntax")]))))

    

    (define-syntax port-type 
        (er-macro-transformer 
            (lambda (expr rename compare)
                (cond 
                    [(assq (cadr expr) type-codes) => cdr]
                    [else (error 'port-type "invalid port type")]))))

    ;; matches the following and returns valid direction code:
    ;; (port-direction input)
    ;; (port-direction output)
    ;; (port-direction input output)     
    ;; (port-direction output input)
    ;; otherwise throws an error
    (define-syntax port-direction 
        (er-macro-transformer
            (lambda (expr rename compare)
                (let (
                    [input. (rename 'input)]
                    [output. (rename 'output)])

                (if (= (length expr) 2)
                    (cond 
                        [(compare (cadr expr) input.) (lookup 'input direction-codes)]
                        [(compare (cadr expr) output.) (lookup 'output direction-codes)]
                        [else (error 'port-direction "invalid port direction")])
                    (cond 
                        [(and (compare (cadr expr) input.) (compare (caddr expr) output.)) (lookup 'input/output direction-codes)]
                        [(and (compare (cadr expr) output.) (compare (caddr expr) input.)) (lookup 'input/output direction-codes)]
                        [else (error 'port-direction "invalid port direction")]))))))

    (define port-lookup-file-option-code (lambda (obj) (lookup obj file-option-codes)))
    (define port-lookup-buffer-mode-code (lambda (obj) (lookup obj buffer-mode-codes)))
    (define port-lookup-codec-code (lambda (obj) (lookup obj codec-codes)))
    (define port-lookup-eol-style-code (lambda (obj) (lookup obj eol-style-codes)))
    (define port-lookup-error-handling-mode-code (lambda (obj) (lookup obj error-handling-mode-codes)))
    (define port-reverse-lookup-codec-code (lambda (obj) (lookup obj flipped-codec-codes)))
    (define port-reverse-lookup-eol-style-code (lambda (obj) (lookup obj flipped-eol-style-codes)))
    (define port-reverse-lookup-error-handling-mode-code (lambda (obj) (lookup obj flipped-error-handling-mode-codes))))

(define-module capy.io
    (import capy.io.support)
)