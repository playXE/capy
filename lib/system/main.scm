
(define file-port (open-text-input-file "lib/system/main.scm"))

(define r (make-reader (current-input-port) #f))

(define (read-datum-repl r)
    (display "> ")
    (flush-output-port (current-output-port))
    (read-datum r))

(with-exception-handler 
    (lambda (e)
        (format #t "~a: ~a (irritants: ~a) ~%" (condition-who e) (condition-message e) (condition-irritants e)))
    (lambda ()
        (let loop ([t (read-datum-repl r)])
            (if (eof-object? t)
                (display "EOF\n")
                (begin
                    (let ([res (eval-core t)])
                        (unless (undefined? res)
                            (format #t "Result: ~a ~%" res)))
                    (flush-output-port (current-output-port))
                    (loop (read-datum-repl r))
                )))))

