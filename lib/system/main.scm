
(define file-port (open-text-input-file "lib/system/main.scm"))

(define r (make-reader file-port #f))

(with-exception-handler 
    (lambda (e)
        (format #t "~a: ~a (irritants: ~a) ~%" (condition-who e) (condition-message e) (condition-irritants e)))
    (lambda ()
        (let loop ([t (read-datum r)])
            (if (eof-object? t)
                (display "EOF\n")
                (begin
                    (write t)
                    (display "\n")
                    (loop (read-datum r))
                )))))