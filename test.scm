(define src (make-file-input-port "test.scm"))

(define (read-to-eof port)
    (define val (read port))
    (if (eof-object? val)
        '()
        (cons val (read-to-eof port))))

(displayln (read-to-eof src))