(define-syntax dov
    (syntax-rules ()
    ((dov ((var init step ...) ...)
        (test expr ...)
            command ...)
     (letrec
        ((loop
            (lambda (var ...)
                (if test
                    (begin
                        (if #f #f)
                        expr ...)
                    (begin
                        command
                        ...
                        (loop (dov "step" var step ...)
                        ...))))))
        (loop init ...)))
    ((dov "step" x)
        x)
    ((dov "step" x y)
        y)))

(define s "hello, world!")
(dov ((n (string-length s))
    (i 0 (+ i 1)))
    ((= i n) (undefined))
(io/write-char (string-ref s i) p))