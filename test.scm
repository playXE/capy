(define (display x . rest)
    (put-string (current-output-port) (format "~a" x)))

(define (newline)
    (put-string (current-output-port) "\n"))

(display "Hello, world!")
(newline)