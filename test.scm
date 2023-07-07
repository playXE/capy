
(require "capy")


(define (display x) (put-string (current-output-port) (format "~a" x)))
(define (newline) (put-string (current-output-port) "\n"))


(define p1 (make-parameter 1))
(define p2 (make-parameter 2))

(parameterize ([p1 4] [p2 5])
  (display (p1))
  (newline)
  (display (p2))
  (newline))

(display (p1))
(newline)
(display (p2))
(newline)