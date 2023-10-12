
(define (format port format-string . args)
  ;(print-raw port format-string)
  (let ((port (cond ((io/output-port? port) port)
                    ((eq? port #t) (current-output-port))
                    (else (error (errmsg 'msg:notopenoutputport) port)
                          #t)))
        (n    (string-length format-string)))

    (define (format-loop i args)
      (cond ((= i n))
            ((char=? (string-ref format-string i) #\~)
             (let ((c (string-ref format-string (+ i 1))))
               (cond ((char=? c #\~)
                      (write-char #\~ port)
                      (format-loop (+ i 2) args))
                     ((char=? c #\%)
                      (newline port)
                      (format-loop (+ i 2) args))
                     ((char=? c #\a)
                      (display-simple (car args) port)
                      
                      (format-loop (+ i 2) (cdr args)))
                     ((char=? c #\s)
                      (write-simple (car args) port)
                      (format-loop (+ i 2) (cdr args)))
                     ((char=? c #\c)
                      (write-char (car args) port)
                      (format-loop (+ i 2) (cdr args)))
                     ((or (char=? c #\b)
                          (char=? c #\B))
                      (let ((bv    (car args))
                            (radix (if (char=? c #\b) 10 16)))
                        (if (not (bytevector? bv))
                            (error 'format (errmsg 'msg:notbytevector) bv))
                        (do ((k 0 (+ k 1)))
                            ((= k (bytevector-length bv)))
                          (display (number->string (bytevector-ref bv k) radix)
                                   port)
                        (write-char #\space port)))
                      (format-loop (+ i 2) (cdr args)))
                     (else
                      (format-loop (+ i 1) args)))))
            (else
             (write-char (string-ref format-string i) port)
             (format-loop (+ i 1) args))))

    (format-loop 0 args)
    (undefined)))

; eof
