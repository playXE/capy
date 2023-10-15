(define assertion-violation
  (lambda (who message . irritants)
    ;(display (format "assertion violation: ~a: ~a ~%" who message))
    (if (or (null? who) (not who) (string? who) (symbol? who) (identifier? who))
        (if (string? message)
            (raise
              (call/cc 
                (lambda (k)
                  (if who 
                    (condition 
                      (make-assertion-violation)
                      (make-who-condition who)
                      (make-message-condition message)
                      (make-irritants-condition irritants)
                      (make-continuation-condition k))
                    (condition 
                      (make-assertion-violation)
                      (make-message-condition message)
                      (make-irritants-condition irritants)
                      (make-continuation-condition k))))))
            (assertion-violation 'assertion-violation (wrong-type-argument-message "string" message 2)))
        (assertion-violation 'assertion-violation (wrong-type-argument-message "string, symbol, or #f" who 1)))))


(define undefined-violation
  (lambda (who . message)
    (raise
      (call/cc (lambda (k)
        (apply
          condition
          (filter
            values
            (list
              (make-undefined-violation)
              (and who (make-who-condition who))
              (and (pair? message) (make-message-condition (car message))
              (make-continuation-condition k))))))))))


(define lexical-violation
  (lambda (who . message)
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-lexical-violation)
            (and who (make-who-condition who))
            (and (pair? message) (make-message-condition (car message)))))))))

(define syntax-violation
  (lambda (who message form . subform)
    (if (or (not who) (string? who) (symbol? who) (identifier? who))
        (if (string? message)
            (raise
              (apply
                condition
                (filter
                  values
                  (list
                    (make-syntax-violation form (and (pair? subform) (car subform)))
                    (if who
                        (make-who-condition who)
                        (cond ((let ((obj (if (wrapped-syntax-object? form) (unwrap-syntax form) form)))
                                 (cond ((identifier? obj) (original-id (syntax-object-expr obj)))
                                       ((and (pair? obj) (identifier? (car obj)))
                                        (original-id (syntax-object-expr (car obj))))
                                       (else #f)))
                               =>
                               make-who-condition)
                              (else #f)))
                    (make-message-condition message)))))
            (assertion-violation 'syntax-violation (wrong-type-argument-message "string" message 2)))
        (assertion-violation 'syntax-violation (wrong-type-argument-message "string, symbol, or #f" who 1)))))


(define error
  (lambda (who message . irritants)
    (if (or (null? who) (not who) (string? who) (symbol? who) (identifier? who))
        (if (string? message)
            (raise
              (apply
                condition
                (filter
                  values
                  (list
                    (make-error)
                    (and who (make-who-condition who))
                    (make-message-condition message)
                    (make-irritants-condition irritants)))))
            (assertion-violation 'error (wrong-type-argument-message "string" message 2)))
        (assertion-violation 'error (wrong-type-argument-message "string, symbol, or #f" who 1)))))

(define wrong-type-argument-message
  (lambda (expect got . nth)
    (if (null? nth)
        (format "expected ~a, but got ~a" expect got)
        (format "expected ~a, but got ~a, as argument ~a" expect got (car nth)))))