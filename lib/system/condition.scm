(define make-condition-uid (lambda () (string->symbol "bruh")))

(define &condition
  (let* ((rtd (make-record-type-descriptor '&condition #f #f #f #f '#()))
         (rcd (make-record-constructor-descriptor rtd #f #f)))
    (make-record-type '&condition rtd rcd)))


(define compound-condition-component (lambda (obj) (tuple-ref obj 1)))

(define condition
  (lambda components
    (tuple
      'type:condition
      (apply
        append
        (map (lambda (component)
               (or (condition? component)
                   (assertion-violation 'condition (format "expected condition, but got ~r" component) components))
               (simple-conditions component))
             components)))))

(define compound-condition?
  (lambda (obj)
    (and (tuple? obj)
         (eq? 'type:condition (tuple-ref obj 0)))))

(define simple-condition?
  (lambda (obj)
    (and (record? obj)
         (rtd-ancestor? (record-type-rtd &condition) (record-rtd obj)))))

(define condition?
  (lambda (obj)
    (or (simple-condition? obj)
        (compound-condition? obj))))

(define simple-conditions
  (lambda (c)
    (cond ((simple-condition? c) (list c))
          ((compound-condition? c) (compound-condition-component c))
          (else
           (assertion-violation 'simple-conditions (format "expected condition, but got ~r" c))))))

(define condition-predicate
  (lambda (rtd)
    (or (rtd-ancestor? (record-type-rtd &condition) rtd)
        (assertion-violation
          'condition-predicate
          (format "expected record-type-descriptor of a subtype of &condition, but got ~r" rtd)))
    (lambda (obj)
      (cond ((simple-condition? obj) (rtd-ancestor? rtd (record-rtd obj)))
            ((compound-condition? obj)
             (any1 (lambda (component) (rtd-ancestor? rtd (record-rtd component))) (compound-condition-component obj)))
            (else #f)))))

(define condition-accessor
  (lambda (rtd proc)
    (define wrong-type
      (lambda (rtd obj)
        (assertion-violation
          "condition accessor"
          (format "expected condition of a subtype of ~s, but got ~r" rtd obj)
          rtd
          obj)))
    (or (rtd-ancestor? (record-type-rtd &condition) rtd)
        (assertion-violation
          'condition-accessor
          (format "expected record-type-descriptor of a subtype of &condition, but got ~r" rtd)
          rtd
          proc))
    (lambda (obj)
      (cond ((simple-condition? obj) (or (rtd-ancestor? rtd (record-rtd obj)) (wrong-type rtd obj)) (proc obj))
            ((compound-condition? obj)
             (cond ((any1
                      (lambda (component) (and (rtd-ancestor? rtd (record-rtd component)) component))
                      (compound-condition-component obj))
                    =>
                    proc)
                   (else (wrong-type rtd obj))))
            (else (wrong-type rtd obj))))))


(define &message
  (let ((rtd (make-record-type-descriptor '&message (record-type-rtd &condition) #f #f #f '#((immutable message)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&message rtd rcd))))

(define make-message-condition (record-constructor (record-type-rcd &message)))
(define message-condition? (condition-predicate (record-type-rtd &message)))
(define condition-message (condition-accessor (record-type-rtd &message) (record-accessor (record-type-rtd &message) 0)))
