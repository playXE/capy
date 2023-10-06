(define point (make-record-type-descriptor '&point #f #f #f #f '#((mutable x) (mutable y))))
(define point-rcd (make-record-constructor-descriptor point #f #f))
(define make-point (record-constructor point-rcd))

(define p (make-point 4 2))

(display p)
(newline)

(define p2 (tuple 'type:point 4 2))
(display p2)
(newline)

(define p3 (tuple 'point 4 2))
(display p3)
(newline)