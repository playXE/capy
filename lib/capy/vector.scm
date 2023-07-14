(select-module capy)
(define vector-map
  (lambda (proc vec1 . vec2)
    (list->vector
     (apply map proc (vector->list vec1)
            (map vector->list vec2)))))

(define vector-for-each
  (lambda (proc vec1 . vec2)
    (apply for-each proc (vector->list vec1)
           (map vector->list vec2))))
