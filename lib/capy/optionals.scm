(select-module capy)

(define-syntax let-optionals
  (syntax-rules ()
    ((_ expr ((v d) ... . tail) . body)
     ($let-optionals (v ...) () (d ...) () f tail expr body))))

(define-syntax $let-optionals
  (syntax-rules ()

    ((_ () (vt ...) _ (cl ...) f tail expr body)
     (letrec ((f (case-lambda cl ... ((vt ... . tail) . body))))
       (apply f expr)))

    ((_ (vrf . vr*) (vt ...) (df . dr*) (cl ...) f . tailexprbody)
     ($let-optionals vr* (vt ... vrf) dr* (cl ... ((vt ...) (f vt ... df))) f . tailexprbody))))
