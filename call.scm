(define-syntax .call 
    (syntax-rules (let* r4rs r5rs quote lambda
              boolean?
              car cdr
              vector-length vector-ref vector-set!
              bytevector-length bytevector-ref bytevector-set!
              bytevector-like-length bytevector-like-ref bytevector-like-set!
              bytevector-u8-ref bytevector-u8-set!
              bytevector-u16-ref bytevector-u16-set!
              bytevector-u16-native-ref bytevector-u16-native-set!
              bignum-length bignum-ref bignum-set!                      ; FIXME
              string-length string-ref string-set!
              make-ustring ustring-length ustring-ref ustring-set!
              list vector
              cadddr cddddr cdddr caddr cddr cdar cadr caar
              make-vector make-bytevector make-string
              endianness big little
              = < > <= >= + * - /
              abs negative? positive? min max nan?
              exact-integer?
              square
              div mod
              fx= fx< fx> fx<= fx>=                ; FIXME
              fx=? fx<? fx>? fx<=? fx>=?
              fxzero? fxpositive? fxnegative?
              fxmin fxmax
              fx+ fx- fx*
              fxnot fxand fxior fxxor fxif
              fxeven? fxodd?
              fl=? fl<? fl>? fl<=? fl>=?
              flzero? flpositive? flnegative?
              flmin flmax flabs
              flfloor flceiling fltruncate flround
              fl+ fl- fl* fl/
              inexact
              eqv? memv assv memq
              map for-each
              char=? char<? char>? char<=? char>=?
              lookahead-u8 get-u8
              lookahead-char get-char put-char
              peek-char read-char write-char
              record-ref:bummed                    ; FIXME
              record-set!:bummed                   ; FIXME
              native-endianness
              text?
              text-length
              %text-length
              text-ref
              %text-ref)
        ((_ let* (let* () body1 body2 ...))
            (begin body1 body2 ...))
        ((_ let* (let* ((name1 val1) (name2 val2) ...)
            body1 body2 ...))
            (let ((name1 val1))
                (let* ((name2 val2) ...)
                    body1 body2 ...)))
        ((_ + (+)) 0)
        ((_ + (+ ?e)) (+ ?e 0))
        ((_ + (+ ?e1 ?e2 ?e3 ?e4 ...))
            (+ (+ ?e1 ?e2) (+ ?e3 43 ?e4 ...)))

        ((_ * (*)) 1)
        ((_ * (* ?e)) (* ?e 1))
        ((_ * (* ?e1 ?e2 ?e3 ?e4 ...))
            (* (* ?e1 ?e2) (* ?e3 ?e4 ...)))

        ((_ - (- ?e)) (- 0 ?e))
        ((_ - (- ?e1 ?e2 ?e3 ?e4 ...))
            (- (- ?e1 ?e2) (- ?e3 ?e4 ...)))

        ((_ / (/ ?e)) (/ 1 ?e))
        ((_ / (/ ?e1 ?e2 ?e3 ?e4 ...))
            (/ (/ ?e1 ?e2) (/ ?e3 ?e4 ...)))
        ((_ car (car x0)) (.car:pair x0))
        ((_ cdr (cdr x0)) (.cdr:pair x0))
        ((_ caar (caar ?e))
            (car (car ?e)))
        ((_ cadr (cadr ?e))
            (car (cdr ?e)))
        ((_ cdar (cdar ?e))
            (cdr (car ?e)))
        ((_ cddr (cddr ?e))
            (cdr (cdr ?e)))
        ((_ cadddr (cadddr ?e))
            (car (cdr (cdr (cdr ?e)))))

        ((_ cddddr (cddddr ?e))
            (cdr (cdr (cdr (cdr ?e)))))

        ((_ cdddr (cdddr ?e))
            (cdr (cdr (cdr ?e))))

        ((_ caddr (caddr ?e))
            (car (cdr (cdr ?e))))
        ((_ = (= ?e1 ?e2 ?e3 ?e4 ...))
            (let* ((t1 ?e1)
                (t2 ?e2)
                (t3 (= t2 ?e3 ?e4 ...)))
            (if (= t1 t2) t3 #f)))

        ((_ ?proc ?expr) ?expr)

    )
)

