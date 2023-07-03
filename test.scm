
(define-syntax let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))
    ((_ tag ((var val) ...) body ...)
      ((letrec ((tag (lambda (var ...) body ...)))
          tag) val ...))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
      (letrec "generate temp names"
        (var1 ...)
        ()
        ((var1 init1) ...)
          body ...))
    ((letrec "generate temp names"
        ()
        (temp1 ...)
        ((var1 init1) ...)
      body ...)
    (let ((var1 #f) ...)
    (let ((temp1 init1) ...)
    (set! var1 temp1)
    ...
    body ...)))
    ((letrec "generate temp names"
    (x y ...)
    (temp ...)
    ((var1 init1) ...)
    body ...)
    (letrec "generate temp names"
    (y ...)
    (newtemp temp ...)
    ((var1 init1) ...)
    body ...))))


(define (fac n) 
  (let loop ((n n) (acc 1))
    (if (= n 0)
        acc
        (loop (- n 1) (* acc n)))))


(define (fac-cps n k)
  (if (= n 0)
      (k 1)
      (fac-cps (- n 1) (lambda (v) (k (* v n))))))
(fac-cps 60000 (lambda (v) v))
0