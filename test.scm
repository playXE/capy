
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


(define (fib-cps n k)
  (if (< n 2)
      (k n)
      (fib-cps (- n 1)
               (lambda (n1)
                 (fib-cps (- n 2)
                          (lambda (n2)
                            (k (+ n1 n2))))))))

(define (fib-iter n)
  (let loop ((n n)
             (a 1)
             (b 0))
    (if (= n 0)
        b
        (loop (- n 1) (+ a b) a))))

(fib-iter 200000)
0