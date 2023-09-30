(define (identity x) x)
(define (find proc list)
    (let loop ([rest list])
        (if (null? rest)
            #f
            (let ([car (car rest)])
                (if (proc car)
                    car
                    (loop (cdr rest)))))))
                    
(define-syntax define-quantifier
  (syntax-rules ()
    ((_ <name> <base-value> <terminating-value?>)
     (define (<name> proc list1 . lists)
       (define (length-error)
         (error (quote <name>) "Lists don't have the same length." (cons list1 lists)))
       (if (null? list1)
           ;; Careful with recursion on for-all.
           (if (or (null? lists) (for-all null? lists))
               <base-value>
               (length-error))
           (let loop ((car1 (car list1))
                      (cars (map car lists))
                      (cdr1 (cdr list1))
                      (cdrs (map cdr lists)))
             (if (null? cdr1)
                 (if (for-all null? cdrs)
                     (apply proc car1 cars)
                     (length-error))
                 (let ((value (apply proc car1 cars)))
                   (if (<terminating-value?> value)
                       value
                       (loop (car cdr1) (map car cdrs)
                             (cdr cdr1) (map cdr cdrs)))))))))))

(define-quantifier for-all #t not)
(define-quantifier exists #f identity)

(define (cons* . args)
    (if (null? args)
        '()
        (cons (car args) (cons* (cdr args)))))


(define (vector-ref v i)
    (vector-ref v i))
(define (vector-set! v i x)
    (vector-set! v i x))

(define (vector-length v)
    (vector-length v))


(define (assq key alist)
  (let loop ((alist alist))
    (if (null? alist)
        #f
        (let ((pair (car alist)))
          (if (eq? key (car pair))
              pair
              (loop (cdr alist)))))))

(define (assv key alist)
  (let loop ((alist alist))
    (if (null? alist)
        #f
        (let ((pair (car alist)))
          (if (eqv? key (car pair))
              pair
              (loop (cdr alist)))))))

(define (assoc key alist)
  (let loop ([alist alist])
    (if (null? alist)
        #f
        (let ([pair (car alist)])
          (if (equal? key (car pair))
              pair
              (loop (cdr alist)))))))


(define (list-ref ls i)
    (let loop ((ls ls) (i i))
        (if (= i 0)
            (car ls)
            (loop (cdr ls) (- i 1)))))

(define (list-head ls n)
    (if (= n 0)
        '()
        (cons (car ls) (list-head (cdr ls) (- n 1)))))

(define (list-tail ls n)
    (if (= n 0)
        ls
        (list-tail (cdr ls) (- n 1))))

(define (split-at ls n)
    (values (list-head ls n) (list-tail ls n)))



(define any1
  (lambda (pred lst)
    (and (not (null? lst))
         (or (pred (car lst)) (any1 pred (cdr lst))))))

(define any2
  (lambda (pred lst1 lst2)
    (and (not (null? lst1))
         (not (null? lst2))
         (or (pred (car lst1) (car lst2))
             (any2 pred (cdr lst1) (cdr lst2))))))

(define filter
  (lambda (pred lst)
    (let loop ((lst lst))
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (loop (cdr lst))))
            (else (loop (cdr lst)))))))

(define partition
  (lambda (pred lst)
    (let loop ((lst lst) (acc1 '()) (acc2 '()))
      (cond ((null? lst) (values (reverse acc1) (reverse acc2)))
            ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2))
            (else (loop (cdr lst) acc1 (cons (car lst) acc2)))))))


(define (zero? x)
    (= x 0))

(define (positive? x)
    (> x 0))

(define (negative? x)
    (< x 0))

(define (memv obj list)
    (let loop ((list list))
        (if (null? list)
            #f
            (if (eqv? obj (car list))
                list
                (loop (cdr list))))))

(define (memq obj list)
    (let loop ((list list))
        (if (null? list)
            #f
            (if (eq? obj (car list))
                list
                (loop (cdr list))))))

(define (reverse list)
    (let loop ((list list) (acc '()))
        (if (null? list)
            acc
            (loop (cdr list) (cons (car list) acc)))))

(define (append . lists)
    (if (null? lists)
        '()
        (let loop ((lists lists))
            (if (null? (cdr lists))
                (car lists)
                (append2 (car lists) (loop (cdr lists)))))))

(define (append2 list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append2 (cdr list1) list2))))

; 'primitive' procedures definition. Compiler recognizes calls to `car`, `cdr`, `+` etc. as primitive calls
; and is able to emit OP_CAR, OP_CDR, OP_PLUS and others. Originally these procedures are not defined
; so we define them in boot library to allow users to use them as bindings basically in calls like `(map car ls)`.

(define (cdr x) (cdr x))
(define (car x) (car x))
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (pair? x) (pair? x))
(define (null? x) (null? x))
(define (vector? x) (vector? x))
(define (string? x) (string? x))
(define (number? x) (number? x))
(define (boolean? x) (boolean? x))
(define (symbol? x) (symbol? x))
(define (char? x) (char? x))


(define (+ . args)
    (let loop ([args args] [acc 0])
        (if (null? args)
            acc
            (loop (cdr args) (+ (car args) acc)))))

(define (- arg . args)
    (let loop ([diff arg] [args args])
        (if (null? args)
            diff
            (loop (- diff (car args)) (cdr args)))))
(define (* . args)
    (let loop ([args args] [acc 1])
        (if (null? args)
            acc
            (loop (cdr args) (* (car args) acc)))))

(define (/ . args)
    (let loop ([args args] [acc 1])
        (if (null? args)
            acc
            (loop (cdr args) (/ acc (car args))))))

(define (make-nary-comparison name binop)
    (lambda (a b . rest)
        (if (null? rest)
            (binop a b)
            (if (binop a b)
                (let loop ([first b] [rest rest])
                    (if (null? rest)
                        #t
                        (if (binop first (car rest))
                            (loop (car rest) (cdr rest))
                            #f)))
                #f))))

(define = (make-nary-comparison '= (lambda (a b) (= a b))))
(define < (make-nary-comparison '< (lambda (a b) (< a b))))
(define > (make-nary-comparison '> (lambda (a b) (> a b))))
(define <= (make-nary-comparison '<= (lambda (a b) (<= a b))))
(define >= (make-nary-comparison '>= (lambda (a b) (>= a b))))


(define (list->vector list)
    (let ([len (length list)])
        (let ([vec (make-vector len)])
            (let loop ([list list] [i 0])
                (if (null? list)
                    vec
                    (begin
                        (vector-set! vec i (car list))
                        (loop (cdr list) (+ i 1))))))))

(define (vector->list vec)
    (let loop ([vec vec] [i 0] [len (vector-length vec)] [acc '()])
        (if (= i len)
            (reverse acc)
            (loop vec (+ i 1) len (cons (vector-ref vec i) acc)))))

(define (length list)
    (let loop ([list list] [len 0])
        (if (null? list)
            len
            (loop (cdr list) (+ len 1)))))

(define (map proc list)
    (let loop ([list list] [acc '()])
        (if (null? list)
            (reverse acc)
            (loop (cdr list) (cons (proc (car list)) acc)))))

(define (call-with-values producer consumer)
    ; use call-with-values call that is recognized by 
    ; compielr as a `let-values` form. This is a hack
    ; to define call-with-values in Scheme instead of
    ; in the VM using raw bytecode. Output Tree IL is like this:
    ;
    ; (let-values ((results (producer))
    ;           (apply consumer results))) 
    
    (call-with-values 
        (lambda () (producer))
        (lambda results
            (apply consumer results))))

(define (for-each proc list)
    (let loop ([list list])
        (if (null? list)
            #t
            (begin
                (proc (car list))
                (loop (cdr list))))))

(define eof-object #f)
(define eof-object? #f)

(let ([eof-object-internal (make-tuple 1)]) 
    (tuple-set! eof-object-internal 0 'type:eof-object)
    (set! eof-object (lambda () eof-object-internal))
    (set! eof-object? (lambda (x) (eq? x eof-object-internal))))

(define (assert . rest) #f)