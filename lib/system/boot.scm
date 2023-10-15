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

(define (cons x y)
    (cons x y))

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

(define (not x)
  (not x))


(define member
  (lambda (item list0 . rest)
    (define (member3 item list equal?)
      (cond ((pair? list) (if (equal? item (car list))
                              list
                              (member3 item (cdr list) equal?)))
            ((null? list) #f)
            (else (error 'member (errmsg 'msg:notlist) list0))))
    (cond ((pair? rest)
           (let ((comp (car rest)))
             (if (and (procedure? comp)
                      (null? (cdr rest)))
                 (member3 item list0 comp)
                 (error 'member (errmsg 'msg:illegalargs)
                                (cons item (cons list0 rest))))))
          ((symbol? item) (memq item list0))
          ((number? item) (memv item list0))
          ((char? item) (memv item list0))
          (else (member3 item list0 equal?)))))

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

(define (memp pred list)
  (cond ((pair? list) (if (pred (car list))
                          list
                          (memp pred (cdr list))))
        ((null? list) #f)
        (else (error "memp: Improper list " list))))

(define (memp-not pred list)
  (cond ((pair? list) (if (pred (car list))
                          (memp pred (cdr list))
                          list))
        ((null? list) #f)
        (else (error "memp-not: Improper list " list))))

(define (append . args)

  (define (revapp x y)
    (do ((x x (cdr x))
         (r y (cons (car x) r)))
        ((not (pair? x))
         (if (null? x)
             r
             (assertion-violation 'append "illegal arguments" args)))))

  (define (append2 x y)
    (revapp (reverse x) y))

  (define (loop rest tail)
    (if (pair? rest)
        (loop (cdr rest)
              (append2 (car rest) tail))
        tail))

  (if (pair? args)
      (let ((a (reverse! args)))
        (loop (cdr a) (car a)))
      '()))


(define (unspecified) (undefined))

;; Reverse L while appending to R.  Although this looks like an
;; unusual thing to want to do, it comes in quite handy in a lot of
;; code.

(define (revappend left right)          ; non-destructive version
  (cond ((pair? left) (revappend (cdr left) (cons (car left) right)))
        ((null? left) right)
        (else (error "revappend: improper list " left))))

(define (reverse list)
  (revappend list '()))

; Probably due to JonL White.

(define (revappend! l r)
  (define (loop0 prev curr next)
    (set-cdr! curr prev)
    (if (pair? next)
        (loop1 (cdr next) curr next)
        curr))
  (define (loop1 next prev curr)
    (set-cdr! curr prev)
    (if (pair? next)
        (loop2 next (cdr next) curr)
        curr))
  (define (loop2 curr next prev)
    (set-cdr! curr prev)
    (if (pair? next)
        (loop0 curr next (cdr next))
        curr))
  (if (pair? l)
      (loop0 r l (cdr l))
      r))

(define (reverse! l)
  (revappend! l '()))

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
(define (-- x) (-- x))

(define +
  (letrec ((loop (lambda (sum args)
		   (if (null? args)
		       sum
		       (loop (+ sum (car args)) (cdr args))))))
    (lambda args
      (if (null? args)
	  0
	  (loop (car args) (cdr args))))))

(define - 
  (letrec ((loop (lambda (diff args)
		   (if (null? args)
		       diff
		       (loop (- diff (car args)) (cdr args))))))
    (lambda (arg . args)
      (if (null? args)
	  (-- arg)
	  (loop arg args)))))

(define * 
  (letrec ((loop (lambda (prod args)
		   (if (null? args)
		       prod
		       (loop (* prod (car args)) (cdr args))))))
    (lambda args
      (if (null? args)
	  1
	  (loop (car args) (cdr args))))))

(define /
  (letrec ((loop (lambda (quot args)
		   (if (null? args)
		       quot
		       (loop (/ quot (car args)) (cdr args))))))
    (lambda (arg . args)
      (if (null? args)
	  (/ 1 arg)
	  (loop arg args)))))
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
(define char=? (make-nary-comparison 'char=? (lambda (a b) (char=? a b))))
(define char<? (make-nary-comparison 'char<? (lambda (a b) (char<? a b))))
(define char>? (make-nary-comparison 'char>? (lambda (a b) (char>? a b))))
(define char<=? (make-nary-comparison 'char<=? (lambda (a b) (char<=? a b))))

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

; FIXME:  The performance of map can be improved.
; That doesn't matter so much because map is usually inlined.

(define (map f x . rest)

  (define (lists-of-different-lengths . args)
    (if (or (and (eq? 'r6rs (capy:execution-mode))
                 (not using-r7rs-semantics))
            (not (every? (lambda (x) (or (null? x) (pair? x)))
                         args)))
        (assertion-violation 'map
                             (errmsg 'msg:illegalargs)
                             (cons f (cons x rest)))
        '()))

  (define (map1 f x)
    (if (pair? x)
        (let* ((a (f (car x)))
               (b (map1 f (cdr x))))
          (cons a b))
        (if (null? x)
            '()
            (lists-of-different-lengths x))))

  (define (map2 f x y)
    (if (and (pair? x) (pair? y))
        (let* ((a (f (car x) (car y)))
               (b (map2 f (cdr x) (cdr y))))
          (cons a b))
        (if (and (null? x) (null? y))
            '()
            (lists-of-different-lengths x y))))

  (define (map3 f x y z)
    (if (and (pair? x) (pair? y) (pair? z))
        (let* ((a (f (car x) (car y) (car z)))
               (b (map3 f (cdr x) (cdr y) (cdr z))))
          (cons a b))
        (if (and (null? x) (null? y) (null? z))
            '()
            (lists-of-different-lengths x y z))))

  (define (map4 f x y z w)
    (if (and (pair? x) (pair? y) (pair? z) (pair? w))
        (let* ((a (f (car x) (car y) (car z) (car w)))
               (b (map4 f (cdr x) (cdr y) (cdr z) (cdr w))))
          (cons a b))
        (if (and (null? x) (null? y) (null? z) (null? w))
            '()
            (lists-of-different-lengths x y z w))))

  (define (mapn f lists)
    (cond ((every? pair? lists)
           (let* ((a (apply f (map car lists)))
                  (b (mapn f (map1 cdr lists))))
             (cons a b)))
          ((every? null? lists)
           '())
          (else
           (apply lists-of-different-lengths lists))))

  (case (length rest)
    ((0)  (map1 f x))
    ((1)  (map2 f x (car rest)))
    ((2)  (map3 f x (car rest) (cadr rest)))
    ((3)  (map4 f x (car rest) (cadr rest) (caddr rest)))
    (else (mapn f (cons x rest)))))

(define (or-map proc list)
    (let loop ([list list])
        (if (null? list)
            #f
            (let ([res (proc (car list))])
                (if res
                    res
                    (loop (cdr list)))))))

(define (and-map proc list)
    (let loop ([list list])
        (if (null? list)
            #t
            (let ([res (proc (car list))])
                (if res
                    (loop (cdr list))
                    res)))))



(define (for-each f x . rest)

  (define (lists-of-different-lengths . args)
    (if (or (and (eq? 'r6rs (larceny:execution-mode))
                 (not using-r7rs-semantics))
            (not (every? (lambda (x) (or (null? x) (pair? x)))
                         args)))
        (assertion-violation 'for-each
                             (errmsg 'msg:illegalargs)
                             (cons f (cons x rest)))
        (unspecified)))

  (define (map1 f x)
    (if (pair? x)
        (cons (f (car x)) (map1 f (cdr x)))
        '()))

  (define (for-each1 f x)
    (if (pair? x)
        (begin (f (car x))
               (for-each1 f (cdr x)))
        (if (null? x)
            (unspecified)
            (lists-of-different-lengths x))))

  (define (for-each2 f x y)
    (if (and (pair? x) (pair? y))
        (begin (f (car x) (car y))
               (for-each2 f (cdr x) (cdr y)))
        (if (and (null? x) (null? y))
            (unspecified)
            (lists-of-different-lengths x y))))

  (define (for-each3 f x y z)
    (if (and (pair? x) (pair? y) (pair? z))
        (begin (f (car x) (car y) (car z))
               (for-each3 f (cdr x) (cdr y) (cdr z)))
        (if (and (null? x) (null? y) (null? z))
            (unspecified)
            (lists-of-different-lengths x y z))))

  (define (for-each4 f x y z w)
    (if (and (pair? x) (pair? y) (pair? z) (pair? w))
        (begin (f (car x) (car y) (car z) (car w))
               (for-each4 f (cdr x) (cdr y) (cdr z) (cdr w)))
        (if (and (null? x) (null? y) (null? z) (null? z))
            (unspecified)
            (lists-of-different-lengths x y z w))))

  (define (for-each-n f lists)
    (cond ((every? pair? lists)
           (apply f (map car lists))
           (for-each-n f (map1 cdr lists)))
          ((every? null? lists)
           (unspecified))
          (else
           (apply lists-of-different-lengths lists))))

  (case (length rest)
    ((0)  (for-each1 f x))
    ((1)  (for-each2 f x (car rest)))
    ((2)  (for-each3 f x (car rest) (cadr rest)))
    ((3)  (for-each4 f x (car rest) (cadr rest) (caddr rest)))
    (else (for-each-n f (cons x rest)))))
(define eof-object #f)
(define eof-object? #f)

(let ([eof-object-internal (make-tuple 1)]) 
    (tuple-set! eof-object-internal 0 'type:eof-object)
    (set! eof-object (lambda () eof-object-internal))
    (set! eof-object? (lambda (x) (eq? x eof-object-internal))))

(define (assert . rest) #f)

(define **nul** 0)
(define **alarm** 7)
(define **backspace** 8)
(define **tab** 9)
(define **linefeed** 10)
(define **vtab** 11)
(define **page** 12)
(define **return** 13)
(define **esc** 27)
(define **space** 32)
(define **delete** 127)

(define **newline** 10)

(define (identifier? x) (symbol? x))
(define (starts-with? s prefix)
    (and (>= (string-length s) (string-length prefix))
         (string=? (substring s 0 (string-length prefix)) prefix)))

(define (get-tuple-type-name tuple)
    (if (symbol? (tuple-ref tuple 0))
        (if (starts-with? (symbol->string (tuple-ref tuple 0)) "type:")
            (substring (symbol->string (tuple-ref tuple 0)) 5)
            #f)))

(define (string->list s)
    (let loop ([s s] [i 0] [len (string-length s)] [acc '()])
        (if (= i len)
            (reverse acc)
            (loop s (+ i 1) len (cons (string-ref s i) acc)))))

(define (bitwise-arithmetic-shift-left x y)
    (if (>= y 0)
        (bitwise-arithmetic-shift x y)
        (bitwise-arithmetic-shift x (-- y))))

(define (bitwise-arithmetic-shift-right x y)
    (if (>= y 0)
        (bitwise-arithmetic-shift x (-- y))
        (bitwise-arithmetic-shift x y)))

(define (bitwise-if ei1 ei2 ei3)
    (bitwise-ior (bitwise-and ei1 ei2)
             (bitwise-and (bitwise-not ei1) ei3)))

(define (bitwise-copy-bit ei1 ei2 ei3)
    (let* ((mask (bitwise-arithmetic-shift 1 ei2)))
        (bitwise-if mask
                    (bitwise-arithmetic-shift ei3 ei2)
                    ei1)))

(define (string-null? s)
    (= (string-length s) 0))

(define file-name-separator-string "/")
(define (file-name-separator? c)
  (char=? c #\/))
(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
                (if (zero? len)
                    #f
                    (string-ref vicinity (- len 1))))))
    (string-append vicinity
                   (if (or (not tail) (file-name-separator? tail))
                       ""
                       file-name-separator-string)
                   file)))

(define (string-copy s)
    (substring s 0 (string-length s)))

(define (list . args) args)


(define (every? p l . ls)

  (define (complain)
    (assertion-violation 'for-all "illegal arguments" (cons p (cons l ls))))

  (define (every1 a)
    (cond ((pair? a)
           (if (null? (cdr a))
               (p (car a))
               (and (p (car a))
                    (every1 (cdr a)))))
          ((null? a) #t)
          (else (complain))))

  (define (every2 a b)
    (cond ((and (pair? a) (pair? b))
           (if (null? (cdr a))
               (if (null? (cdr b))
                   (p (car a) (car b))
                   (complain))
               (and (p (car a) (car b))
                    (every2 (cdr a) (cdr b)))))
          ((and (null? a) (null? b))
           #t)
          (else (complain))))

  (define (every-n arglists)
    (cond ((pair? arglists)
           (if (null? (cdr arglists))
               (apply p (car arglists))
               (and (apply p (car arglists))
                    (every-n (cdr arglists)))))
          ((null? arglists) #t)
          (else (complain))))

  (cond ((null? ls) (every1 l))
        ((null? (cdr ls))
         (every2 l (car ls)))
        (else
         (let ((arglists (apply map list l ls)))
           (every-n arglists)))))


(define (some? p l . ls)

  (define (complain)
    (assertion-violation 'for-all "illegal arguments" (cons p (cons l ls))))

  (define (some1 a)
    (cond ((pair? a)
           (if (null? (cdr a))
               (p (car a))
               (or (p (car a))
                   (some1 (cdr a)))))
          ((null? a) #f)
          (else (complain))))

  (define (some2 a b)
    (cond ((and (pair? a) (pair? b))
           (if (null? (cdr a))
               (if (null? (cdr b))
                   (p (car a) (car b))
                   (complain))
               (or (p (car a) (car b))
                   (some2 (cdr a) (cdr b)))))
          ((and (null? a) (null? b))
           #f)
          (else (complain))))

  (define (some-n arglists)
    (cond ((pair? arglists)
           (if (null? (cdr arglists))
               (apply p (car arglists))
               (or (apply p (car arglists))
                   (some-n (cdr arglists)))))
          ((null? arglists) #f)
          (else (complain))))

  (cond ((null? ls) (some1 l))
        ((null? (cdr ls))
         (some2 l (car ls)))
        (else
         (let ((arglists (apply map list l ls)))
           (some-n arglists)))))

