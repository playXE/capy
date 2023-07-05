(select-module capy)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cdar x) (cdr (car x)))
(define caaar #f)
(define caadr #f)
(define cadar #f)
(define caddr #f)
(define cdaar #f)
(define cdadr #f)
(define cddar #f)
(define cdddr #f)
(define caaaar #f)
(define caaadr #f)
(define caadar #f)
(define caaddr #f)
(define cadaar #f)
(define cadadr #f)
(define caddar #f)
(define cadddr #f)
(define cdaaar #f)
(define cdaadr #f)
(define cdadar #f)
(define cdaddr #f)
(define cddaar #f)  
(define cddadr #f)
(define cdddar #f)
(define cddddr #f)


(let-syntax (
    (%define-cxr 
        (syntax-rules ()
            ((_ name a b)
                (set! name (lambda (x) (a (b x))))))))
        (%define-cxr caaar car caar)
        (%define-cxr caadr  car  cadr)
        (%define-cxr cadar  car  cdar)
        (%define-cxr caddr  car  cddr)
        (%define-cxr cdaar  cdr  caar)
        (%define-cxr cdadr  cdr  cadr)
        (%define-cxr cddar  cdr  cdar)
        (%define-cxr cdddr  cdr  cddr)
        (%define-cxr caaaar caar caar)
        (%define-cxr caaadr caar cadr)
        (%define-cxr caadar caar cdar)
        (%define-cxr caaddr caar cddr)
        (%define-cxr cadaar cadr caar)
        (%define-cxr cadadr cadr cadr)
        (%define-cxr caddar cadr cdar)
        (%define-cxr cadddr cadr cddr)
        (%define-cxr cdaaar cdar caar)
        (%define-cxr cdaadr cdar cadr)
        (%define-cxr cdadar cdar cdar)
        (%define-cxr cdaddr cdar cddr)
        (%define-cxr cddaar cddr caar)
        (%define-cxr cddadr cddr cadr)
        (%define-cxr cdddar cddr cdar)
        (%define-cxr cddddr cddr cddr))


(define find #f)
(define for-all #f)
(define exists #f)
(define partition #f)
(define filter #f)
(define fold-left #f)
(define fold-right #f)
(define remp #f)
(define remv #f)
(define remq #f)
(define remove #f)
(define assp #f)
(define assv #f)
(define assq #f)
(define assoc #f)
(define take #f)
(define drop #f)
(define break #f)
(define member #f)
(define memv #f)
(define memq #f)
(define memp #f)
(define map #f)
(define list-copy #f)

(let () 
    (define collect-cdr
        (lambda (lst)
            (let loop ((lst lst))
                (cond ((null? lst) '())
                    ((null? (cdar lst)) (loop (cdr lst)))
                    (else (cons (cdar lst) (loop (cdr lst))))))))

    (define for-all-n
        (lambda (pred list-of-lists)
        (let ((argc (length list-of-lists)))

            (define collect-car
                (lambda (lst)
                    (let loop ((lst lst))
                        (cond 
                            ((null? lst) '())
                            ((pair? (car lst))
                                (cons (caar lst) (loop (cdr lst))))
                            (else
                                (raise-argument-error 'for-all "list?" list-of-lists))))))

            (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
            (or (= (length head) argc)
                (raise-argument-error 'for-all "expected same length chains of pairs" list-of-lists))
            (if (null? rest)
                (apply pred head)
                (and (apply pred head)
                    (loop (collect-car rest) (collect-cdr rest))))))))

    (define exists-n
        (lambda (pred list-of-lists)
        (let ((argc (length list-of-lists)))

            (define collect-car
            (lambda (lst)
                (let loop ((lst lst))
                (cond ((null? lst) '())
                        ((pair? (car lst))
                        (cons (caar lst) (loop (cdr lst))))
                        (else
                        (error 'exists (format "traversal reached to non-pair element ~s" (car lst))))))))

            (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
            (or (= (length head) argc)
                (raise-argument-error 'exists "expected same length chains of pairs" list-of-lists))
            (if (null? rest)
                (apply pred head)
                (or (apply pred head)
                    (loop (collect-car rest) (collect-cdr rest))))))))

    (define for-all-n-quick
        (lambda (pred lst)
        (or (null? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
                (if (null? rest)
                    (apply pred head)
                    (and (apply pred head)
                        (loop (car rest) (cdr rest))))))))

  (define exists-n-quick
    (lambda (pred lst)
      (and (pair? lst)
           (let loop ((head (car lst)) (rest (cdr lst)))
             (if (null? rest)
                 (apply pred head)
                 (or (apply pred head)
                     (loop (car rest) (cdr rest))))))))

  (define map-1 
    (lambda (proc lst)
      (if (null? lst)
          '()
          (cons (proc (car lst)) (map-1 proc (cdr lst))))))
  (define map-n-quick
    (lambda (proc lst)
      (if (null? lst)
          '()
          (let loop ((head (car lst)) (rest (cdr lst)))
            (if (null? rest)
                (list (proc head))
                (cons (proc head) (loop (car rest) (cdr rest))))))))


  (define map-n 
    (lambda (proc list-of-lists)
      (let ((argc (length list-of-lists)))
        (define collect-car
          (lambda (lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((pair? (car lst))
                     (cons (caar lst) (loop (cdr lst))))
                    (else
                     (raise-argument-error 'map "list?" list-of-lists))))))

        (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
          (or (= (length head) argc)
              (raise-argument-error 'map "expected same length chains of pairs" list-of-lists))
          (if (null? rest)
              (apply proc head)
              (cons (apply proc head) (loop (collect-car rest) (collect-cdr rest))))))))
  (define for-all-1
    (lambda (pred lst)
      (cond ((null? lst) #t)
            ((pair? lst)
             (let loop ((head (car lst)) (rest (cdr lst)))
               (cond ((null? rest) (pred head))
                     ((pair? rest)
                      (and (pred head)
                           (loop (car rest) (cdr rest))))
                     (else
                      (and (pred head)
                           (error 'for-all "traversal reached to non-pair element ~s" rest))))))
            (else
             (raise-argument-error 'for-all (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

  (define exists-1
    (lambda (pred lst)
      (cond ((null? lst) #f)
            ((pair? lst)
             (let loop ((head (car lst)) (rest (cdr lst)))
               (cond ((null? rest) (pred head))
                     ((pred head))
                     ((pair? rest) (loop (car rest) (cdr rest)))
                     (else
                      (error 'exists (format "traversal reached to non-pair element ~s" rest) (list pred lst))))))
            (else
             (raise-argument-error 'exists (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

    (define fold-left-1
        (lambda (proc seed lst)
        (cond ((null? lst) seed)
                (else
                (fold-left-1 proc (proc seed (car lst)) (cdr lst))))))

    (define fold-left-n
        (lambda (proc seed lst)
        (cond ((null? lst) seed)
                (else
                (fold-left-n proc (apply proc (append (list seed) (car lst))) (cdr lst))))))

     (define fold-right-1
        (lambda (proc seed lst)
            (cond ((null? lst) seed)
                    (else
                    (proc (car lst) (fold-right-1 proc seed (cdr lst)))))))

    (define fold-right-n
        (lambda (proc seed lst)
            (cond ((null? lst) seed)
                    (else
                    (apply proc (append (car lst) (list (fold-right-n proc seed (cdr lst)))))))))


    (set! find
        (lambda (pred lst)
            (cond 
                [(null? lst) #f]
                [(pair? lst)
                    (let loop ([head (car lst)] [rest (cdr lst)] [orig lst])
                        (cond 
                            [(pred head) head]
                            [(null? rest) #f]
                            [(pair? rest) (loop (car rest) (cdr rest) orig)]
                            [else 
                                (raise-argument-error 'find "list?" orig)]))]
                [else 
                    (raise-argument-error 'find "list?" lst)])))    

    (set! for-all 
        (lambda (pred lst1 . lst2)
            (cond ((null? lst2)
                    (for-all-1 pred lst1))
                    ((apply list-transpose+ lst1 lst2)
                    => (lambda (lst) (for-all-n-quick pred lst)))
                    (else
                    (for-all-n pred (cons lst1 lst2))))))

    (set! exists 
        (lambda (pred lst1 . lst2)
            (cond ((null? lst2)
                    (exists-1 pred lst1))
                    ((apply list-transpose+ lst1 lst2)
                    => (lambda (lst) (exists-n-quick pred lst)))
                    (else
                    (exists-n pred (cons lst1 lst2))))))   

    (set! filter
        (lambda (pred lst)
            (let loop ((lst lst))
                (cond ((null? lst) '())
                    ((pred (car lst)) (cons (car lst) (loop (cdr lst))))
                    (else (loop (cdr lst)))))))

    (set! partition 
        (lambda (pred lst)
            (let loop ((lst lst) (acc1 '()) (acc2 '()))
                (cond ((null? lst) (values (reverse acc1) (reverse acc2)))
                    ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2))
                    (else (loop (cdr lst) acc1 (cons (car lst) acc2)))))))

    (set! fold-left 
        (lambda (proc seed lst1 . lst2)
            (if (null? lst2)
                (if (list? lst1)
                    (fold-left-1 proc seed lst1)
                    (raise-argument-error 'fold-left "list?" lst1))
                (cond ((apply list-transpose+ lst1 lst2)
                        => (lambda (lst) (fold-left-n proc seed lst)))
                        (else
                        (error 'fold-left "expected same length proper lists: ~a ~a" lst1 lst2))))))

    (set! fold-right 
        (lambda (proc seed lst1 . lst2)
            (if (null? lst2)
                (if (list? lst1)
                    (fold-right-1 proc seed lst1)
                    (raise-argument-error 'fold-right "list?" lst1))
                (cond ((apply list-transpose+ lst1 lst2)
                        => (lambda (lst) (fold-right-n proc seed lst)))
                        (else
                        (error 'fold-right "expected same length proper lists ~a" (cons* proc seed lst1 lst2)))))))

    (set! remp 
        (lambda (pred lst)
            (let loop ((lst lst))
                (cond ((null? lst) '())
                    ((pred (car lst))
                        (loop (cdr lst)))
                    (else
                        (cons (car lst)
                                (loop (cdr lst))))))))

    (set! remove 
        (lambda (obj lst)
            (let loop ((lst lst))
                (cond ((null? lst) '())
                    ((equal? (car lst) obj)
                        (loop (cdr lst)))
                    (else
                        (cons (car lst)
                            (loop (cdr lst))))))))

    (set! remv
        (lambda (obj lst)
            (let loop ((lst lst))
                (cond ((null? lst) '())
                    ((eqv? (car lst) obj)
                        (loop (cdr lst)))
                    (else
                        (cons (car lst)
                            (loop (cdr lst))))))))
    (set! remq 
        (lambda (obj lst)
            (let loop ((lst lst))
                (cond ((null? lst) '())
                    ((eq? (car lst) obj)
                        (loop (cdr lst)))
                    (else
                        (cons (car lst)
                            (loop (cdr lst))))))))
    (set! memp 
        (lambda (proc lst)
            (cond
            ((null? lst) #f)
            ((proc (car lst)) lst)
            (else
                (memp proc (cdr lst))))))

    (set! assp 
        (lambda (proc lst)
            (cond
            ((null? lst) #f)
            ((proc (caar lst)) (car lst))
            (else
                (assp proc (cdr lst))))))

    (set! assv 
        (lambda (obj alist)
            (cond
            ((null? alist) #f)
            ((eqv? obj (caar alist)) (car alist))
            (else
                (assv obj (cdr alist))))))

    (set! assoc
        (lambda (obj alist)
            (cond
            ((null? alist) #f)
            ((equal? obj (caar alist)) (car alist))
            (else
                (assoc obj (cdr alist))))))
    
    (set! member
        (lambda (obj lst)
            (cond
            ((null? lst) #f)
            ((equal? obj (car lst)) lst)
            (else
                (member obj (cdr lst))))))
    
    (set! memv
        (lambda (obj lst)
            (cond
            ((null? lst) #f)
            ((eqv? obj (car lst)) lst)
            (else
                (memv obj (cdr lst))))))

    (set! memq
        (lambda (obj lst)
            (cond
            ((null? lst) #f)
            ((eq? obj (car lst)) lst)
            (else
                (memq obj (cdr lst))))))

    (set! list-copy
        (lambda (lst)
            (cond
            ((null? lst) '())
            ((pair? lst) (cons (car lst) (list-copy (cdr lst))))
            (else lst))))

    (set! map 
        (lambda (proc lst1 . lst2)
            (cond ((null? lst2)
                    (map-1 proc lst1))
                    ((apply list-transpose+ lst1 lst2)
                    => (lambda (lst) (map-n-quick proc lst)))
                    (else
                    (map-n proc (cons lst1 lst2)))))))
