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
         (error "Lists don't have the same length." (cons list1 lists)))
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

; 0: kind, 1: message, 2: stack-trace
(define exn-vtable (make-vtable "phphph"))

(define (exn? x)
    (if (struct? x)
        (eq? (struct-vtable x) exn-vtable)
        #f))

(define (exn-message exn)
    (if (exn? exn)
        (struct-ref exn 1)
        #f))

(define (exn-stack-trace exn)
    (if (exn? exn)
        (struct-ref exn 2)
        #f))

(define (exn-kind exn)
    (if (exn? exn)
        (struct-ref exn 0)
        #f))

; dynamic-wind
;
; Snarfed from Lisp Pointers, V(4), October-December 1992, p45.
; Written by Jonathan Rees.
;
; FIXME
;
; This implementation works only with single thread. 
; Once we start worrying about actual threads, we'll need to
; update `*here*` to be thread-local parameter.

(define *here* (list #f))
(define (call-with-current-continuation proc)
    (let ([here *here*])
        (%call/cc (lambda (cont)
            (proc (lambda results
                (reroot! here)
                (if (pair? results)
                    (if (null? (cdr results))
                        (cont (car results))))
                (apply cont results)))))))
(define call/cc call-with-current-continuation)

(define (reroot! there)
    (define (reroot-loop there)
        (if (not (eq? there *here*))
            (begin 
                (reroot-loop (cdr there))
                (let ([old-pair (car there)])
                    (let ([before (car old-pair)] [after (cdr old-pair)])
                        (set-car! *here* (cons after before))
                        (set-cdr! *here* there)
                        (set-car! there #f)
                        (set-cdr! there '()) 
                        (set! *here* there)
                        (before)
                    )))))
    (reroot-loop there))

(define (dynamic-wind before thunk after)
    (let ([here *here*])
        (let ([there (list #f)])
            (before)
            
            (set-car! *here* (cons after before))
            (set-cdr! *here* there)
            (set! *here* there)
            (let ([result (thunk)])
                (reroot! here)
                result))))
            ;(call-with-values 
            ;    thunk 
            ;    (lambda results
            ;        (reroot! here)
            ;        (apply values results))))))

(define (unhandled-exception-error val)
    (%raise val)) ; raise value to Rust runtime and panic

(define *basic-exception-handlers*
  (list unhandled-exception-error))

(define *current-exception-handlers* *basic-exception-handlers*)


(define (with-exception-handler handler thunk)
    (with-exception-handlers (cons handler *current-exception-handlers*) thunk))

(define (with-exception-handlers new-handlers thunk)
  (let ((previous-handlers *current-exception-handlers*)
        (new-handlers (if (null? new-handlers)
                          *basic-exception-handlers*
                          new-handlers)))
    (dynamic-wind
      (lambda ()
        (set! *current-exception-handlers* new-handlers))
      thunk
      (lambda ()
        (set! *current-exception-handlers* previous-handlers)))))


(define (raise obj)
    (let ([handlers *current-exception-handlers*])
        (with-exception-handlers (cdr handlers)
            (lambda ()
                ((car handlers) obj)
                (%raise "handler returned")))))

(with-exception-handler (lambda (exn)
    (print "caught:" exn))
    (lambda ()
        (raise "foo")))

(define x0 42)
(define x1 42)
(define x2 42)
(define x3 42)
(define x4 42)
(define x5 42)
(define x6 42)
(define x7 42)
(define x8 42)
(define x9 42)
(define x10 42)
(define x11 42)
(define x12 42)
(define x13 42)
(define x14 42)
(define x15 42)
(define x16 42)
(define x17 42)
(define x18 42)