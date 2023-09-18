
; dynamic-wind
;
; Snarfed from Lisp Pointers, V(4), October-December 1992, p45.
; Written by Jonathan Rees.
;
; FIXME #1
;
; This implementation works only with single thread. 
; Once we start worrying about actual threads, we'll need to
; update `*here*` to be thread-local parameter.
;
; FIXME #2
;
; Implement prompts. This will require modifying this file, we'll need
; to add new type of handlers.
; 

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
            ;(let ([result (thunk)])
            ;    (reroot! here)
            ;    result))))
            (call-with-values 
                thunk 
                (lambda results
                    (reroot! here)
                    (apply values results))))))

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

