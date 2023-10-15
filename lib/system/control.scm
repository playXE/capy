
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
            (call-with-values 
                thunk 
                (lambda results
                    (reroot! here)
                    (apply values results))))))


; FIXME: Very hacky implmenetation.
;
; We define our own continuation object in Rust
; but in Scheme we wrap it inside lambda. So
; we have to check captured variable to see if 
; it is a continuation object. 
(define continuation? 
    ; save previous definition of continuation? to local variable,
    ; it is a correct function but expects different type of continuation.
    (let ([continuation? continuation?])
        (lambda (x)
            (and 
                (procedure? x)
                (= (program-num-free-vars x) 2)
                (continuation? (program-ref x 1))))))

(define (%extract-continuation-object x)
    (unless (continuation? x)
        (assertion-violation '%extract-continuation-object "not a continuation" x))
    (program-ref x 1))

(define make-stack 
    ; wrapper over native `make-stack`. It extracts continuation object
    ; from captured variable and passes it to Rust.
    (let ([make-stack make-stack])
        (lambda (obj . args)
            (if (continuation? obj)
                (apply make-stack (%extract-continuation-object obj) args)
                (apply make-stack obj args)))))

(define (unhandled-exception-error val)
   (let ([out (current-output-port)])
    (display "An exception has been raised, but no exception handler is installed \n" out)
    (if (continuation-condition? val)
        (print-frames (stack->vector (make-stack (condition-continuation val))) out))
    (flush-output-port out)
    (print-condition val)
    
    (%raise val)
    )
   
   ) 
(define (print-condition exn)
  (cond ((condition? exn)
         (let ((c* (simple-conditions exn)))
           (display "An unhandled condition was raised:\n")
           (do ((i 1 (fx+ i 1))
                (c* c* (cdr c*)))
               ((null? c*))
             (let* ((c (car c*))
                    (rtd (record-rtd c)))
               (display " ")
               (let loop ((rtd rtd))
                 (display (record-type-name rtd))
                 (cond ((record-type-parent rtd) =>
                        (lambda (rtd)
                          (unless (eq? rtd (record-type-descriptor &condition))
                            (display " ")
                            (loop rtd))))))
               (let loop ((rtd rtd))
                 (do ((f* (record-type-field-names rtd))
                      (i 0 (+ i 1)))
                     ((= i (vector-length f*))
                      (cond ((record-type-parent rtd) => loop)))
                   (newline)
                   (display "  ")
                   (display (vector-ref f* i))
                   (display ": ")
                   (let ((x ((record-accessor rtd i) c)))
                     (cond ((and #f (eq? rtd (record-type-descriptor &irritants))
                                 (pair? x) (list? x))
                            (display "(")
                            (let ((list-x 0))
                              (write (car x))
                              (for-each (lambda (value)
                                          (newline)
                                          (display "     ")
                                          (write value))
                                        (cdr x)))
                            
                            (display ")"))
                           (else
                            
                            (write x)))))))
             (newline))))
        (else
         (display "A non-condition object was raised:\n")
         (write exn))))
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

(define (raise-continuable obj)
  (let ((handlers *current-exception-handlers*))
    (with-exception-handlers (cdr handlers)
      (lambda () ((car handlers) obj)))))

(define (stack-length stack)
    (unless (stack? stack)
        (error 'stack-length "not a stack" stack))
    (tuple-ref stack 1))

(define (stack-frame stack)
    (unless (stack? stack)
        (error 'stack-frame "not a stack" stack))
    (tuple-ref stack 2))

(define (stack->vector stack)
  (let* ((len (stack-length stack))
         (v (make-vector len)))
    (if (positive? len)
        (let lp ((i 0) (frame (stack-ref stack 0)))
          (if (< i len)
              (begin
                (vector-set! v i frame)
                (lp (+ i 1) (frame-previous frame))))))
    v))

(define (print-frame frame . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
        (let ([ip (number->string (frame-instruction-pointer frame) 16)]
              [name (frame-procedure-name frame)]
              [loc (frame-source-location frame)])
            (cond 
                [(and loc name) 
                    (write-string 
                        (format "   at ~a (~a:~a:~a) ip=~a~%" name (vector-ref loc 0) (vector-ref loc 1) (vector-ref loc 2) ip)
                        p)]
                [loc
                    (write-string 
                        (format "   at <anonymous> (~a:~a:~a)~%" (vector-ref loc 0) (vector-ref loc 1) (vector-ref loc 2))
                        p)]
                [name
                    (write-string 
                        (format "   at ~a~%" name)
                        p)]
                [else 
                    (write-string 
                        (format "   at <anonymous> at ~a~%" ip) p)]))))

(define (print-frames frames . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
        (let ([len (vector-length frames)])
            (let lp ((i 0))
                (if (< i len)
                    (begin
                        (print-frame (vector-ref frames i) p)
                        (lp (+ i 1))))))))


(define-syntax false-if-exception
    (syntax-rules ()
        ((_ body ...)
            (call/cc (lambda (k)
                (with-exception-handler
                    (lambda (x)
                        (k #f))
                    (lambda ()
                        body ...)))))))