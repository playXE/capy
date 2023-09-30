(define *current-console-input*  #f)    ; There is only one!
(define *current-console-output* #f)    ; There is only one!
(define *current-console-error* #f)     ; There is only one!

(define *conio-input-firsttime* #f)
(define *conio-output-firsttime* #f)
(define *conio-error-firsttime* #f)

(define (osdep/initialize-console)     ; Must be called in every process.
  (set! *conio-input-firsttime* #t)
  (set! *conio-output-firsttime* #t)
  (set! *conio-error-firsttime* #t)
  #t)

(define (console-io/console-input-port)
  
      (let ((ccin *current-console-input*))
        (if (or (not (io/open-port? ccin))
                (io/port-error-condition? ccin)
                (io/port-at-eof? ccin))
            (begin (if (io/open-port? ccin)
                       #t)              ; FIXME: reap the descriptor.
                   (set! *current-console-input*
                         (console-io/open-input-console))))
        *current-console-input*))

(define (console-io/console-output-port)
      (let ((ccout *current-console-output*))
        (if (or (not (io/open-port? ccout))
                (io/port-error-condition? ccout))
            (begin (if (io/open-port? ccout)
                       #t)              ; FIXME: reap the descriptor
                   (set! *current-console-output* 
                         (console-io/open-output-console))))
        *current-console-output*))

(define (console-io/console-error-port)
      (let ((ccout *current-console-error*))
        (if (or (not (io/open-port? ccout))
                (io/port-error-condition? ccout))
            (begin (if (io/open-port? ccout)
                       #t)              ; FIXME: reap the descriptor
                   (set! *current-console-error* 
                         (console-io/open-error-console))))
        *current-console-error*))

(define (console-io/initialize)
  (osdep/initialize-console)
  (set! *current-console-input* (console-io/open-input-console))
  (set! *current-console-output* (console-io/open-output-console))
  (set! *current-console-error* (console-io/open-error-console)))

(define (console-io/ioproc op)
  (case op
    ((read)
     (console-io/with-retry (file-io/ioproc op)))
    ((write) 
     (file-io/ioproc op))               ; wrong if console is intermittent
    ((close) 
     (lambda (data)
       (let ((r (osdep/close-console (file-io/fd data))))
         (if (< r 0)
             'error
             'ok))))
    ((ready?)
     (lambda (data)
       (osdep/char-ready-console? (file-io/fd data))))
    ((name)
     (lambda (data)
       (file-io/name data)))
    (else 
     (error "console-io/ioproc: illegal operation: " op)
     #t)))

(define (console-io/with-retry proc)
  (lambda args
    (apply proc args)))

(define (console-transcoder)
  (default-transcoder))


(define (console-io/open-input-console)
  (let* ((fd (osdep/open-console 'input))
         (p (io/make-port console-io/ioproc
                          (file-io/data fd "*console-input*")
                          'binary
                          'input)))
    (transcoded-port p (console-transcoder))))

(define (console-io/open-output-console)
  (let* ((fd (osdep/open-console 'output))
         (p (io/make-port console-io/ioproc
                          (file-io/data fd "*console-output*")
                          'binary
                          'output
                          'flush)))
    (transcoded-port p (console-transcoder))))

(define (console-io/open-error-console)
  (let* ((fd (osdep/open-console 'error))
         (p (io/make-port console-io/ioproc
                          (file-io/data fd "*console-error*")
                          'binary
                          'output
                          'flush)))
    (transcoded-port p (console-transcoder))))
