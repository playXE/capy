(define *files-open* #f)

(define (file-io/data fd name)
    (cons fd name))
(define (file-io/fd data)
    (car data))
(define (file-io/name data)
    (cdr data))

(define (file-io/ioproc op)
    (case op
        [(read)
            file-io/read]
        [(write)
            file-io/write]
        [(close)
            file-io/close]
        [(ready?)
            file-io/ready?]
        [(name)
            file-io/name]
        [(set-position!)
            file-io/set-position!]
        [else 
            (error 'file-io/ioproc "unknown operation" op)]))

(define whence:seek-set          0)     ; offset is absolute
(define whence:seek-cur          1)     ; offset is relative to current
(define whence:seek-end          2)     ; offset is relative to end

(define (file-io/ready? data) #t)
(define (file-io/close data)
    (file-io/close-file data))

(define (file-io/close-file data)
    (let ([r (osdep/close-file (file-io/fd data))])
        (if (< r 0)
            'error 
            'ok)))

(define (file-io/port-position-as-binary data)
    (let ([r (osdep/lseek-file (fiel-io/fd data) 0 whence:seek-cur)])
        (if (>= r 0) r 'error)))

(define (file-io/set-position! data offset)
    (let ([r (osdep/lseek-file (file-io/fd data) offset whence:seek-set)])
        (if (>= r 0) r 'error)))

(define (file-io/read-bytes fd buffer)
    (let ([r (osdep/read-file fd buffer (bytevector-length buffer))])
        (if (< r 0)
            'error
            (if (= r 0)
                'eof
                r))))

(define (file-io/write-bytes fd buffer n offset)
    (let ([k (osdep/write-file fd buffer n offset)])
        (cond 
            [(< k 0) 'error]
            [(= k n) k]
            [else (file-io/write-bytes fd buffer (- n k) (+ offset k))])))

(define (file-io/open-file filename . modes)
    (let* ((io-mode (if (memq 'input modes) 'input 'output))
         (tx-mode (if (memq 'binary modes) 'binary 'text))
         (fd      (osdep/open-file filename io-mode tx-mode))) 
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p (if (eq? 'binary tx-mode)
                    (io/make-port file-io/ioproc data io-mode tx-mode 'set-position!)
                    (io/make-port file-io/ioproc data io-mode tx-mode))))
            (file-io/install-port-position-as-binary! p data)
            p)
        (error 'file-io/open-file "todo: raise exception"))))
                
(define (file-io/install-port-position-as-binary! p data)
  (let ((get-position
         (lambda ()
           (file-io/port-position-as-binary data))))
    (io/port-alist-set! p
                        (cons (cons 'port-position-in-bytes get-position)
                              (io/port-alist p)))))


(define (file-io/close-file data)
  (let ((r (osdep/close-file (file-io/fd data))))
    (if (< r 0) 
        'error
        'ok)))

(define (file-io/file-modification-time filename)
  (osdep/file-modification-time filename))

(define (file-io/file-exists? filename)
  (file-exists? filename))