(define *file-option-symbols*
  '(no-create no-fail no-truncate))


; Enumeration sets can't be created until late in the initialization
; process, so the creation of this enumeration set must be delayed.

(define *file-options-enumeration-set* #f)

(define (file-options-enumeration-set)
  (if (not *file-options-enumeration-set*)
      (set! *file-options-enumeration-set*
            (make-enumeration *file-option-symbols*)))
  *file-options-enumeration-set*)  

(define (make-file-options-set syms)
  ((enum-set-constructor (file-options-enumeration-set)) syms))

(define (file-options->list options)
  (enum-set->list options))

(define (file-options . symbols)
  (make-file-options-set
   (filter (lambda (sym) (memq sym *file-option-symbols*))
           symbols)))

(define (native-eol-style) 'none) 
(define (latin-1-codec) 'latin-1)
(define (utf-8-codec) 'utf-8)
(define (utf-16-codec) 'utf-16)


(define (make-transcoder codec . rest)
    (cond 
        [(null? rest) 
            (io/make-transocder codec (native-eol-style) 'replace)]
        [(null? (cdr rest))
            (io/make-transcoder codec (car rest) 'replace)]
        [(null? (cddr rest))
            (io/make-transcoder codec (car rest) (cadr rest))]
        [else 
            (assertion-violation 
                'make-transcoder 
                "too many arguments"
                (cons codec rest))]))
(define (native-transcoder)
  ;(make-transcoder (latin-1-codec) 'none 'ignore))
  (default-transcoder))

(define (transcoder-codec t)
  (io/transcoder-codec t))

(define (transcoder-eol-style t)
  (io/transcoder-eol-style t))

(define (transcoder-error-handling-mode t)
  (io/transcoder-error-handling-mode t))

(define (port-transcoder p)
  (let ((t (io/port-transcoder p)))
    (if (eq? (io/transcoder-codec t) 'binary)
        #f
        t)))

;;; The R6RS says textual-port? and binary-port? take a port as argument.
;;; The R7RS says they accept any argument.

(define (textual-port? p)
  (and (port? p)
       (io/r7rs-textual-port? p)))

(define (binary-port? p)
  (and (port? p)
       (io/r7rs-binary-port? p)))

(define (transcoded-port p t)
  (if (and (io/binary-port? p)
           (memq (transcoder-codec t) '(latin-1 utf-8 utf-16))
           (memq (transcoder-eol-style t) '(none lf cr crlf nel crnel ls))
           (memq (transcoder-error-handling-mode t) '(ignore replace raise))
           (if (and (io/input-port? p) (io/output-port? p))
               (and (eq? (transcoder-codec t) 'latin-1)
                    (eq? (transcoder-eol-style t) 'none))
               #t))
      (io/transcoded-port p t)
      (assertion-violation 'transcoded-port
                           (errmsg 'msg:illegalargs) p t)))

; All binary and textual ports support port-position internally
; but custom ports may claim not to.

(define (port-has-port-position? p)
  (let ((probe (assq 'port-position (io/port-alist p))))
    (cond ((not probe)
           (or (binary-port? p) (textual-port? p)))
          ((cdr probe) #t)
          (else #f))))

; FIXME:  Custom implementations of port-position are ignored.

(define (port-position-nocache p) (io/port-position-nocache p))

(define (port-position p) (io/port-position p))


(define (port-has-set-port-position!? p)
  (io/port-has-set-port-position!? p))

(define (set-port-position! p pos)
  (io/set-port-position! p pos))

(define (close-port p)
  (io/close-port p))


(define (call-with-port p f)
  (call-with-values
   (lambda () 
    (f p))
   (lambda results
     (if (io/open-port? p) (io/close-port p))
     (apply values results))))

(define (port-lines-read p) (io/port-lines-read p))
(define (port-line-start p) (io/port-line-start p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input ports.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (port-eof? p)
  (assert (io/input-port? p))
  (cond ((io/binary-port? p)
         (eof-object? (lookahead-u8 p)))
        ((io/textual-port? p)
         (eof-object? (lookahead-char p)))
        (else
         ; probably closed
         #f)))


(define (open-file-input-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-input-port filename (file-options) 'block #f))
        ((null? (cdr rest))
         (file-io/open-file-input-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (file-io/open-file-input-port filename (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (file-io/open-file-input-port filename
                                  (car rest) (cadr rest) (caddr rest)))
        (else
         (assertion-violation 'open-file-input-port
                              (errmsg 'msg:wna)
                              (cons filename rest)))))

(define (open-bytevector-input-port bv . rest)
  (let ((transcoder (if (null? rest) #f (car rest)))
        (port (bytevector-io/open-input-bytevector bv)))
    (if transcoder
        (transcoded-port port transcoder)
        port)))

(define (open-string-input-port s)
  (let ((transcoder (make-transcoder (utf-8-codec) 'none 'ignore))
        (port (bytevector-io/open-input-bytevector-no-copy (string->utf8 s))))
    (transcoded-port port transcoder)))
(define (standard-input-port)
  (let ((fd (osdep/open-console 'input)))
    (io/make-port console-io/ioproc
                  (file-io/data fd "*console-input*")
                  'input
                  'binary)))

(define (output-port-buffer-mode p)
  (if (output-port? p)
      (io/buffer-mode p)
      (assertion-violation 'output-port-buffer-mode
                           (errmsg 'msg:notoutput) p)))

(define (open-file-output-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-output-port filename (file-options) 'block #f))
        ((null? (cdr rest))
         (file-io/open-file-output-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (file-io/open-file-output-port filename (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (file-io/open-file-output-port filename
                                  (car rest) (cadr rest) (caddr rest)))
        (else
         (assertion-violation 'open-file-output-port
                              (errmsg 'msg:wna)
                              (cons filename rest)))))

(define (call-with-bytevector-output-port f . rest)
  (if (and (procedure? f)
           (or (null? rest)
               (null? (cdr rest))))
      (let ((transcoder (if (null? rest) #f (car rest))))
        (if transcoder
            (call-with-port
             (open-output-string)
             (lambda (out)
               (f out)
               (let ((s (get-output-string out)))
                 (reset-output-string out)
                 (string->bytevector s transcoder))))
            (call-with-port
             (open-output-bytevector)
             (lambda (out)
               (f out)
               (get-output-bytevector out)))))
      (assertion-violation 'call-with-bytevector-output-port
                           (errmsg 'msg:illegalargs) f)))


(define (call-with-string-output-port f)
  (if (procedure? f)
      (call-with-port
       (open-output-string)
       (lambda (out) (f out) (get-output-string out)))
      (assertion-violation 'call-with-string-output-port
                           (errmsg 'msg:illegalarg) f)))

(define (open-file-input/output-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-input/output-port
          filename (file-options) 'block #f))
        ((null? (cdr rest))
         (file-io/open-file-input/output-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (file-io/open-file-input/output-port filename
                                              (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (file-io/open-file-input/output-port filename
                                  (car rest) (cadr rest) (caddr rest)))
        (else
         (assertion-violation 'open-file-input/output-port
                              (errmsg 'msg:wna)
                              (cons filename rest)))))

(define (standard-output-port)
  (let ((fd (osdep/open-console 'output)))
    (io/make-port console-io/ioproc
                  (file-io/data fd "*console-output*")
                  'output
                  'flush
                  'binary)))

(define (standard-error-port)
  (let ((fd (osdep/open-console 'error)))
    (io/make-port console-io/ioproc
                  (file-io/data fd "*error-output*")
                  'output
                  'flush
                  'binary)))

(define (make-custom-binary-input-port
         id read! get-position set-position! close)
  (customio/make-binary-input-port
   id read! get-position set-position! close))

(define (make-custom-binary-output-port
         id write! get-position set-position! close)
  (customio/make-binary-output-port
   id write! get-position set-position! close))


(define (make-custom-binary-input/output-port
         id read! write! get-position set-position! close)
  (customio/make-binary-input/output-port
   id read! write! get-position set-position! close))

(define (make-custom-textual-input-port
         id read! get-position set-position! close)
  (customio/make-textual-input-port
   id read! get-position set-position! close))

(define (make-custom-textual-output-port
         id write! get-position set-position! close)
  (customio/make-textual-output-port
   id write! get-position set-position! close))

(define (make-custom-textual-input/output-port
         id read! write! get-position set-position! close)
  (customio/make-textual-input/output-port
   id read! write! get-position set-position! close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic input (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookahead-u8 p)   (io/get-u8 p #t))
(define (get-u8 p)         (io/get-u8 p #f))
(define (lookahead-char p) (io/get-char p #t))
(define (get-char p)       (io/get-char p #f))

(define (get-bytevector-n p count)
  (if (and (io/input-port? p)
           (io/binary-port? p)
           (fixnum? count)
           (<= 0 count))
      (let* ((bv (make-bytevector count))
             (n (get-bytevector-n! p bv 0 count)))
        (cond ((not (fixnum? n))
               n)
              ((= n count)
               bv)
              (else
               (let ((bv2 (make-bytevector n)))
                 (bytevector-copy! bv 0 bv2 0 n)
                 bv2))))
      (portio/illegal-arguments 'get-bytevector-n p count)))

(define (get-bytevector-n! p bv start count)
  (if (and (io/input-port? p)
           (io/binary-port? p)
           (bytevector? bv)
           (fixnum? start)
           (<= 0 start)
           (fixnum? count)
           (<= 0 count)
           (<= (+ start count) (bytevector-length bv)))
      (if (fx= 0 count)
          0
          (let loop ((i start)
                     (n (+ start count)))
            (cond ((= i n)
                   (- i start))
                  (else
                   (let ((byte (get-u8 p)))
                     (cond ((fixnum? byte)
                            (bytevector-u8-set! bv i byte)
                            (loop (+ i 1) n))
                           ((= i start)
                            (eof-object))
                           (else
                            (- i start))))))))
      (portio/illegal-arguments 'get-bytevector-n! p bv start count)))


; FIXME:  This is extremely inefficient.

(define (get-bytevector-some p)
  (issue-warning-deprecated 'get-bytevector-some)
  (if (and (io/input-port? p)
           (io/binary-port? p))
      (let ((byte (get-u8 p)))
        (if (eof-object? byte)
            byte
            (make-bytevector 1 byte)))
      (portio/illegal-arguments 'get-bytevector-some p)))

(define (get-bytevector-all p)
  (if (and (io/input-port? p)
           (io/binary-port? p))
      (let ((bv (call-with-port
                 (open-output-bytevector)
                 (lambda (out)
                   (do ((byte (get-u8 p) (get-u8 p)))
                       ((eof-object? byte)
                        (get-output-bytevector out))
                     (put-u8 out byte))))))
        (if (= 0 (bytevector-length bv))
            (eof-object)
            bv))
      (portio/illegal-arguments 'get-bytevector-all p)))


; FIXME:  The R6RS specifications for get-string-n and
; get-string-n! insist that (get-string-n p 0) returns
; an end-of-file object instead of the empty string,
; and (get-string-n! p s k 0) returns an end-of-file
; object instead of the empty string, when p is at the
; end of input.  I believe this is an error in the R6RS.

(define (get-string-n p count)
  (if (and (io/input-port? p)
           (io/textual-port? p)
           (fixnum? count)
           (<= 0 count))
      (let ((out (open-output-string)))
        (define (loop count)
          (cond ((<= count 0)
                 (get-output-string out))
                (else
                 (let ((c (get-char p)))
                   (cond ((eof-object? c)
                          (let ((s (get-output-string out)))
                            (if (= 0 (string-length s))
                                c
                                s)))
                         (else
                          (put-char out c)
                          (loop (- count 1))))))))
        (loop count))
      (portio/illegal-arguments 'get-string-n p count)))

(define (get-string-n! p s start count)
  (if (and (io/input-port? p)
           (io/textual-port? p)
           (string? s)
           (fixnum? start)
           (<= 0 start)
           (fixnum? count)
           (<= 0 count)
           (<= (+ start count) (string-length s)))
      (let ((n (+ start count)))
        (define (loop i)
          (cond ((= i n)
                 (- i start))
                (else
                 (let ((c (get-char p)))
                   (cond ((eof-object? c)
                          (if (= i start)
                              c
                              (- i start)))
                         (else
                          (string-set! s i c)
                          (loop (+ i 1))))))))
        (loop start))
      (portio/illegal-arguments 'get-string-n! p s start count)))

(define (get-string-all p)
  (if (and (io/input-port? p)
           (io/textual-port? p))
      (let ((s (call-with-string-output-port
                 (lambda (out)
                   (do ((char (get-char p) (get-char p)))
                       ((eof-object? char))
                     (put-char out char))))))
        (if (= 0 (string-length s))
            (eof-object)
            s))
      (portio/illegal-arguments 'get-string-all p)))

(define .linefeed. (integer->char 10))

(define (portio/get-line p)
  
  (if (and (io/input-port? p)
           (io/textual-port? p))
      (let* ((eof? #f)
             (s (call-with-string-output-port
                 (lambda (out)
                   (do ((char (get-char p) (get-char p)))
                       ((or (eof-object? char) (char=? char .linefeed.))
                        (if (eof-object? char) (set! eof? #t)))
                     (put-char out char))))))
        (if (and eof? (= 0 (string-length s)))
            (eof-object)
            s))
      (portio/illegal-arguments 'get-line p)))

(define (get-line p)
  (or (io/get-line-maybe p)
      (portio/get-line p)))

(define (portio/illegal-arguments who . irritants)
  (apply assertion-violation who (errmsg 'msg:illegalargs) irritants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic output (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-u8 p k)   (io/put-u8 p k))
(define (put-char p c) (io/put-char p c))


(define (put-bytevector p bv . rest)
  (define (put-bytevector p bv start count)
    
    (if (and (io/binary-port? p)
             (io/output-port? p)
             (bytevector? bv)
             (fixnum? start)
             (fixnum? count)
             (<= 0 start)
             (<= 0 count)
             (<= (+ start count) (bytevector-length bv)))
        (let ((n (+ start count)))
          (do ((i start (+ i 1)))
              ((= i n))
            (put-u8 p (bytevector-u8-ref bv i))))
        (begin 
          (assertion-violation 'put-bytevector
                             "illegal args" p bv start count))))
  (cond ((null? rest)
         (put-bytevector p bv 0 (bytevector-length bv)))
        ((null? (cdr rest))
         (let* ((start (car rest))
                (count (- (bytevector-length bv) start)))
           (put-bytevector p bv start count)))
        ((null? (cddr rest))
         (put-bytevector p bv (car rest) (cadr rest)))
        (else
         (assertion-violation 'put-bytevector
                              "too many args"
                              (cons p (cons bv rest))))))

(define (put-string p s . rest)

  (define (portio/put-string p s start count)
    (if (and (io/textual-port? p)
             (io/output-port? p)
             (string? s)
             (fixnum? start)
             (fixnum? count)
             (<= 0 start)
             (<= 0 count)
             (<= (+ start count) (string-length s)))
        (let ((n (+ start count)))
          (do ((i start (+ i 1)))
              ((= i n))
            (put-char p (string-ref s i))))
        (assertion-violation 'put-string
                             (errmsg 'msg:illegalargs) p s start count)))

  (define (put-string p s start count)
    (let ([res (or (io/put-string-maybe p s start count)
        (portio/put-string p s start count))])
        res))

  (cond ((null? rest)
         (put-string p s 0 (string-length s)))
        ((null? (cdr rest))
         (put-string p s (car rest) (- (string-length s) (car rest))))
        ((null? (cddr rest))
         (put-string p s (car rest) (cadr rest)))
        (else
         (assertion-violation 'put-string
                              (errmsg 'msg:toomanyargs)
                              (cons p (cons s rest))))))

(define (put-datum p x)
  (write x p))
