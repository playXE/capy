(define unix:stdin 0)
(define unix:stdout 1)
(define unix:stderr 2)

(define unix:open-read 1)
(define unix:open-write 2)
(define unix:open-append 4)
(define unix:open-create 8)
(define unix:open-trunc 16)
(define unix:open-binary 32)
(define unix:create-mode 438)


(define unix:access-exists 1)
(define unix:access-read 2)
(define unix:access-write 4)
(define unix:access-execute 8)

(define osdep/newline 10)

(define *conio-input-firsttime* #t)
(define *conio-output-firsttime* #t)
(define *conio-error-firsttime* #t)

(define (osdep/open-console io-mode)
    (case io-mode 
        [(input)
            (if *conio-input-firsttime*
                (begin 
                    (set! *conio-input-firsttime* #f)
                    unix:stdin)
                (unix-open "/dev/tty" unix:open-read 0))]
        [(output)
            (if *conio-output-firsttime*
                (begin 
                    (set! *conio-output-firsttime* #f)
                    unix:stdout)
                (unix-open "/dev/tty" unix:open-write 0))]
        [(error)
            (if *conio-error-firsttime*
                (begin 
                    (set! *conio-error-firsttime* #f)
                    unix:stderr)
                (unix-open "/dev/tty" unix:open-write 0))]
        [else 
            (error 'osdep/open-console "unknown io-mode: " io-mode)]))

; File system.
;
; A file name is a string.
; A file descriptor is a fixnum.
; A buffer is a bytevector.
;
; io-mode is a symbol ('input' or 'output').
; tx-mode is a symbol ('text' or 'binary').
;
; The optional arguments recognized by osdep/open-file are the symbols
;     'no-create'
;     'no-truncate'
; These optional arguments are ignored if io-mode is 'input'.


(define (osdep/close-file fd)
    (if (not (fixnum? fd))
        (error 'osdep/close-file "not a file descriptor: " fd))
    
    (unix-close fd))

(define (osdep/open-file fn io-mode tx-mode . optargs)
    (if (not (string? fn))
        (error 'osdep/open-file "not a file name: " fn))
    (let (
        [binary-mode (if (eq? tx-mode 'binary) unix:open-binary 0)]
        [create-mode (if (and (eq? io-mode 'output) (memq 'no-create optargs)) 0 unix:open-create)]
        [truncate-mode (if (and (eq? io-mode 'output) (memq 'no-truncate optargs)) 0 unix:open-trunc)])
        (cond 
            [(eq? io-mode 'input)
                (unix-open fn (+ unix:open-read binary-mode) 0)]
            [(eq? io-mode 'output)
                (unix-open fn (+ unix:open-write create-mode truncate-mode binary-mode) unix:create-mode)]
            [else 
                (error 'osdep/open-file "unknown io-mode: " io-mode)])))

(define (osdep/close-console fd)
    (if (not (fixnum? fd))
        (error 'osdep/close-console "not a file descriptor: " fd))
    
    (unix-close fd))

(define (osdep/read-file fd buffer nbytes)
    (if (not (fixnum? fd))
        (error 'osdep/read-file "not a file descriptor: " fd))
    (if (not (bytevector? buffer))
        (error 'osdep/read-file "not a buffer: " buffer))
    (if (not (and (fixnum? nbytes) (>= nbytes 0)))
        (error 'osdep/read-file "not a non-negative fixnum: " nbytes))
    
    (unix-read fd buffer nbytes))

(define (osdep/write-file fd buf k)
    (osdep/write-file4 fd buf k 0))

(define (osdep/write-file4 fd buf k offset)
    (if (not (fixnum? fd))
        (error 'osdep/write-file "not a file descriptor: " fd))
    (if (not (bytevector? buf))
        (error 'osdep/write-file "not a buffer: " buf))
    (if (not (fixnum? offset))
        (error 'osdep/write-file "not a fixnum: " offset))
    (if (not (and (>= offset 0)
                (<= (+ offset k) (bytevector-length buf))))
        (error 'osdep/write-file "invalid offset: " offset))
    (unix-write fd buf k offset))

(define (osdep/lseek-file fd offset whence)
    (if (not (fixnum? fd))
        (error 'osdep/lseek-file "not a file descriptor: " fd))
    (if (not (fixnum? offset))
        (error 'osdep/lseek-file "not a fixnum: " offset))
    (if (not (fixnum? whence))
        (error 'osdep/lseek-file "not a fixnum: " whence))
    (unix-lseek fd offset whence))

(define (osdep/file-modification-time fn)
    (if (not (string? fn))
        (error 'osdep/file-modification-time "not a file name: " fn))
    (let ([v (make-vector 6)])
        (if (= (mtime fn v) 0)
            v
            #f)))