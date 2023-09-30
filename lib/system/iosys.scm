; The port type is a fixnum that encodes the binary/textual
; and input/output distinctions.  It is the inclusive or of
;
;     binary/textual:
;         0 means binary
;         1 means textual
;     direction:
;         0 means closed
;         2 means input
;         4 means output
;         6 means input/output

(define type-mask:binary/textual  1)
(define type-mask:direction       6)

(define type:binary               0)
(define type:textual              1)
(define type:input                2)
(define type:output               4)

(define type:closed               0)
(define type:binary-input         2)
(define type:textual-input        3)
(define type:binary-output        4)
(define type:textual-output       5)
(define type:binary-input/output  6)
(define type:textual-input/output 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Fields and offsets of a port structure.
; The most frequently accessed fields are at the beginning
; of the port structure, in hope of improving cache performance.
;
; NOTE that you can *not* change the offsets without also changing
; them in Compiler/common.imp.sch, where they are likely to be
; inlined.  The offsets may also be used in the following files:
;
;     bytevectorio.sch
;     stringio.sch
;     transio.sch
;
; They should not be used in any other files.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port.type       1) ; fixnum: see above for encoding
(define port.mainbuf    2) ; bytevector: Latin-1 or UTF-8 encodings
(define port.mainptr    3) ; nonnegative fixnum: next loc in mainbuf (input)
(define port.mainlim    4) ; nonnegative fixnum: sentinel in mainbuf (input)

; For input ports, port.mainpos holds the current byte
; or character position minus the current value of the
; port.mainptr field.
;
; For output ports, port.mainpos holds the current byte
; or character position minus the current value of the
; port.mainlim field.

(define port.mainpos    5) ; integer: byte/char position - (mainptr or mainlim)
(define port.transcoder 6) ; fixnum: see comment at make-transcoder

; The state is always one of the following symbols:
;     closed error eof
;     binary
;     textual auxstart auxend
;     input/output

(define port.state      7) ; symbol: see above
(define port.iodata     8) ; port-specific data
(define port.ioproc     9) ; port*symbol -> varies-with-symbol

; output ports

(define port.bufmode    10) ; symbol: none, line, datum, block
(define port.wr-flush? 11) ; boolean: true iff bufmode is datum

; textual input ports

(define port.auxbuf    12) ; bytevector: 4 bytes before or after mainbuf
(define port.auxptr    13) ; fixnum: index of next byte in auxbuf
(define port.auxlim    14) ; fixnum: 1 + index of last byte in auxbuf
(define port.linesread 15) ; integer: number of line endings read so far
(define port.linestart 16) ; integer: character position after last line ending
(define port.wasreturn 17) ; boolean: last line ending was #\return
(define port.readmode  18) ; fixnum: see comment before default-read-mode

; all ports

(define port.setposn   19) ; boolean: true iff supports set-port-position!
(define port.alist     20) ; association list: used mainly by custom ports
(define port.r7rstype  21) ; copy of port.type but unaltered by closing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous constants.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Default length of i/o buffer.

(define port.mainbuf-size    1024)

; Textual input uses 255 as a sentinel byte to force
; inline code to call a procedure for the general case.
; Note that 255 is not a legal code unit of UTF-8.

(define port.sentinel 255)

(define port.structure-size 22)

(define (io/make-port ioproc iodata . rest)
    (let (
        [v (make-tuple port.structure-size)]
        [input? #f]
        [output? #f]
        [binary? #f]
        [textual? #f]
        [set-position? #f])
        (tuple-set! v port.bufmode 'block)
        
        (for-each 
            (lambda (keyword)
                (case keyword
                    [(input) (set! input? #t)]
                    [(output) (set! output? #t)]
                    [(binary) (set! binary? #t)]
                    [(text) (set! textual? #t)]
                    [(set-position!) (set! set-position? #t)]
                    [(none) (tuple-set! v port.bufmode 'none)]
                    [(line) (tuple-set! v port.bufmode 'line)]
                    [(datum flush) (tuple-set! v port.bufmode 'datum) (tuple-set! v port.wr-flush #t)]
                    [(block) (tuple-set! v port.bufmode 'block)]
                    [else 
                        (assertion-violation 'io/make-port "bad attribute" (car rest))]))
            rest)
        (if (and binary? textual?)
            (assertion-violation 'io/make-port "binary and textual attributes are incompatible" rest))

        (tuple-set! v
                    port.type
                    (cond 
                        [(and binary? input? output?) type:binary-input/output]
                        [(and binary? input?) type:binary-input]
                        [(and binary? output?) type:binary-output]
                        [(and textual? input? output?) type:textual-input/output]
                        [(and textual? input?) type:textual-input]
                        [(and textual? output?) type:textual-output]
                        [input? type:textual-input]
                        [output? type:textual-output]
                        [else 
                            (error 'io/make-port "neither input nor output port" rest)]))
        (tuple-set! v port.mainbuf (make-bytevector port.mainbuf-size))
        (tuple-set! v port.mainptr 0)
        (tuple-set! v port.mainlim 0)
        (tuple-set! v port.mainpos 0)
        (tuple-set! v port.transcoder (if binary? 0 (native-transcoder)))
        (tuple-set! v port.state (if binary? 'binary 'textual))
        (tuple-set! v port.iodata iodata)
        (tuple-set! v port.ioproc ioproc)
        (tuple-set! v port.auxbuf (make-bytevector 4))
        (tuple-set! v port.auxlim 0)
        (tuple-set! v port.linesread 0)
        (tuple-set! v port.linestart 0)
        (tuple-set! v port.wasreturn #f)
        (tuple-set! v port.readmode (if (not binary?) readmode:r7rs readmode:binary))
        (tuple-set! v port.setposn set-position?)
        (tuple-set! v port.alist '())
        (tuple-set! v port.r7rstype (tuple-ref v port.type))
        (tuple-set! v 0 'type:port)

        v))

(define readmode-mask:foldcase        1)
(define readmode-mask:locations       2)
(define readmode-mask:javadot         4)
(define readmode-mask:flags           8)
(define readmode-mask:weirdness    1008)    ; (+ 16 32 64 128 256 512)

(define readmode:binary               0)
(define readmode:nofoldcase           0)
(define readmode:foldcase             1)
(define readmode:nolocations          0)
(define readmode:locations            2)
(define readmode:nojavadot            0)
(define readmode:javadot              4)
(define readmode:noflags              0)
(define readmode:flags                8)
(define readmode:noweird              0)
(define readmode:larceny             16)
(define readmode:traditional         32)
(define readmode:mzscheme            64)
(define readmode:r5rs               128)
(define readmode:r6rs               256)
(define readmode:r7rs               512)

(define (port? t)
    (and (tuple? t)
         (eq? (tuple-ref t 0) 'type:port)))

(define (io/input-port? p)
    (and (port? p)
        (let ([direction (fxlogand type-mask:direction (tuple-ref p port.type))])
            (= type:input (fxlogand direction type:input)))))

(define (io/output-port? p)
    (and (port? p)
        (let ([direction (fxlogand type-mask:direction (tuple-ref p port.type))])
            (= type:output (fxlogand direction type:output)))))

(define (io/open-port? p)
    (or (io/input-port? p)
        (io/output-port? p)))

(define (io/buffer-mode p)
    (tuple-ref p port.bufmode))


(define (io/read-char p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (buf  (tuple-ref p port.mainbuf))
            (ptr  (tuple-ref p port.mainptr)))
        (cond ((eq? type type:textual-input)
               (let ((unit (bytevector-u8-ref buf ptr)))
                 (if (< unit 128)
                     (begin (tuple-set! p port.mainptr (+ ptr 1))
                            (integer->char unit))
                     (io/get-char p #f))))
              ((eq? type type:binary-input)                 ; FIXME (was io/if)
               (let ((x (io/get-u8 p #f)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error 'read-char "not a textual input port" p)
               #t)))
      (begin (error 'read-char "not a textual input port" p)
             #t)))

; FIXME:  See comments for io/read-char above.

(define (io/peek-char p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (buf  (tuple-ref p port.mainbuf))
            (ptr  (tuple-ref p port.mainptr)))
        (cond ((eq? type type:textual-input)
               (let ((unit (bytevector-u8-ref buf ptr)))
                 (if (< unit 128)
                     (integer->char unit)
                     (io/get-char p #t))))
              ((eq? type type:binary-input)                 ; FIXME (was io/if)
               (let ((x (io/get-u8 p #t)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error 'peek-char "not a textual input port" p)
               #t)))
      (begin (error 'peek-char "not a textual input port" p)
             #t)))

; FIXME: trusts the ioproc, which might be unwise.

(define (io/u8-ready? p)
    (if (port? p)
        (let (
            [type (tuple-ref p port.type)]
            [ptr (tuple-ref p port.mainptr)]
            [lim (tuple-ref p port.mainlim)])
            (cond 
                [(eq? type type:binary-input)
                    (or (< ptr lim)
                        (eq? (tuple-ref p port.state) 'eof)
                        (((tuple-ref p port.ioproc) 'ready?) (tuple-ref p port.iodata)))]
                [else #f]))
        (error 'u8-ready? "not binary input" p)))

; This was dropped in R6RS because its semantics as specified
; by the R5RS aren't really useful.  See below.
;
; FIXME: reliable only when an Ascii character is ready on a
; textual port, which is a restriction permitted by the R5RS.
; The problem here is that a non-Ascii character might have
; been read in part, but attempting to read the full character
; might hang.  A more complex implementation is needed.
;
; FIXME: trusts the ioproc, which might be unwise.
(define (io/char-ready? p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (buf  (tuple-ref p port.mainbuf))
            (ptr  (tuple-ref p port.mainptr)))
        (cond ((eq? type type:textual-input)
               (let ((unit (bytevector-u8-ref buf ptr)))
                 (or (< unit 128)
                     (eq? (tuple-ref p port.state)
                          'eof)
                     (((tuple-ref p port.ioproc) 'ready?)
                      (tuple-ref p port.iodata)))))
              (else #f)))
      (error 'char-ready? "not textual input" p)))



(define (io/write-char c p)
  (if (port? p)
      (let ((type (tuple-ref p port.type)))
        (cond ((eq? type type:binary-output)                ; FIXME (was io/if)
               (let ((sv (char->integer c)))
                 (if (fx< sv 256)
                     (io/put-u8 p sv)
                     (error 'write-char
                            "non-latin-1 character to binary port"
                            c p))))
              ((eq? type type:textual-output)
               (io/put-char p c))
              (else
               (error 'write-char "not a textual output port" p)
               #t)))
      (begin (error 'write-char "not a textual output port" p)
             #t)))

; FIXME:  The name is misleading, since it now requires a bytevector.
; FIXME:  Asm/Shared/makefasl.sch uses this procedure, mainly
; to write codevectors.
;
; For short bytevectors, it might be more effective to copy rather than
; flush.  This procedure is really most useful for long bytevectors, and
; was written to speed up fasl file writing.
;
; With the advent of transcoded i/o in v0.94, this procedure
; is useful only for writing fasl files, which are either
; binary or Latin-1 with no end-of-line translation.

(define (io/write-bytetuple bvl p)
  (let ((buf (tuple-ref p port.mainbuf)))
    (io/flush-buffer p)
    (tuple-set! p port.mainbuf bvl)
    (tuple-set! p port.mainlim (bytetuple-length bvl))
    (io/flush-buffer p)
    (tuple-set! p port.mainbuf buf)
    (tuple-set! p port.mainlim 0)
    (undefined)))

; When writing the contents of an entire string,
; we could do the error checking just once, but
; this should be fast enough for now.

(define (io/write-string s p)
  (do ((n (string-length s))
       (i 0 (+ i 1)))
      ((= i n) (undefined))
    (io/write-char (string-ref s i) p)))

(define (io/discretionary-flush p)
  (if (and (port? p) (io/output-port? p))
      (if (tuple-ref p port.wr-flush?)
          (io/flush-buffer p))
      (begin (error "io/discretionary-flush: not an output port: " p)
             #t)))

; Flushes output-only ports, but does not flush combined input/output
; ports because they are unbuffered on the output side.

(define (io/flush p)
  (if (and (port? p) (io/output-port? p))
      (if (not (io/input-port? p))
          (io/flush-buffer p))
      (begin (error "io/flush: not an output port: " p)
             #t)))


(define (io/close-port p)
  (if (port? p)
      (begin
        (if (io/output-port? p)
            (io/flush-buffer p))
        (((tuple-ref p port.ioproc) 'close)
         (tuple-ref p port.iodata))
        (io/set-closed-state! p))
      (begin (error 'io/close-port "not a port" p)
             #t)))

(define (io/port-name p)
  (((tuple-ref p port.ioproc) 'name) (tuple-ref p port.iodata)))

(define (io/port-error-condition? p)
  (and (port? p) (eq? (tuple-ref p port.state) 'error)))

(define (io/port-at-eof? p)
  (and (port? p) (eq? (tuple-ref p port.state) 'eof)))


; The port alist was added to implement R6RS semantics for
; port-position and set-port-position!, and may eventually
; be used for other things also.

(define (io/port-alist p)
  (cond ((port? p)
         (tuple-ref p port.alist))
        (else
         (error 'io/port-alist "illegal" p)
         #t)))

(define (io/port-alist-set! p alist)
  (cond ((not (port? p))
         (error 'io/port-alist "illegal" p)
        ((not (and (list? alist)
                   (every? pair? alist)))
         (error 'io/port-alist "illegal" p))
        (else
         (tuple-set! p port.alist alist)))))

(define (io/fill-buffer! p)
    (let ([r (((tuple-ref p port.ioproc) 'read)
            (tuple-ref p port.iodata)
            (tuple-ref p port.mainbuf))])
        (cond 
            [(eq? r 'eof)
                (io/set-eof-state! p)]
            [(eq? r 'error)
                (io/set-error-state! p)
                (error 'io/fill-buffer! "error reading from port" p)]
            [(and (fixnum? r) (>= r 0))
                (tuple-set! p port.mainpos
                    (+ (tuple-ref p port.mainpos)
                       (tuple-ref p port.mainptr)))]
            [else 
                (io/set-error-state! p)
                (error 'io/fill-buffer! "bad return value from read" r)]))
    (io/transcode-port! p))

; The main buffer has just been filled, but the state has not been changed.
; If the port was in the textual state, it should enter the auxstart state.
; If the port was in the auxstart state, it should remain in that state.
; If the port was in the auxend state, it should enter the auxstart state.
; So the main job here is to convert the port to the auxstart state.

(define (io/transcode-port! p)
    (let* (
        [type (tuple-ref p port.type)]
        [state (tuple-ref p port.state)]
        [mainbuf (tuple-ref p port.mainbuf)]
        [mainptr (tuple-ref p port.mainptr)]
        [mainlim (tuple-ref p port.mainlim)]
        [auxbuf (tuple-ref p port.auxbuf)]
        [auxptr (tuple-ref p port.auxptr)]
        [auxlim (tuple-ref p port.auxlim)])
        (cond 
            [(= type type:binary-input) #t]
            [(= type type:textual-input)
                (case state 
                    [(closed error eof)
                        (io/reset-buffer! p)
                    ])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS i/o
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; R6RS port positions are a mess, mainly because they mimic
; a Posix feature that works only for file descriptors and
; whose semantics is specified in terms of bytes, not characters.
; As specified in Scheme, port positions also interact (badly)
; with other misfeatures, including input/output ports (whose
; semantics were modelled on Posix features that assume one
; byte per character) and custom ports (whose semantics mandates
; at least one character of lookahead buffering, but does not
; include any provision for buffer corrections when calculating
; port positions).
;
; In Capy, all ports support the port-position operation.
; Binary ports report the position in bytes, and textual ports
; report the position in characters.
;
; For set-port-position!, the situation is more complex:
;
; some ports do not support set-port-position!
;     e.g. pipes and sockets
; binary file ports support set-port-position!
; custom ports may support set-port-position!
;     If so, then Capy assumes their positions are reported
;     in bytes (for binary ports) or characters (for textual
;     ports) and relies on those assumptions to implement
;     buffer corrections.
; custom input/output (combined) ports must support set-port-position!
;     Capy relies on set-port-position! to implement any buffer
;     correction required when switching from input to output.
; textual input file ports support set-port-position!
;     With Latin-1 or UTF-8 transcoding, the implementation
;     uses a cache and a port-position-in-bytes procedure.
;     If the position has not been cached, then a warning
;     is issued and the implementation proceeds as for UTF-16
;     transcoding.
;     With UTF-16 transcoding, set-port-position! is implemented
;     by resetting to the beginning of the file and reading the
;     specified number of characters.
; textual input/output file ports support set-port-position!
;     Implementation is the same as for input-only file ports.
; textual output-only files ports support set-port-position!
;     Buffer corrections are implemented using caching and
;     a port-position-in-bytes procedure that is part of the
;     port structure.
; bytevector and string ports support set-port-position!
;     They mimic file ports.
;
; For textual ports, if the argument to set-port-position! is
; nonzero and is also not the result of a previous call to
; port-position on the port, then an exception should be raised.
;
; A position cache that maps character positions to byte positions
; is maintained for textual ports that support set-port-position!.
; If the port uses variable-length transcoding (with a codec other
; than Latin-1, or an eol-style other than none), then the port
; must supply one of the following procedures in the port's alist:
;
;     port-position-in-bytes
;     port-position-in-chars (used by custom ports)

(define (io/port-position p)
  (io/port-position-nocache p)  )

; Like io/port-position, but faster and more space-efficient
; because it doesn't cache.  The call to io/flush is necessary
; for correct caching when called by io/port-position above.

(define (io/port-position-nocache p)
  (cond ((io/input-port? p)
         (+ (tuple-ref p port.mainpos)
            (tuple-ref p port.mainptr)))
        ((io/output-port? p)
         (io/flush p)
         (+ (tuple-ref p port.mainpos)
            (tuple-ref p port.mainlim)))
        (else
         (error 'io/port-position "not an open port" p)
         #t)))


(define (io/port-lines-read p)
  (cond ((io/input-port? p)
         (tuple-ref p port.linesread))
        (else
         (error 'io/port-lines-read "not a textual input port" p)
         #t)))

(define (io/port-line-start p)
  (cond ((io/input-port? p)
         (tuple-ref p port.linestart))
        (else
         (error 'io/port-line-start "not a textual input port" p)
         #t)))

(define (io/port-has-set-port-position!? p)
  (cond ((port? p)
         (tuple-ref p port.setposn))
        (else
         (error 'io/has-set-port-position!? "illegal argument" p)
         #t)))

; FIXME: for textual output ports with variable-length encoding,
; any output operation should invalidate all cached positions
; that lie beyond the position of the output operation.

(define (io/set-port-position! p posn)
  (if (io/output-port? p)
      (io/flush p))
  (cond ((not (and (port? p)
                   (tuple-ref p port.setposn)))
         (error 'io/set-port-position! (errmsg 'msg:illegalarg1) p posn))
        ((eq? (tuple-ref p port.state) 'closed)
         (undefined))
        ((not (and (exact? posn) (integer? posn)))
         (error 'io/set-port-position! "illegal argument" posn))
        ((or (= posn 0)
             (io/binary-port? p))
         (io/reset-buffers! p)
         (tuple-set! p port.mainpos posn)
         (io/set-port-position-as-binary! p posn))
        (else

         ; Lookup the corresponding byte position.

         (let* ((t (io/port-transcoder p))
                (codec (io/transcoder-codec t))
                (input? (io/input-port? p))
                (output? (io/output-port? p))
                (alist (io/port-alist p))
                (probe1 (assq 'port-position alist))
                (port-position-in-chars (if probe1 (cdr probe1) #f))
                (probe2 (assq 'port-position-in-bytes alist))
                (port-position-in-bytes (if probe2 (cdr probe2) #f))
                (probe3 (assq 'cached-positions alist))
                (ht (if probe3 (cdr probe3) #f))
                (byte-posn (and ht (hashtable-ref ht posn #f))))

           (define (reposition!)
             (io/reset-buffers! p)
             (tuple-set! p port.mainpos posn)
             (io/set-port-position-as-binary! p byte-posn))

           ; We can't enforce the R6RS restriction for combined
           ; input/output ports because it may be a lookahead correction.

           (cond ((or byte-posn
                      (and input? output?))

                  (if (and (not input?)
                           output?
                           (not (eq? codec 'latin-1))
                           (not port-position-in-chars))
                      (issue-warning-deprecated
                       'set-port-position!...on_Unicode_output_port))

                  (reposition!))

                 (else

                  ; error case: posn > 0 and not in cache

                  (if (not (issue-deprecated-warnings?))

                      (assertion-violation 'set-port-position!
                                           (errmsg 'msg:uncachedposition)
                                           p posn)

                      ; FIXME: ad hoc warning message

                      (let ((out (current-error-port)))
                        (display "Warning from set-port-position!: " out)
                        (newline out)
                        (display (errmsg 'msg:uncachedposition) out)
                        (display ": " out)
                        (write posn out)
                        (newline out)

                        ; Attempt the operation anyway.  Hey, it might work.

                        (cond ((or port-position-in-chars
                                   (and
                                    (eq? 'latin-1 codec)
                                    (eq? 'none (io/transcoder-eol-style t))))
                               (reposition!))
                              ((io/input-port? p)
                               (io/set-port-position! p 0)
                               (do ((posn posn (- posn 1)))
                                   ((= posn 0))
                                 (read-char p)))
                              (else
                               (reposition!))))))))))

  (undefined))

(define (io/set-port-position-as-binary! p posn)
  (let ((r (((tuple-ref p port.ioproc) 'set-position!)
            (tuple-ref p port.iodata)
            posn)))
    (cond ((eq? r 'ok)
           (if (eq? (tuple-ref p port.state) 'eof)
               (tuple-set!
                p
                port.state
                (if (binary-port? p) 'binary 'textual)))
           (undefined))
          (else
           (error 'set-port-position! "io error" p posn)))))


(define (io/port-transcoder p)
  (tuple-ref p port.transcoder))

(define (io/textual-port? p)
  (not (= 0 (fxlogand type-mask:binary/textual
                        (tuple-ref p port.type)))))

(define (io/r7rs-textual-port? p)
  (not (= 0 (fxlogand type-mask:binary/textual
                        (tuple-ref p port.r7rstype)))))

(define (io/binary-port? p)
  (= 0 (fxlogand type-mask:binary/textual (tuple-ref p port.type))))

(define (io/r7rs-binary-port? p)
  (= 0 (fxlogand type-mask:binary/textual
                   (tuple-ref p port.r7rstype))))

; Transcoders et cetera.
;
; Internally, transcoders are represented as fixnums of the form
; xxxyyyzz, where xxx encodes a codec, yyy encodes an eol style,
; and zz encodes an error handling mode.
;
; codec (3 bits):
;     000 means none (port is binary)
;     001 means Latin-1
;     010 means UTF-8
;     011 means UTF-16
;     111 means UTF-16 in little-endian mode (internal only)
; 
; eol style (3 bits):
;     000 means none
;     001 means lf
;     010 means nel
;     011 means ls
;     100 means cr
;     101 means crlf
;     110 means crnel
;
; error handling mode (2 bits):
;     00 means ignore
;     01 means replace
;     10 means raise
;
; FIXME:  The external world should see a more self-explanatory
; representation of transcoders, and the three accessors for
; transcoders should return the original symbols instead of
; their canonical equivalents.

(define transcoder-mask:codec    224)
(define transcoder-mask:eolstyle 28)
(define transcoder-mask:errmode  3)

(define codec:binary  0)
(define codec:latin-1 32)
(define codec:utf-8   64)
(define codec:utf-16  96)

(define eolstyle:none  0)
(define eolstyle:lf    4)
(define eolstyle:nel   8)
(define eolstyle:ls    12)
(define eolstyle:cr    16)
(define eolstyle:crlf  20)
(define eolstyle:crnel 24)

(define errmode:ignore  0)
(define errmode:replace 1)
(define errmode:raise   2)

(define default-transcoder 
    (make-parameter 
        "default-transcoder"
        codec:latin-1
        (lambda (t)
            (and (fixnum? t)    
                (<= codec:latin-1 t transcoder-mask:codec)))))

(define (io/make-transcoder codec eol-style handling-mode)
    (define (local-error msg irritant)
    (if (issue-deprecated-warnings?)
        (let ((out (current-error-port)))
          (display "Warning: " out)
          (display msg out)
          (display ": " out)
          (write irritant out)
          (newline out)
          (display "Using Larceny-specific interpretation." out)
          (newline out))))
  (let ((bits:codec (case codec
                     ((latin-1) codec:latin-1)
                     ((utf-8)   codec:utf-8)
                     ((utf-16)  codec:utf-16)
                     (else (local-error "nonstandard codec" codec)
                           codec:latin-1)))
        (bits:eol (case eol-style
                   ((none)  eolstyle:none)
                   ((lf)    eolstyle:lf)
                   ((nel)   eolstyle:nel)
                   ((ls)    eolstyle:ls)
                   ((cr)    eolstyle:cr)
                   ((crlf)  eolstyle:crlf)
                   ((crnel) eolstyle:crnel)
                   (else (local-error "nonstandard eol style" eol-style)
                         eolstyle:none)))
        (bits:ehm (case handling-mode
                   ((ignore)  errmode:ignore)
                   ((replace) errmode:replace)
                   ((raise)   errmode:raise)
                   (else (local-error "nonstandard error handling mode"
                                      handling-mode)
                         errmode:replace))))
    (+ bits:codec bits:eol bits:ehm)))


; Programmers should never see a transcoder with binary codec.

(define (io/transcoder-codec t)
  (let ((codec (fxlogand t transcoder-mask:codec)))

    (cond ((= codec codec:binary)  'binary)
          ((= codec codec:latin-1) 'latin-1)
          ((= codec codec:utf-8)   'utf-8)
          ((= codec codec:utf-16)  'utf-16)
          (else
           (assertion-violation 'transcoder-codec
                                "weird transcoder" t)))))


(define (io/transcoder-eol-style t)
  (let ((style (fxlogand t transcoder-mask:eolstyle)))
    (cond ((= style eolstyle:none)  'none)
          ((= style eolstyle:lf)    'lf)
          ((= style eolstyle:nel)   'nel)
          ((= style eolstyle:ls)    'ls)
          ((= style eolstyle:cr)    'cr)
          ((= style eolstyle:crlf)  'crlf)
          ((= style eolstyle:crnel) 'crnel)
          (else
           (assertion-violation 'transcoder-eol-style
                                "weird transcoder" t)))))

(define (io/transcoder-error-handling-mode t)
  (let ((mode (fxlogand t transcoder-mask:errmode)))
    (cond ((= mode errmode:ignore)  'ignore)
          ((= mode errmode:replace) 'replace)
          ((= mode errmode:raise)   'raise)
          (else
           (assertion-violation 'transcoder-error-handling-mode
                                "weird transcoder" t)))))

; Like transcoded-port, but performs less error checking.
(define (io/transcoded-port p t)
  (if (io/output-port? p)
    (io/flush p))
  (if (not (memq (transcoder-codec t) '(latin-1 utf-8)))
    (io/transcoded-port-random p t)
    
    
    ; shallow copy
    (let ([newport (io/clone-port p)])
      (tuple-set! newport port.type (fxlogior type:textual (tuple-ref p port.type)))
      (tuple-set! newport port.r7rstype (fxlogior type:textual (tuple-ref newport port.type)))
      (tuple-set! newport port.transcoder t)
      (tuple-set! newport port.state 'textual)
      (tuple-set! newport port.readmode (default-read-mode))
      ; io/transcode-port! expects a newly filled mainbuf,
      ; so we have to fake it here.
      ;
      ; FIXME: Is the above really true?
    
      (let* ((mainbuf1 (tuple-ref p port.mainbuf))
        (mainptr1 (tuple-ref p port.mainptr))
        (mainlim1 (tuple-ref p port.mainlim))
        (mainbuf2 (make-bytevector (bytevector-length mainbuf1)))
        (mainlim2 (fx- mainlim1 mainptr1)))

        (tuple-set! newport port.mainpos 0)
        (bytevector-copy! mainbuf1 mainptr1 mainbuf2 0 mainlim2)
        (tuple-set! newport port.mainbuf mainbuf2)
        (tuple-set! newport port.mainptr 0)
        (tuple-set! newport port.mainlim mainlim2))
      ; close original port, destroying original mainbuf
      (io/set-closed-state! p)
      (cond 
        [(and (io/input-port? newport) (io/output-port? newport))
          #t]
        [(io/input-port? newport)
          (io/transocde-port! newport)])
      newport)))

(define (io/custom-transcoded-port p)
  (let* ([t (make-transcoder (utf8-codec) 'none 'ignore)]
         [newport (io/transocded-port p t)])
      newport))

(define (io/get-u8 p lookahead?)
  (if (not (port? p))
    (assertion-violation 'io/get-u8 "not a port" p))
  (let (
    [type (tuple-ref p port.type)]
    [buf (tuple-ref p port.mainbuf)]
    [ptr (tuple-ref p port.mainptr)]
    [lim (tuple-ref p port.mainlim)])
    (cond 
      [(not (eq? type type:binary-input))
        (if (eq? type type:binary-input/output)
          (io/get-u8-input/output p lookahead?)
          (assertion-violation 'io/get-u8 "not an open binary input port" p))]
      [(< ptr lim)
        (let ([byte (bytevector-u8-ref buf ptr)])
          (if (not lookahead?)
            (tuple-set! p port.mainptr (+ ptr 1)))
          byte)]
      [(eq? (tuple-ref p port.state) 'eof)
        (eof-object)]
      [else 
        (io/fill-buffer! p)
        (io/get-u8 p lookahead?)])))

(define (io/get-char p lookahead?)
  (if (not (port? p))
    (assertion-violation 'io/get-char "not a port" p))
  
  (let (
    [type (tuple-ref p port.type)]
    [buf (tuple-ref p port.mainbuf)]
    [ptr (tuple-ref p port.mainptr)]
    [lim (tuple-ref p port.mainlim)])
    (cond 
      [(not (eq? type type:textual-input))
        (if (eq? type type:textual-input/output)
          (io/get-char-input/output p lookahead?)
          (assertion-violation 'io/get-char "not an open textual input port" p))]
      [(< ptr lim)
        (let ([unit (bytevector-u8-ref buf ptr)])
          (cond 
            [(<= unit 127)
              (cond 
                [(> unit 13)
                   ; not #\linefeed, #\return, #\nel, or #\x2028
                   (if (not lookahead?)
                      (tuple-set! p port.mainptr (+ ptr 1)))
                   (integer->char unit)]
                [(or (= unit 10) (= unit 13)) ; #\linefeed or #\return
                  (if (not lookahead?)
                    (tuple-set! p port.mainptr (+ ptr 1)))
                  (io/return-eol p lookahead? unit)]
                [else
                  (if (not lookahead?)
                    (tuple-set! p port.mainptr (+ ptr 1)))
                  (integer->char unit)])]
            [(and (= unit port.sentinel)
                  (= ptr 0)
                  (eq? (tuple-ref p port.state) 'auxstart))
                  (io/get-char-auxstart p lookahead?)]
            [else 
              (let ([state (tuple-ref p port.state)])
                (case state 
                  [(eof) (eof-object)]
                  [(error) (error 'get-char "permanent read error" p)]
                  [(textual auxend)
                    (let ([codec (fxlogand
                                    transcoder-mask:codec
                                    (tuple-ref p port.transcoder))])
                                    
                        (cond 
                          [(= codec codec:latin-1)
                            (if (not lookahead?)
                              (tuple-set! p port.mainptr (+ ptr 1)))
                            (if (= unit 133)
                              (io/return-eol p lookahead? unit)
                              (integer->char unit))]
                          [(= codec codec:utf-8)
                            (io/get-char-utf-8 p lookahead? unit buf ptr lim)]
                          [else (error 'io/get-char "unimplemented codec" codec p)]))]
                  [else (error 'io/get-char "internal error" state p)]))]))]
        [(eq? (tuple-ref p port.state) 'eof)
          (io/reset-buffers! p) ; FIXME: Probably redundant
          (eof-object)]
        [(eq? (tuple-ref p port.state) 'error)
          (io/reset-buffers! p)
          (error 'get-char "permanent read error" p)]
        [(eq? (tuple-ref p port.state) 'auxend)
          (let* (
            [auxbuf (tuple-ref p port.auxbuf)]
            [auxptr (tuple-ref p port.auxptr)]
            [auxlim (tuple-ref p port.auxlim)]
            [n      (- auxlim auxptr)]
            [mainbuf (tuple-ref p port.mainbuf)])
            
            (bytevector-copy! auxbuf auxptr mainbuf 0 n) 
            (bytevector-u8-set! mainbuf n port.sentinel)
            (tuple-set! p port.mainpos 
                          (+ (tuple-ref p port.mainpos) ptr))
            (tuple-set! p port.mainptr 0)
            (tuple-set! p port.mainlim n)
            (tuple-set! p port.auxptr 0)
            (tuple-set! p port.state 'textual)
            (io/get-char p lookahead?))]
        [else 
          (io/reset-buffers! p)
          (io/fill-buffer! p) 
          (io/get-char p lookahead?)])))

(define (io/put-u8 p byte)
  (let (
      [type (tuple-ref p port.type)]
      [buf  (tuple-ref p port.mainbuf)]
      [lim   (tuple-ref p port.mainlim)])
    (cond 
      [(eq? type type:binary-output)
        (cond 
          [(< lim (bytevector-length buf))
            (bytevector-u8-set! buf lim byte)
            (tuple-set! p port.mainlim (+ lim 1))]
          [else 
            (io/flush-buffer p)
            (io/put-u8 p byte)])]
      [(eq? type type:binary-input/output)
        (io/put-u8-input/output p byte)]
      [else (error 'put-u8 "not a binary output port")])))
(define (io/put-char p c)
  (if (port? p)
    (let (
        [type (tuple-ref p port.type)]
        [buf  (tuple-ref p port.mainbuf)]
        [lim   (tuple-ref p port.mainlim)])
      (cond 
        [(eq? type type:textual-output)
          (let ([sv (char->integer c)]
                [n (bytevector-length buf)])
            (cond 
              [(>= lim n) 
                (io/flush-buffer p)
                (io/put-char p c)]
              [(= sv 10)
                (io/put-eol p)]
              [(<= sv 127)
                (bytevector-u8-set! buf lim sv)
                (tuple-set! p port.mainlim (+ lim 1))]
              [(and (<= sv 255)
                    (= codec:latin-1
                        (fxlogand 
                          transcoder-mask:codec
                          (tuple-ref p port.transcoder))))
                (bytevector-u8-set! buf lim sv)
                (tuple-set! p port.mainlim (+ lim 1))]
              [(not (= codec:utf-8)
                      (fxlogand 
                        transcoder-mask:codec 
                        (tuple-ref p port.transcoder)))
                (let* (
                    [t (tuple-ref p port.transcoder)]
                    [mode (fxlogand transcoder-mask:errmode t)])
                  (cond 
                    [(= mode errmode:ignore)
                      (undefined)]
                    [(= mode errmode:replace)
                      (io/put-char p #\?)]
                    [(= mode errmode:raise)
                      (error 'put-char "encoding error" (list p c))]
                    [else 
                      (assertion-violation 'put-char "internal error" p c)]))]
              [(>= lim (- n 4))
                (io/flush-buffer p)
                (io/put-char p c)]
              [(<= sv 2047)
                (let (
                    [u0 (fxlogior 192 (fxrshl sv 6))]
                    [u1 (fxlogior 128 (fxlogand sv 63))]
                    [pos (tuple-ref p port.mainpos)])
                  (bytevector-u8-set! buf lim u0)
                  (bytevector-u8-set! buf (+ lim 1) u1)
                  (tuple-set! p port.mainpos (- pos 1)
                  (tuple-set! p port.mainlim (+ lim 2))))]
              [(<= 65535)
                (let (
                    [u0 (fxlogior 224 (fxrshl sv 12))]
                    [u1 (fxlogior 128 (fxlogand (fxrshl sv 6) 63))]
                    [u2 (fxlogior 128 (fxlogand sv 63))]
                    [pos (tuple-ref p port.mainpos)])
                  (bytevector-u8-set! buf lim u0)
                  (bytevector-u8-set! buf (+ lim 1) u1)
                  (bytevector-u8-set! buf (+ lim 2) u2)
                  (tuple-set! p port.mainpos (- pos 2)
                  (tuple-set! p port.mainlim (+ lim 3))))]
              [else 
                (let (
                    [u0 (fxlogior 240 (fxrshl sv 18))]
                    [u1 (fxlogior 128 (fxlogand (fxrshl sv 12) 63))]
                    [u2 (fxlogior 128 (fxlogand (fxrshl sv 6) 63))]
                    [u3 (fxlogior 128 (fxlogand sv 63))]
                    [pos (tuple-ref p port.mainpos)])
                  (bytevector-u8-set! buf lim u0)
                  (bytevector-u8-set! buf (+ lim 1) u1)
                  (bytevector-u8-set! buf (+ lim 2) u2)
                  (bytevector-u8-set! buf (+ lim 3) u3)
                  (tuple-set! p port.mainpos (- pos 3)
                  (tuple-set! p port.mainlim (+ lim 4))))]))]
          [(eq? type:textual-input/output)
            (io/put-char-input/output p c)]
          [else 
            (error 'put-char "not an output port" p)])
      ) (error 'put-char "not an output port" p)))

; Operations on input/output ports, which are peculiar.
; Currently, the only input/output ports are
;
;     bytevector input/output ports
;     custom input/output ports
;     transcoded input/output ports
;
; These operations are invoked only when p is known to be an
; input/output port of the correct type (binary/textual).


(define (io/get-u8-input/output p lookahead?)
  (let* ((state   (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainlim (tuple-ref p port.mainlim))
         (iodata  (tuple-ref p port.iodata))
         (ioproc  (tuple-ref p port.ioproc)))
    (cond ((eq? state 'eof)
           (eof-object))
          ((eq? state 'error)
           (error 'get-u8 "permanent read error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'get-u8 "read attempted on closed port " p))
          ((> mainlim 0)
           (let ((r (bytevector-u8-ref mainbuf 0)))
             (if (not lookahead?)
                 (let ((mainpos (tuple-ref p port.mainpos)))
                   (tuple-set! p port.mainpos (+ mainpos 1))
                   (tuple-set! p port.mainlim 0)))
             r))
          (else
           (let ((n ((ioproc 'read) iodata mainbuf)))
             (cond ((eq? n 1)
                    (let ((r (bytevector-u8-ref mainbuf 0)))
                      (if (not lookahead?)
                          (let ((mainpos (tuple-ref p port.mainpos)))
                            (tuple-set! p port.mainpos (+ mainpos 1))
                            (tuple-set! p port.mainlim 0))
                          (tuple-set! p port.mainlim 1))
                      r))
                   ((or (eq? n 0) (eq? n 'eof))
                    (io/set-eof-state! p)
                    (eof-object))
                   (else
                    (io/set-error-state! p)
                    (io/get-u8-input/output p lookahead?))))))))

(define (io/get-char-input/output p lookahead?)
  (let* ((state   (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainlim (tuple-ref p port.mainlim))
         (iodata  (tuple-ref p port.iodata))
         (ioproc  (tuple-ref p port.ioproc)))
    (cond ((eq? state 'eof)
           (eof-object))
          ((eq? state 'error)
           (error 'get-char "Read error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'get-char "Read attempted on closed port " p))
          ((> mainlim 0)
           (let* ((bv (make-bytevector mainlim))
                  (s  (begin (r6rs:bytevector-copy! mainbuf 0 bv 0 mainlim)
                             (utf8->string bv))))
             (if (not lookahead?)
                 (let ((mainpos (tuple-ref p port.mainpos)))
                   (tuple-set! p port.mainpos (+ mainpos 1))
                   (tuple-set! p port.mainlim 0)))
             (string-ref s 0)))
          (else
           (let ((n ((ioproc 'read) iodata mainbuf)))
             (cond ((and (fixnum? n) (> n 0))
                    (let* ((bv (make-bytevector n))
                           (s  (begin (r6rs:bytevector-copy! mainbuf 0 bv 0 n)
                                      (utf8->string bv))))
                      (if (not lookahead?)
                          (let ((mainpos (tuple-ref p port.mainpos)))
                            (tuple-set! p port.mainpos (+ mainpos 1))
                            (tuple-set! p port.mainlim 0))
                          (tuple-set! p port.mainlim n))
                      (string-ref s 0)))
                   ((or (eq? n 0) (eq? n 'eof))
                    (io/set-eof-state! p)
                    (eof-object))
                   (else
                    (io/set-error-state! p)
                    (io/get-char-input/output p lookahead?))))))))

(define (io/put-u8-input/output p byte)
  (let* ((state   (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainpos (tuple-ref p port.mainpos))
         (mainlim (tuple-ref p port.mainlim))
         (iodata  (tuple-ref p port.iodata))
         (ioproc  (tuple-ref p port.ioproc))
         (buf     mainbuf))
    (cond ((eq? state 'error)
           (error 'put-u8 "permanent write error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'put-u8 "write attempted on closed port " p))
          ((> mainlim 0)
           (if (tuple-ref p port.setposn)
               (begin (io/set-port-position! p mainpos)
                      (io/put-u8-input/output p byte))
               (begin (io/set-error-state! p)
                      (error 'put-u8
                             "input/output port without set-port-position!"
                             p))))
          (else
           (bytevector-u8-set! buf 0 byte)
           (let ((r ((ioproc 'write) iodata buf 1)))
             (cond ((eq? r 'ok)
                    (tuple-set! p port.mainpos (+ mainpos 1))
                    (undefined))
                   (else
                    (io/set-error-state! p)
                    (io/put-u8-input/output p byte))))))))

(define (io/put-char-input/output p c)
  (let* ((state   (tuple-ref p port.state))
         (mainpos (tuple-ref p port.mainpos))
         (mainlim (tuple-ref p port.mainlim))
         (iodata  (tuple-ref p port.iodata))
         (ioproc  (tuple-ref p port.ioproc))
         (buf     (string->utf8 (string c))))
    (cond ((eq? state 'error)
           (error 'put-char "permanent write error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'put-char "write attempted on closed port " p))
          ((> mainlim 0)

           ; Must correct for buffered lookahead character.

           (if (tuple-ref p port.setposn)
               (begin (io/set-port-position! p mainpos)
                      (io/put-char-input/output p c))
               (begin (io/set-error-state! p)
                      (error 'put-char
                             "input/output port without set-port-position!"
                             p))))
          (else
           (let* ((n0 (bytevector-length buf))
                  (r ((ioproc 'write) iodata buf n0)))
             (cond ((eq? r 'ok)
                    (tuple-set! p port.mainpos (+ mainpos 1))
                    (undefined))
                   (else
                    (io/set-error-state! p)
                    (io/put-char-input/output p c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bulk i/o.
;;;
;;; Most of these handle a common case by returning the same
;;; value as a corresponding R6RS library procedure, but may
;;; fail on complex or unusual cases by returning #f.
;;;
;;; These should be majorly bummed, else there's no point.
;;;
;;; FIXME: could add a few more, such as
;;;     get-bytevector-n!
;;;     get-string-n!
;;;     put-bytevector
;;;
;;; Note, however, that io/put-string-maybe didn't help as much
;;; as io/get-line-maybe, and probably wasn't worth the effort
;;; and code size.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Handles the common case in which the line is all-Ascii,
; terminated by a linefeed, and lies entirely within the buffer.

(define (io/get-line-maybe p)
  (and (port? p)
       (let ((type (tuple-ref p port.type))
             (buf  (tuple-ref p port.mainbuf))
             (ptr  (tuple-ref p port.mainptr)))
         (define (loop i)
           (let ((unit (bytevector-u8-ref buf i)))     ; FIXME: should be trusted
             (cond ((and (< 13 unit)       ; 13 = #\return
                         (< unit 128))
                    (loop (+ i 1)))
                   ((= 10 unit)            ; 10 = #\linefeed
                    (let* ((n (- i ptr))
                           (s (make-string n)))
                      (loop2 ptr i s 0)))
                   (else #f))))
         (define (loop2 j k s i)
           (cond ((< j k)
                  (string-set! s i (integer->char
                                             (bytevector-u8-ref buf j)))
                  (loop2 (+ j 1) k s (+ i 1)))
                 (else
                  (tuple-set! p port.mainptr (+ k 1))
                  s)))
         (and (eq? type type:textual-input)
              (not (tuple-ref p port.wasreturn))
              (loop ptr)))))
  
; Handles the common case in which the string is all-Ascii
; and can be buffered without flushing.

(define (io/put-string-maybe p s start count)


  (and (port? p)
       (string? s)
       (fixnum? start)
       (fixnum? count)
       (<= 0 start)
       (let ((k (+ start count))
             (n (string-length s))
             (type (tuple-ref p port.type))
             (buf  (tuple-ref p port.mainbuf))
             (lim  (tuple-ref p port.mainlim)))
         (define (loop i j)
           (cond ((< i k)
                  (let* ((c (string-ref s i))
                         (sv (char->integer c)))
                    (if (and (< 10 sv)    ; 10 = #\newline
                             (< sv 128))
                        (begin (bytevector-u8-set! buf j sv) ; FIXME
                               (loop (+ i 1) (+ j 1)))
                        #f)))
                 (else
                  (tuple-set! p port.mainlim j)
                  #t)))
        
         (and (< start n)
              (<= k n)
              (eq? type type:textual-output)
              (<= (+ lim count) (bytevector-length buf))
              (loop start lim)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private procedures (called only from code within this file)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Works only on input ports.
; The main invariants may not hold here.
; In particular, the port may be in the textual state
; but have a nonempty auxbuf.

(define (io/fill-buffer! p)
  (let ((r (((tuple-ref p port.ioproc) 'read)
            (tuple-ref p port.iodata)
            (tuple-ref p port.mainbuf))))
    (cond ((eq? r 'eof)
           (io/set-eof-state! p))
          ((eq? r 'error)
           ; FIXME: should retry before giving up
           (io/set-error-state! p)
           (error 'io/fill-buffer! "Read error on port " p)
           #t)
          ((and (fixnum? r) (>= r 0))
           (tuple-set! p port.mainpos
                             (+ (tuple-ref p port.mainpos)
                                (tuple-ref p port.mainptr)))
           (tuple-set! p port.mainptr 0)
           (tuple-set! p port.mainlim r))
          (else
           (io/set-error-state! p)
           (error 'io/fill-buffer! "bad value " r " on " p)))
    (io/transcode-port! p)))


; The main buffer has just been filled, but the state has not been changed.
; If the port was in the textual state, it should enter the auxstart state.
; If the port was in the auxstart state, it should remain in that state.
; If the port was in the auxend state, it should enter the auxstart state.
; So the main job here is to convert the port to the auxstart state.

(define (io/transcode-port! p)
  (let* ((type       (tuple-ref p port.type))
         (state      (tuple-ref p port.state))
         (mainbuf    (tuple-ref p port.mainbuf))
         (mainptr    (tuple-ref p port.mainptr))
         (mainlim    (tuple-ref p port.mainlim))
         (auxbuf     (tuple-ref p port.auxbuf))
         (auxptr     (tuple-ref p port.auxptr))
         (auxlim     (tuple-ref p port.auxlim)))
    
    (cond ((= type type:binary-input) #t)
          ((= type type:textual-input)
           (case state
            ((closed error eof)
             (io/reset-buffers! p))
            ((textual auxstart auxend)
             (bytevector-u8-set! auxbuf auxlim (bytevector-u8-ref mainbuf 0))
             (tuple-set! p port.auxlim (+ auxlim 1))
             (bytevector-u8-set! mainbuf 0 port.sentinel)
             (tuple-set! p port.state 'auxstart))
            (else
             (error 'io/transcode-port! "internal error" p))))
          (else
           (error 'io/transcode-port! "internal error" p)))))

; Works only on output ports.

(define (io/flush-buffer p)
  (let ((wr-ptr (tuple-ref p port.mainlim)))
    (if (> wr-ptr 0)
        (let ((r (((tuple-ref p port.ioproc) 'write)
                  (tuple-ref p port.iodata)
                  (tuple-ref p port.mainbuf)
                  wr-ptr)))
          (cond ((eq? r 'ok)
                 (tuple-set! p port.mainpos
                                   (+ (tuple-ref p port.mainpos) wr-ptr))
                 (tuple-set! p port.mainlim 0))
                ((eq? r 'error)
                 (io/set-error-state! p)
                 (error 'io/flush-buffer "Write error on port " p)
                 #t)
                (else
                 (io/set-error-state! p)
                 (error 'io/flush-buffer "bad value " r " on " p)
                 #t))))))


; Converts port to a clean error state.

(define (io/set-error-state! p)
  (tuple-set! p port.state 'error)
  (io/reset-buffers! p))

; Converts port to a clean eof state.

(define (io/set-eof-state! p)
  (tuple-set! p port.state 'eof)
  (io/reset-buffers! p))

; Converts port to a clean closed state.
; FIXME:  Should this reduce the size of mainbuf?

(define (io/set-closed-state! p)
  (tuple-set! p port.type  type:closed)
  (tuple-set! p port.state 'closed)
  (io/reset-buffers! p))

; Resets buffers to an empty state.

(define (io/reset-buffers! p)
  (tuple-set! p
                    port.mainpos
                    (+ (tuple-ref p port.mainpos)
                       (tuple-ref p port.mainptr)))
  (tuple-set! p port.mainptr 0)
  (tuple-set! p port.mainlim 0)
  (tuple-set! p port.auxptr 0)
  (tuple-set! p port.auxlim 0)
  (bytevector-u8-set! (tuple-ref p port.mainbuf) 0 port.sentinel)
  (case (tuple-ref p port.state)
   ((auxstart auxend)
    (tuple-set! p port.state 'textual))))

; Shallow-clones a port without closing it.

(define (io/clone-port p)
  (let* ((n (tuple-length p))
         (newport (make-tuple n)))
    (do ((i 0 (+ i 1)))
        ((= i n)
         (tuple-set! newport 0 'type:port)
         newport)
      (tuple-set! newport i (tuple-ref p i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End-of-line processing.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Whenever io/get-char is about to return one of the four
; end-of-line characters (#\linefeed, #\return, #\x85, #x2028),
; it should perform a tail call to this procedure instead, with
; the scalar value of the specific end-of-line character as the
; last argument.
;
; That call should occur *after* the port's state has been
; updated to consume the character (unless lookahead?
; is true), but before the linesread, linestart, and wasreturn
; fields have been updated.  Updating those fields is the
; responsibility of these procedures.

(define (io/return-eol p lookahead? sv)
  (case sv
   ((13)
    (io/return-cr p lookahead?))
   ((10 133 8232)
    (if (tuple-ref p port.wasreturn)
        (begin (tuple-set! p port.wasreturn #f)
               (io/return-char-following-cr p lookahead? sv))
        (let* ((pos (tuple-ref p port.mainpos))
               (ptr (tuple-ref p port.mainptr))
               (line (tuple-ref p port.linesread))
               (transcoder (tuple-ref p port.transcoder))
               (eolstyle (fxlogand transcoder transcoder-mask:eolstyle)))
          (cond ((or (= sv 10)
                     (not (= eolstyle eolstyle:none)))
                 (if (not lookahead?)
                     (begin (tuple-set! p port.linesread (+ line 1))
                            (tuple-set! p port.linestart (+ pos ptr))))
                 #\newline)
                (else
                 (integer->char sv))))))
   (else
    (assertion-violation 'io/return-eol "internal error" p lookahead? sv))))

; Whenever io/put-char is about to output a #\newline,
; it should perform a tail call to this procedure instead.

(define (io/put-eol p)
  (let* ((buf        (tuple-ref p port.mainbuf))
         (mainlim    (tuple-ref p port.mainlim))
         (transcoder (tuple-ref p port.transcoder))
         (eolstyle (fxlogand transcoder transcoder-mask:eolstyle)))

    (define (put-byte b)
      (bytevector-u8-set! buf mainlim b)
      (let* ((mainpos (tuple-ref p port.mainpos))
             (linesread (tuple-ref p port.linesread)))
        (tuple-set! p port.mainlim (+ mainlim 1))
        (tuple-set! p port.linesread (+ linesread 1))
        (tuple-set! p port.linestart (+ mainlim mainpos)))
      (undefined))

    (define (put-bytes2 b0 b1)
      (bytevector-u8-set! buf mainlim b0)
      (bytevector-u8-set! buf (+ mainlim 1) b1)
      (finish 2))
    (define (put-bytes3 b0 b1 b2)
      (bytevector-u8-set! buf mainlim b0)
      (bytevector-u8-set! buf (+ mainlim 1) b1)
      (bytevector-u8-set! buf (+ mainlim 2) b2)
      (finish 3))

    (define (finish count)
      (let* ((mainpos (tuple-ref p port.mainpos))
             (mainpos (+ mainpos (- count 1)))
             (linesread (tuple-ref p port.linesread)))
        (tuple-set! p port.mainlim (+ mainlim count))
        (tuple-set! p port.mainpos mainpos)
        (tuple-set! p port.linesread (+ linesread 1))
        (tuple-set! p port.linestart (+ mainlim mainpos)))
      (undefined))

    (cond ((< (- (bytevector-length buf) mainlim) 4)
           (io/flush-buffer p)
           (io/put-eol p))

          ((or (eq? eolstyle eolstyle:none)
               (eq? eolstyle eolstyle:lf))
           (put-byte 10))

          ((eq? eolstyle eolstyle:cr)
           (put-byte 13))

          ((eq? eolstyle eolstyle:crlf)
           (put-bytes2 13 10))

          ((eq? codec:latin-1 (fxlogand transcoder transcoder-mask:codec))
           (cond ((eq? eolstyle eolstyle:nel)
                  (put-byte 133))
                 ((eq? eolstyle eolstyle:crnel)
                  (put-bytes2 13 133))
                 (else
                  (assertion-violation 'put-char "internal error" p))))

          ((eq? codec:utf-8 (fxlogand transcoder transcoder-mask:codec))
           (cond ((eq? eolstyle eolstyle:nel)
                  (put-bytes2 194 133))
                 ((eq? eolstyle eolstyle:crnel)
                  (put-bytes3 13 194 133))
                 ((eq? eolstyle eolstyle:ls)
                  (put-bytes3 226 128 168))
                 (else
                  (assertion-violation 'put-char "internal error" p))))

          (else
           (assertion-violation 'put-char "internal error" p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; On-the-fly transcoding of UTF-8.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; On-the-fly transcoding of a non-Ascii UTF-8 character.
; The first argument is known to be a textual input port
; whose transcoder uses the UTF-8 codec.
; The second argument is true if this is a lookahead operation.
; The third argument is the first code unit of the character.
; The last three arguments are for the active buffer, either
; mainbuf (if the state is textual or auxend)
; or auxbuf (if the state is auxstart).
;
; If the active buffer does not contain enough bytes, then
; the port must be forced into the auxstart state, and bytes
; must be transferred from mainbuf to auxbuf until the auxbuf
; contains a complete character.

(define (io/get-char-utf-8 p lookahead? unit buf ptr lim)

  (define (decoding-error units)
    (for-each (lambda (x) (io/consume-byte! p)) units)
    (let* ((transcoder (tuple-ref p port.transcoder))
           (errmode (fxlogand transcoder-mask:errmode transcoder)))
      (cond ((= errmode errmode:replace)
             (integer->char 65533))
            ((= errmode errmode:ignore)
             (io/get-char p lookahead?))
            (else
             (let* ((line (+ 1 (port-lines-read p)))
                    (msg (string-append "utf-8 decoding error in line "
                                        (number->string line))))
               (raise-r6rs-exception (make-i/o-decoding-error p)
                                     'get-char
                                     msg
                                     units))))))
  ; Forces at least one more byte into the active buffer,
  ; and retries.

  (define (read-more-bytes)
    (let* ((state (tuple-ref p port.state))
           (mainbuf (tuple-ref p port.mainbuf))
           (mainptr (tuple-ref p port.mainptr))
           (mainlim (tuple-ref p port.mainlim))
           (auxbuf (tuple-ref p port.auxbuf))
           (auxptr (tuple-ref p port.auxptr))
           (auxlim (tuple-ref p port.auxlim))
           (m (- mainlim mainptr))
           (n (- auxlim auxptr)))
      (case state

       ((auxend)
        (bytevector-copy! mainbuf mainptr mainbuf 0 m)
        (bytevector-copy! auxbuf auxptr mainbuf m n)
        (bytevector-u8-set! mainbuf (+ m n) port.sentinel)

        (tuple-set! p
                          port.mainpos
                          (+ (tuple-ref p port.mainpos) mainptr))
        (tuple-set! p port.mainptr 0)
        (tuple-set! p port.mainlim (+ m n))
        (tuple-set! p port.auxptr 0)
        (tuple-set! p port.auxlim 0)
        (tuple-set! p port.state 'textual)
        (io/get-char p lookahead?))

       ((textual)
        (bytevector-copy! mainbuf mainptr auxbuf 0 m)
        (tuple-set! p
                          port.mainpos
                          (+ (tuple-ref p port.mainpos) mainptr))
        (tuple-set! p port.mainptr 0)
        (tuple-set! p port.mainlim 0)
        (tuple-set! p port.auxptr 0)
        (tuple-set! p port.auxlim m)
        (io/fill-buffer! p)
        (io/get-char p lookahead?))

       ((auxstart)
        (if (>= m 2)
            (begin

             ; Copy one byte from mainbuf to auxbuf,
             ; and move mainbuf down by 1.
             ; FIXME:  This is grossly inefficient, but works for now.

             (bytevector-u8-set! auxbuf auxlim (bytevector-u8-ref mainbuf 1))
             (bytevector-copy! mainbuf 2 mainbuf 1 (- m 2))
             (tuple-set! p port.mainlim (- mainlim 1))
             (tuple-set! p port.auxlim (+ auxlim 1))
             (io/get-char p lookahead?))

            (begin
             (io/fill-buffer! p)
             (io/get-char p lookahead?))))

       (else
        ; state is closed, error, eof, or binary
        (error 'io/get-char-utf-8 "internal error" state)))))
  
  (define (finish k sv)
    (if (not lookahead?)
        (let ((mainbuf (tuple-ref p port.mainbuf))
              (mainptr (tuple-ref p port.mainptr))
              (mainpos (tuple-ref p port.mainpos)))
          (if (eq? mainbuf buf)
              (begin (tuple-set! p port.mainpos (+ mainpos (- 1 k)))
                     (tuple-set! p port.mainptr (+ k mainptr)))
              (begin (tuple-set! p port.mainpos (+ mainpos 1))
                     (io/consume-byte-from-auxbuf! p)
                     (io/consume-byte-from-auxbuf! p)
                     (if (> k 2) (io/consume-byte-from-auxbuf! p))
                     (if (> k 3) (io/consume-byte-from-auxbuf! p))))))
    (case sv
     ((133 8232)
      (io/return-eol p lookahead? sv))
     (else
      (integer->char sv))))
  
  (define (decode2) ; decodes 2 bytes
    (let ((unit2 (bytevector-u8-ref buf (+ ptr 1))))
      (if (<= 128 unit2 191)
          (finish 2
                  (fxlogior
                   (fxlsh (fxlogand 31 unit) 6)
                   (fxlogand 63 (bytevector-u8-ref buf (+ ptr 1)))))
          (decoding-error (list unit unit2)))))

  (define (decode3) ; decodes 3 bytes
    (let ((unit2 (bytevector-u8-ref buf (+ ptr 1)))
          (unit3 (bytevector-u8-ref buf (+ ptr 2))))
      (cond ((or (and (fx= unit 224)
                      (fx< unit2 160))
                 (not (<= 128 unit2 191)))
             (decoding-error (list unit unit2)))
            ((not (<= 128 unit3 191))
             (decoding-error (list unit unit2 unit3)))
            (else
             (finish 3
                     (fxlogior
                      (fxlsh (fxlogand 15 unit) 12)
                      (fxlogior
                       (fxlsh (fxlogand 63 unit2) 6)
                       (fxlogand 63 unit3))))))))
  
  (define (decode4) ; decodes 4 bytes
    (let ((unit2 (bytevector-u8-ref buf (+ ptr 1)))
          (unit3 (bytevector-u8-ref buf (+ ptr 2)))
          (unit4 (bytevector-u8-ref buf (+ ptr 3))))
      (cond ((or (and (fx= unit 240)
                      (fx< unit2 144))
                 (and (fx= unit 244)
                      (fx> unit2 143))
                 (not (<= 128 unit2 191)))
             (decoding-error (list unit unit2)))
            ((not (<= 128 unit3 191))
             (decoding-error (list unit unit2 unit3)))
            ((not (<= 128 unit4 191))
             (decoding-error (list unit unit2 unit3 unit4)))
            (else
             (finish 4
                     (fxlogior
                      (fxlogior
                       (fxlsh (fxlogand 7 unit) 18)
                       (fxlsh (fxlogand 63 unit2) 12))
                      (fxlogior
                       (fxlsh (fxlogand 63 unit3) 6)
                       (fxlogand 63 unit4))))))))

  (define n (- lim ptr))

  (cond ((< n 2)
         (read-more-bytes))
        ((<= unit 193)
         (decoding-error (list unit)))
        ((<= unit 223)
         (decode2))
        ((< n 3)
         (read-more-bytes))
        ((<= unit 239)
         (decode3))
        ((< n 4)
         (read-more-bytes))
        ((<= unit 244)
         (decode4))
        (else
         (decoding-error (list unit)))))


; The special case of get-char and lookahead-char on a textual port
; that's in the auxstart state, where it reads from auxbuf instead
; of mainbuf.
; The port is known to be a textual input port in the auxstart state.

(define (io/get-char-auxstart p lookahead?)
  (let ((buf  (tuple-ref p port.auxbuf))
        (ptr  (tuple-ref p port.auxptr))
        (lim  (tuple-ref p port.auxlim)))

    (cond ((fx< ptr lim)
           (let ((unit (bytevector-u8-ref buf ptr)))
             (cond ((<= unit 127)
                    (cond ((fx> unit 13)
                           ; not #\linefeed, #\return, #\nel, or #\x2028
                           (if (not lookahead?)
                               (let ((pos  (tuple-ref p port.mainpos)))
                                 (tuple-set! p port.mainpos (+ pos 1))
                                 (io/consume-byte-from-auxbuf! p)))
                           (integer->char unit))
                          ((or (fx= unit 10)                       ; #\linefeed
                               (fx= unit 13))                        ; #\return
                           (let ((pos (tuple-ref p port.mainpos)))
                             (if (not lookahead?)
                                 (begin
                                  (tuple-set! p port.mainpos (+ pos 1))
                                  (io/consume-byte-from-auxbuf! p)))
                             (io/return-eol p lookahead? unit)))
                          (else
                           (if (not lookahead?)
                               (let ((pos  (tuple-ref p port.mainpos)))
                                 (tuple-set! p port.mainpos (+ pos 1))
                                 (io/consume-byte-from-auxbuf! p)))
                           (integer->char unit))))
                   ((let ((codec (fxlogand
                                  transcoder-mask:codec
                                  (tuple-ref p port.transcoder))))
                      (fx= codec codec:latin-1))
                    ; Latin-1
                    (if (not lookahead?)
                        (let ((pos  (tuple-ref p port.mainpos)))
                          (tuple-set! p port.mainpos (+ pos 1))
                          (io/consume-byte-from-auxbuf! p)))
                    (if (fx= unit 133)
                        (io/return-eol p lookahead? unit)
                        (integer->char unit)))
                   (else
                    (io/get-char-utf-8 p lookahead? unit buf ptr lim)))))
          (else
           ; In the auxstart state, auxbuf should always be nonempty.
           (error 'io/get-char-auxstart "internal error" p)
           (eof-object)))))


; Given an input port in auxstart state, consumes a byte from its auxbuf.
; If that empties auxbuf, then the port enters a textual or auxend state.

(define (io/consume-byte-from-auxbuf! p)

  (define (leave-auxstart-state!)
    (let ((mainbuf (tuple-ref p port.mainbuf))
          (mainptr (tuple-ref p port.mainptr))
          (mainlim (tuple-ref p port.mainlim))
          (mainpos (tuple-ref p port.mainpos)))
      (assert (fx= 0 mainptr))
      (assert (fx< 0 mainlim))
      (tuple-set! p port.mainpos (- mainpos 1))
      (tuple-set! p port.mainptr 1)
      (if (fx< mainlim (bytevector-length mainbuf))
          (begin (bytevector-u8-set! mainbuf mainlim port.sentinel)
                 (tuple-set! p port.state 'textual))
          (begin (bytevector-u8-set! (tuple-ref p port.auxbuf)
                                  0
                                  (bytevector-u8-ref mainbuf (- mainlim 1)))
                 (bytevector-u8-set! mainbuf (- mainlim 1) port.sentinel)
                 (tuple-set! p port.mainlim (- mainlim 1))
                 (tuple-set! p port.auxptr 0)
                 (tuple-set! p port.auxlim 1)
                 (tuple-set! p port.state 'auxend)))))

  (assert (eq? 'auxstart (tuple-ref p port.state)))

  (let* ((ptr (tuple-ref p port.auxptr))
         (lim (tuple-ref p port.auxlim))
         (ptr+1 (+ ptr 1)))

    (cond ((fx= ptr+1 lim)
           (tuple-set! p port.auxptr 0)
           (tuple-set! p port.auxlim 0)
           (leave-auxstart-state!))
          (else
           (tuple-set! p port.auxptr ptr+1)))))

; Given an input textual port, consumes a byte from its buffers.
; This may cause a change of state.
; This procedure is called only during error handling
; and #\return handling, so it can be fairly slow.

(define (io/consume-byte! p)
  (let ((state (tuple-ref p port.state))
        (mainbuf (tuple-ref p port.mainbuf))
        (mainptr (tuple-ref p port.mainptr))
        (mainlim (tuple-ref p port.mainlim))
        (auxbuf (tuple-ref p port.auxbuf))
        (auxptr (tuple-ref p port.auxptr))
        (auxlim (tuple-ref p port.auxlim)))
    (case state
     ((auxstart)
      (io/consume-byte-from-auxbuf! p))
     ((textual auxend)
      (cond ((fx< mainptr mainlim)
             (tuple-set! p port.mainpos
                               (- (tuple-ref p port.mainpos) 1))
             (tuple-set! p port.mainptr (+ mainptr 1)))
            ((eq? state 'auxend)
             (assert (fx< auxptr auxlim))
             (r6rs:bytevector-copy! auxbuf auxptr mainbuf 0 (- auxlim auxptr))
             (bytevector-u8-set! mainbuf (- auxlim auxptr) port.sentinel)
             (tuple-set! p
                               port.mainpos
                               (+ (tuple-ref p port.mainpos) mainptr))
             (tuple-set! p port.mainptr 0)
             (tuple-set! p port.mainlim (- auxlim auxptr))
             (tuple-set! p port.auxptr 0)
             (tuple-set! p port.auxlim 0)
             (tuple-set! p port.state 'textual)
             (io/consume-byte! p))
            (else
             (io/reset-buffers! p))))
     (else
      ; must be closed, error, eof
      (undefined)))))

; eof
