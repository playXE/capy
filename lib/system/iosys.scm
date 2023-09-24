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
                    [(datum flush) (tuple-set! v port.bufmode 'datum) (tuple-set! v port.wr-flush #t)]\
                    [(block) (tuple-set! v port.bufmode 'block)]
                    [else 
                        (print rest)
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
        (tuple-set! v port.transcoder (if binary? 0 #f))
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
            (fx= type:input (fxlogand direction type:input)))))

(define (io/output-port? p)
    (and (port? p)
        (let ([direction (fxlogand type-mask:direction (tuple-ref p port.type))])
            (fx= type:output (fxlogand direction type:output)))))

(define (io/open-port? p)
    (or (io/input-port? p)
        (io/output-port? p)))

(define (io/buffer-mode p)
    (tuple-ref p port.bufmode))


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
               (let ((unit (bytevector-ref buf ptr)))
                 (or (< unit 128)
                     (eq? (tuple-ref p port.state)
                          'eof)
                     (((tuple-ref p port.ioproc) 'ready?)
                      (tuple-ref p port.iodata)))))
              (else #f)))
      (error 'char-ready? "not textual input" p)))

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
            [(fx= type type:binary-input) #t]
            [(fx= type type:textual-input)
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
  (io/port-position-nocache p))

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

(define (io/port-transcoder p)
  (tuple-ref p port.transcoder))

(define (io/textual-port? p)
  (not (fx= 0 (fxlogand type-mask:binary/textual
                        (tuple-ref p port.type)))))

(define (io/r7rs-textual-port? p)
  (not (fx= 0 (fxlogand type-mask:binary/textual
                        (tuple-ref p port.r7rstype)))))

(define (io/binary-port? p)
  (fx= 0 (fxlogand type-mask:binary/textual (tuple-ref p port.type))))

(define (io/r7rs-binary-port? p)
  (fx= 0 (fxlogand type-mask:binary/textual
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

    (cond ((fx= codec codec:binary)  'binary)
          ((fx= codec codec:latin-1) 'latin-1)
          ((fx= codec codec:utf-8)   'utf-8)
          ((fx= codec codec:utf-16)  'utf-16)
          (else
           (assertion-violation 'transcoder-codec
                                "weird transcoder" t)))))


(define (io/transcoder-eol-style t)
  (let ((style (fxlogand t transcoder-mask:eolstyle)))
    (cond ((fx= style eolstyle:none)  'none)
          ((fx= style eolstyle:lf)    'lf)
          ((fx= style eolstyle:nel)   'nel)
          ((fx= style eolstyle:ls)    'ls)
          ((fx= style eolstyle:cr)    'cr)
          ((fx= style eolstyle:crlf)  'crlf)
          ((fx= style eolstyle:crnel) 'crnel)
          (else
           (assertion-violation 'transcoder-eol-style
                                "weird transcoder" t)))))

(define (io/transcoder-error-handling-mode t)
  (let ((mode (fxlogand t transcoder-mask:errmode)))
    (cond ((fx= mode errmode:ignore)  'ignore)
          ((fx= mode errmode:replace) 'replace)
          ((fx= mode errmode:raise)   'raise)
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
        (let ([byte (bytevector-ref buf ptr)])
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
        (let ([unit (bytevector-ref buf ptr)])
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
                  (io/return-eol p lookahead?unit)]
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
                            (io/get-char-utf8 p lookahead? unit buf ptr lim)]
                          [else (error 'io/get-char "unimplemented codec" codec p)]))]))]))])))