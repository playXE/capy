

; If slashify is true, print something that can be read back in.
; If slashify is false, use display semantics.
(define (print x p slashify)
  (define write-char io/write-char)
  (define quoters '(quote quasiquote unquote unquote-splicing
                    syntax quasisyntax unsyntax unsyntax-splicing))
  (define quoter-strings '((quote . "'")
                           (quasiquote . "`")
                           (unquote . ",")
                           (unquote-splicing . ",@")
                           (syntax . "#'")
                           (quasisyntax . "#`")
                           (unsyntax . "#,")
                           (unsyntax-splicing . "#,@")))
  
  (define funny-characters (list (integer->char 34) (integer->char 92)))

  (define ctrl-B (integer->char 2))
  (define ctrl-C (integer->char 3))
  (define ctrl-F (integer->char 6))

  
  ;; Which characters are written in hex and which are not
  ;; is completely implementation-dependent, so long as
  ;; get-datum can reconstruct the datum.
  ;;
  ;; Differences between this predicate and the rule for
  ;; hexifying the characters of an identifier:
  ;;     does not hexify Nd, Mc, or Me even at beginning of string
  ;;     does not hexify Ps, Pe, Pi, or Pf
  ;;     hexifies Co (private use)

  (define (print-in-string-without-hexifying? c)
    (let ((sv (char->integer c)))
      (or (<= 32 sv 126)
          (and (<= 128 sv)
               (not (= sv 171))    ; left double angle quote
               (not (= sv 187))    ; right double angle quote
               (not (memq (char-general-category c)
                          '(Zs Zl Zp Cc Cf Cs Co Cn)))))))
  ;; Same as above but also hexifies Mn, Mc, and Me.
  (define (print-as-char-without-hexifying? c)
    (let ((sv (char->integer c)))
      (or (<= 32 sv 126)
          (and (<= 128 sv)
               (not (memq (char-general-category c)
                          '(Mn Mc Me Zs Zl Zp Cc Cf Cs Co Cn)))))))

  (define (print x p slashify level)
    (cond [(and (not slashify)
                (zero? level))
           (printstr "..." p)]
          [(not (pair? x)) (patom x p slashify level)]
          [(and (memq (car x) quoters)
                (pair? (cdr x))
                (null? (cddr x)))
            (print-quoted x p slashify level)]
          [(and (not slashify)
                (zero? (- level 1)))
            (printstr "(...)" p)]
          [else 
            (write-char (string-ref "(" 0) p)
            (print (car x) p slashify (- level 1))
            (print-cdr (cdr x) p slashify 
                       (- level 1)
                       (- (or (print-length) 0) 1))]))
  (define (print-cdr x p slashify level length)
    (cond ((null? x)
           (write-char (string-ref ")" 0) p))
          ((and (not slashify)
                (zero? length))
           (printstr " ...)" p))
          ((pair? x)
           (write-char (integer->char 32) p)
           (print (car x) p slashify level)
           (print-cdr (cdr x) p slashify level (- length 1)))
          (else
           (printstr " . " p)
           (patom x p slashify level)
           (write-char (string-ref ")" 0) p))))
  
  (define (printsym s p) (printstr s p))

  (define (printstr s p)
    (define (loop x p i n)
      (if (< i n)
          (begin (write-char (string-ref x i) p)
                 (loop x p (+ 1 i) n))))

    (loop s p 0 (string-length s)))
  (define (print-slashed-symbol x p)
    (printstr (symbol->string x) p))
  (define (print-slashed-string s p)
    (define (loop i n)
      (if (< i n)
        (let* ([c (string-ref s i)]
               [sv (char->integer c)])
          (cond 
            [(<= 32 sv 126)
              (if (or (char=? c #\\)
                      (char=? c #\"))
                  (write-char #\\ p)
                (write-char c p))]
            [(and (<= 128 sv)
              (memq (transcoder-codec (port-transcoder p) '(utf-8 utf-16))))
              (write-char c p)]
            [else 
              (write-char #\\ p)
              (case sv 
                [(7) (write-char #\a p)]
                [(8) (write-char #\b p)]
                [(9) (write-char #\t p)]
                [(10) (write-char #\n p)]
                [(13) (write-char #\r p)]
                [else 
                  (let ([hexstring (number->string sv 16)])
                    (write-char #\x p)
                    (print-slashed-string hexstring p)
                    (write-char #\; p))])])
          (loop (+ i 1) n))))
      (loop 0 (string-length s)))

  (define (print-slashed-bytevector s p)
    (define (loop x p i n)
      (if (< i n)
          (let ((c (integer->char (bytevector-ref x i))))
            (if (memq c funny-characters)
                (write-char #\\ p))
            (write-char c p)
            (loop x p (+ 1 i) n))))

    (loop s p 0 (bytevector-length s)))

  (define (patom x p slashify level)
    (cond 
      [(eq? x '())            (printstr "()" p)]
      [(not x)                (printstr "#f" p)]
      [(eq? x #t)             (printstr "#t" p)]
      [(symbol? x)
        (printsym (symbol->string x) p)]
      [(number? x)            (printstr (number->string x) p)]
      [(char? x)
        (if slashify 
          (printcharacter x p)
          (write-char x p))]
      [(string? x)
        (if slashify
          (begin 
            (write-char #\" p)
            (print-slashed-string x p)
            (write-char #\" p))
          (printstr x p))]
      [(vector? x)
        (write-char #\# p)
        (print (vector->list x) p slashify level)]
      [(procedure? x) (printstr (.procedure->string x))]
      [(bytevector? x) (printbytevector x p slashify level)]
      [(eof-object? x) (printeof x p slashify)]
      [(eq? (undefined) x) (prinstr "#!undefined" p)]
      [(port? x) (printport x p slashify)]
      [(tuple? x) (printtuple x p slashify level)]
      [else (print-raw x)]))

  (define (printtuple x p slashify level)
    (let (
      [n (tuple-length x)])
      (cond 
        [(and 
          (tuple? (tuple-ref x 0))
          (eq? (tuple-ref (tuple-ref x 0) 0) 'type:record-type-descriptor))
          (let* (
            [rtd (tuple-ref x 0)]
            [name (tuple-ref rtd 1)]
            [opaque (tuple-ref rtd 2)])
            (cond 
              [opaque 
                (printstr "#<opaque-record " p)
                (print name p #t level)
                (printstr ">" p)]
              [else 
                (printstr "#<record " p)
                (print name p slashify level)
                (let loop ([i 1])
                  (when (< i n) 
                    (write-char (integer->char **space**) p)
                    (print (tuple-ref x i) p #t level)
                    (loop (+ i 1))))
                (printstr ">" p)]))]
        [else 
          (cond 
            [(get-tuple-type-name x) => 
              (lambda (name)
                (printstr "#<" p)
                (print name p slashify level)
                (let loop ([i 1])
                  (when (< i n) 
                    (write-char (integer->char **space**) p)
                    (print (tuple-ref x i) p #t level)
                    (loop (+ i 1))))
                (printstr ">" p))]
            [else 
              (printstr "#<tuple" p)
              (let loop ([i 0])
                (when (< i n) 
                  (write-char (integer->char **space**) p)
                  (print (tuple-ref x i) p #t level)
                  (loop (+ i 1))))
              (printstr ">" p)]
          )
        ])))

  (define (printcharacter c p)
    (write-char #\# p)
    (write-char #\\ p)
    (let ([k (char->integer c)])
      (cond [(<= k **space**)
             (cond [(= k **space**) (printstr "space" p)]
                   [(= k **newline**) (printstr "newline" p)]
                   [(= k **linefeed**) (printstr "linefeed" p)]
                   [(= k **return**) (printstr "return" p)]
                   [(= k **tab**) (printstr "tab" p)]
                   [(= k **alarm**) (printstr "alarm" p)]
                   [(= k **backspace**) (printstr "backspace")]
                   [(= k **vtab**) (printstr "vtab")]
                   [(= k **page**) (printstr "page")]
                   [(= k **nul**) (printstr "null")]
                   [(= k **esc**) (printstr "escape")]
                   [else 
                    (printstr "x" p)
                    (printstr (number->string k 16) p)])]
            [(< k **delete**) (write-char c p)]
            [(= k **delete**) (prinstr "delete" p)]
            [(and (memq (transcoder-codec (port-transcoder p) '(utf-8 utf-16)))
                  (print-as-char-without-hexifying? c))
              (write-char c p)]
            [else 
              (printstr "x" p)
              (printstr (number->string k 16) p)])))

  (define (printbytevecotr x p slashify level)
    (write-char #\# p)
    (write-char #\u p)
    (write-char #\8 p)
    (print (bytevector->list x) p slashify (- level 1)))
  (define (printport x p slashify)
    (cond 
      [(input-port? x) (printstr "#<input-port>" p)]
      [(output-port? x) (printstr "#<output-port>" p)]
      [else (printstr "#<port>" p)]))
  (define (printeof x p slashify)
    (printstr "#<eof>" p))
  (define (print-quoted x p slashify level)
    (printstr (cdr (assq (car x) quoter-strings)) p)
    (print (cadr x) p slashify (- level 1)))
  (print x p slashify (+ (or (print-level) -2) 1)))

(define print-length
  (make-parameter "print-length"
                  #f
                  (lambda (x)
                    (or (not x)
                        (and (fixnum? x) (>= x 0))))))

(define print-level
  (make-parameter "print-level"
                  #f
                  (lambda (x)
                    (or (not x)
                        (and (fixnum? x) (>= x 0))))))


(define write-simple
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p #t)
      (io/discretionary-flush p)
      (undefined))))

;;; For the display procedure, see print-shared.sch

(define display-simple
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p #f)
      (io/discretionary-flush p)
      (undefined))))

(define newline
  (lambda rest
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (write-char #\newline p)
      (io/discretionary-flush p)
      (undefined))))


(define (->string x)
  (call-with-string-output-port 
    (lambda (p)
      (print x p #f))))