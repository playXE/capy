;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017, 2018, 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;
;;


(define (reader-lookahead-char reader)
  (lookahead-char (reader-port reader)))

(define (reader-get-char reader) 
  (let ([c (get-char (reader-port reader))])
    (when (eqv? c (integer->char **linefeed**))
      (reader-line-set! reader (+ (reader-line reader) 1))
      (reader-column-set! reader -1))
    (reader-column-set! reader (+ (reader-column reader) 1))
    c))  

(define reader-rtd 
  (make-record-type-descriptor 
    'reader
    #f 
    #f 
    #t 
    #f 
    '#(
      (immutable port)
      (immutable filename)
      (mutable line) (mutable column)
      (mutable saved-line) (mutable saved-column)
      (mutable fold-case?)
      (mutable mode) ; a symbol: r7rs, r5rs, r6rs, rnrs
      (mutable tolerant?))))

(define reader-rcd 
  (make-record-constructor-descriptor reader-rtd #f 
    (lambda (p)
      (lambda (port filename)
          (p port filename 1 0 1 0 #f 'rnrs #f)))))

(define make-reader (record-constructor reader-rcd))
(define reader? (record-predicate reader-rtd))
(define reader-port (record-accessor reader-rtd 0))
(define reader-filename (record-accessor reader-rtd 1))
(define reader-line (record-accessor reader-rtd 2))
(define reader-line-set! (record-mutator reader-rtd 2))
(define reader-column (record-accessor reader-rtd 3))
(define reader-column-set! (record-mutator reader-rtd 3))
(define reader-saved-line (record-accessor reader-rtd 4))
(define reader-saved-line-set! (record-mutator reader-rtd 4))
(define reader-saved-column (record-accessor reader-rtd 5))
(define reader-saved-column-set! (record-mutator reader-rtd 5))
(define reader-fold-case? (record-accessor reader-rtd 6))
(define reader-fold-case?-set! (record-mutator reader-rtd 6))
(define reader-mode (record-accessor reader-rtd 7))
(define reader-mode-set! (record-mutator reader-rtd 7))
(define reader-tolerant? (record-accessor reader-rtd 8))
(define reader-tolerant?-set! (record-mutator reader-rtd 8))


(define (reader-mark reader)
  (reader-saved-line-set! reader (reader-line reader))
  (reader-saved-column-set! reader (reader-column reader)))

(define annotation-rtd 
  (make-record-type-descriptor 
    'annotation 
    #f 
    'annotation-v0-dc9637b3-85e8-4599-9fe9-151508e9c850
    #t 
    #f 
    '#(
      (immutable expression)
      (immutable source)
      (immutable stripped))))

(define annotation-rcd 
  (make-record-constructor-descriptor annotation-rtd #f 
    (lambda (p)
      (lambda (expression source stripped)
          (p expression source stripped)))))

(define make-annotation (record-constructor annotation-rcd))
(define annotation? (record-predicate annotation-rtd))
(define annotation-expression (record-accessor annotation-rtd 0))
(define annotation-source (record-accessor annotation-rtd 1))
(define annotation-stripped (record-accessor annotation-rtd 2))

(define &source-information
  (let ((rtd (make-record-type-descriptor '&source-information (record-type-rtd &condition) #f #f #f '#((immutable file-name) (immutable line) (immutable column)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&source-information rtd rcd))))
(define make-source-condition (record-constructor (record-type-rcd &source-information)))
(define source-condition? (condition-predicate (record-type-rtd &source-information)))
(define source-condition-file-name (condition-accessor (record-type-rtd &source-information) (record-accessor (record-type-rtd &source-information) 0)))
(define source-condition-line (condition-accessor (record-type-rtd &source-information) (record-accessor (record-type-rtd &source-information) 1)))
(define source-condition-column (condition-accessor (record-type-rtd &source-information) (record-accessor (record-type-rtd &source-information) 2)))
(define (annotation-source->condition x)
  (if (vector? x)
    (apply make-source-condition (vector->list x))
    (condition)))

(define (reader-source reader)
  (vector 
    (reader-filename reader)
    (reader-line reader)
    (reader-column reader)))
  
(define (annotate source stripped datum)
  (assert (vector? source))
  (make-annotation datum source stripped))

(define (lexical-condition reader msg irritants)
  (condition 
    (make-lexical-violation)
    (make-message-condition msg)
    (make-source-condition (reader-filename reader)
                           (reader-line reader)
                           (reader-column reader))
    (make-irritants-condition irritants)))
  
(define (reader-error reader msg . irritants)
  (raise (lexical-condition reader msg irritants)))

(define (reader-warning reader msg . irritants)
  ;; Recoverable if the reader is in tolerant mode.
  (if (reader-tolerant? reader)
      (raise-continuable
        (condition
         (make-warning)
         (lexical-condition reader msg irritants)))
      (apply reader-error reader msg irritants)))

(define (assert-mode p msg modes)
  (unless (memq (reader-mode p) modes)
    (reader-warning p (string-append msg " is not allowed in this mode") (reader-mode p))))

(define (eof-warning reader)
  (reader-warning reader "unexpected end of file"))

(define (unicode-scalar-value? sv)
  (and (fixnum? sv)
       (<= 0 sv 1114111)
       (not (<= 55296 sv 57343))))

(define (char-delimiter? reader c)
  (or (eof-object? c)
    (char-whitespace? c)
    (case (reader-mode reader)
      [(r6rs)
        (memv c '(#\( #\) #\[ #\] #\" #\; #\#))]
       [(r7rs)
         (memv c '(#\( #\) #\" #\; #\|))]
       [else
         (memv c '(#\( #\) #\[ #\] #\" #\; #\# #\|))])))

(define (reader-get-line reader)
  (call-with-string-output-port 
    (lambda (out)
      (do ([c (reader-get-char reader) (reader-get-char reader)])
          [(or (eqv? c (integer->char **linefeed**)) (eof-object? c))]
          (put-char out c)))))

(define (reader-get-whitespace reader char)
  (call-with-string-output-port
   (lambda (out)
     (let lp ((char char))
       (put-char out char)
       (let ((char (reader-lookahead-char reader)))
         (when (and (char? char) (char-whitespace? char))
           (lp (reader-get-char reader))))))))

;; Get an inline hex escape (escaped character inside an identifier).
(define (get-inline-hex-escape p)
  (reader-mark p)
  (let lp ((digits '()))
    (let ((c (reader-get-char p)))
      (cond ((eof-object? c)
             (eof-warning p)
             (integer->char 65533))
            ((or (char<=? #\0 c #\9)
                 (char<=? #\a c #\f)
                 (char<=? #\A c #\F))
             (lp (cons c digits)))
            ((and (char=? c #\;) (pair? digits))
             (let ((sv (string->number (list->string (reverse digits)) 16)))
               (cond ((unicode-scalar-value? sv)
                      (integer->char sv))
                     (else
                      (reader-warning p "Inline hex escape outside valid range" sv)
                      (integer->char 65533)))))
            (else
             (reader-warning p "Invalid inline hex escape" c)
             (integer->char 65533))))))

(define (reader-get-identifier p initial-char pipe-quoted?)
  (let lp ((chars (if initial-char (list initial-char) '())))
    (let ((c (reader-lookahead-char p)))
      (cond
        ((and (char? c)
              (or (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char<=? #\0 c #\9)
                  (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                            #\+ #\- #\. #\@))
                  (and (> (char->integer c) 127)
                       (memq (char-general-category c) ;XXX: could be done faster
                             '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co Nd Mc Me)))
                  (and (memv (reader-mode p) '(rnrs r7rs))
                       (memv c '(#\x200C #\x200D)))))
         (lp (cons (reader-get-char p) chars)))
        ((and pipe-quoted? (char? c) (not (memv c '(#\| #\\))))
         (lp (cons (reader-get-char p) chars)))
        ((or (char-delimiter? p c) (and pipe-quoted? (eqv? c #\|)))
         (when (eqv? c #\|)
           (reader-get-char p))
         (let ((id (list->string (reverse chars))))
           (if (reader-fold-case? p)
               (values 'identifier (string->symbol (string-foldcase id)))
               (values 'identifier (string->symbol id)))))
        ((char=? c #\\)           ;\xUUUU;
         (reader-get-char p)             ;consume #\\
         (let ((c (reader-get-char p)))  ;should be #\x
           (cond ((eqv? c #\x)
                  (lp (cons (get-inline-hex-escape p) chars)))
                 ((and pipe-quoted?
                       (assv c '((#\" . #\")
                                 (#\\ . #\\)
                                 (#\a . #\alarm)
                                 (#\b . #\backspace)
                                 (#\t . #\tab)
                                 (#\n . #\linefeed)
                                 (#\r . #\return)
                                 (#\| . #\|))))
                  => (lambda (c) (lp (cons (cdr c) chars))))
                 (else
                  (if (eof-object? c)
                      (eof-warning p)
                      (reader-warning p "Invalid character following \\"))
                  (lp chars)))))
        (else
         (reader-warning p "Invalid character in identifier" c)
         (reader-get-char p)
         (lp chars))))))

;; Get a number from the reader.
(define (reader-get-number p initial-chars)
  (let lp ((chars initial-chars))
    (let ((c (reader-lookahead-char p)))
      (cond ((and (not (eqv? c #\#)) (char-delimiter? p c))
             ;; TODO: some standard numbers are not supported
             ;; everywhere, should use a number lexer.
             (let ((str (list->string (reverse chars))))
               (cond ((string->number str) =>
                      (lambda (num)
                        (values 'value num)))
                     ((and (memq (reader-mode p) '(rnrs r7rs))
                           ;; TODO: This is incomplete.
                           (not (and (pair? initial-chars)
                                     (char<=? #\0 (car initial-chars) #\9))))
                      (values 'identifier (string->symbol str)))
                     (else
                      (reader-warning p "Invalid number syntax" str)
                      (values 'identifier (string->symbol str))))))
            (else
             (lp (cons (reader-get-char p) chars)))))))

;; Get a string datum from the reader.
(define (reader-get-string p)
  (let lp ((chars '()))
    (let ((c (reader-lookahead-char p)))
      (cond ((eof-object? c)
             (eof-warning p)
             c)
            ((char=? c #\")
             (reader-get-char p)
             (list->string (reverse chars)))
            ((char=? c #\\)           ;escapes
             (reader-get-char p)             ;consume #\\
             (let ((c (reader-lookahead-char p)))
               (cond ((eof-object? c)
                      (eof-warning p)
                      c)
                     ((or (memv c '(#\tab #\linefeed #\x85 #\x2028))
                          (eq? (char-general-category c) 'Zs))
                      ;; \<intraline whitespace>*<line ending>
                      ;; <intraline whitespace>*
                      (letrec ((skip-intraline-whitespace*
                                (lambda ()
                                  (let ((c (reader-lookahead-char p)))
                                    (cond ((eof-object? c)
                                           (eof-warning p)
                                           c)
                                          ((or (char=? c '#\tab)
                                               (eq? (char-general-category c) 'Zs))
                                           (reader-get-char p)
                                           (skip-intraline-whitespace*))))))
                               (skip-newline
                                (lambda ()
                                  (let ((c (reader-get-char p)))
                                    ;; XXX: it appears that the port
                                    ;; transcoder is meant to
                                    ;; replace all these linefeeds
                                    ;; with #\linefeed.
                                    (cond ((eof-object? c) c)
                                          ((memv c '(#\linefeed #\x85 #\x2028)))
                                          ((char=? c #\return)
                                           (when (memv (reader-lookahead-char p)
                                                       '(#\linefeed #\x85))
                                             (reader-get-char p)))
                                          (else
                                           (reader-warning p "Expected a line ending" c)))))))
                        (skip-intraline-whitespace*)
                        (skip-newline)
                        (skip-intraline-whitespace*)
                        (lp chars)))
                     (else
                      (lp (cons
                           (case (reader-get-char p)
                             ((#\") #\")
                             ((#\\) #\\)
                             ((#\a) #\alarm)
                             ((#\b) #\backspace)
                             ((#\t) #\tab)
                             ((#\n) #\linefeed)
                             ((#\v) (assert-mode p "\\v" '(rnrs r6rs)) #\vtab)
                             ((#\f) (assert-mode p "\\f" '(rnrs r6rs)) #\page)
                             ((#\r) #\return)
                             ((#\|) (assert-mode p "\\|" '(rnrs r7rs)) #\|)
                             ((#\x) (get-inline-hex-escape p))
                             (else
                              (reader-warning p "Invalid escape in string" c)
                              (integer->char 65533)))
                           chars))))))
            (else
             (lp (cons (reader-get-char p) chars)))))))

;; Gets a nested comment from the reader.
(define (get-nested-comment reader)
  ;; The reader is immediately after "#|".
  (call-with-string-output-port
   (lambda (out)
     (let lp ((levels 1) (c0 (reader-get-char reader)))
       (let ((c1 (reader-get-char reader)))
         (cond ((eof-object? c0)
                (eof-warning reader))
               ((and (eqv? c0 #\|) (eqv? c1 #\#))
                (unless (eqv? levels 1)
                  (put-char out c0)
                  (put-char out c1)
                  (lp (- levels 1) (reader-get-char reader))))
               ((and (eqv? c0 #\#) (eqv? c1 #\|))
                (put-char out c0)
                (put-char out c1)
                (lp (+ levels 1) (reader-get-char reader)))
               (else
                (put-char out c0)
                (lp levels c1))))))))

;; Gets a #! !# comment from the reader.
(define (get-!-comment reader)
  ;; The reader is immediately after "#!".
  (call-with-string-output-port
   (lambda (out)
     (let lp ((c0 (reader-get-char reader)))
       (let ((c1 (reader-get-char reader)))
         (cond ((eof-object? c0)
                (eof-warning reader))
               ((and (eqv? c0 #\!) (eqv? c1 #\#))
                #f)
               (else
                (put-char out c0)
                (lp c1))))))))

;; Get a comment from the reader (including the terminating whitespace).
(define (get-comment reader)
  ;; The reader is immediately after #\;.
  (call-with-string-output-port
   (lambda (out)
     (let lp ()
       (let ((c (reader-get-char reader)))
         (unless (eof-object? c)
           (put-char out c)
           (cond ((memv c '(#\linefeed #\x85 #\x2028 #\x2029)))
                 ((char=? c #\return)
                  ;; Weird line ending. This lookahead is what forces
                  ;; the procedure to include the terminator.
                  (when (memv (reader-lookahead-char reader) '(#\linefeed #\x85))
                    (put-char out (get-char reader))))
                 (else
                  (lp)))))))))

;; Whitespace and comments can appear anywhere.
(define (atmosphere? type)
  (memq type '(directive whitespace comment inline-comment nested-comment)))

;; Get the next lexeme from the reader, ignoring anything that is
;; like a comment.
(define (get-lexeme p)
  (let-values (((type lexeme) (get-token p)))
    (if (atmosphere? type)
        (get-lexeme p)
        (values type lexeme))))

;; Get the next token. Can be a lexeme, directive, whitespace or comment.
(define (get-token p)
  (assert (reader? p))
  (reader-mark p)
  (let ((c (reader-get-char p)))
    (cond
      ((eof-object? c)
       (values 'eof c))
      ((char-whitespace? c)
       (values 'whitespace (reader-get-whitespace p c)))
      ((char=? c #\;)                 ;a comment like this one
       (values 'comment (get-comment p)))
      ((char=? c #\#)                 ;the mighty octothorpe
       (let ((c (reader-get-char p)))
         (case c
           ((#\() (values 'vector #f))
           ((#\') (values 'abbrev 'syntax))
           ((#\`) (values 'abbrev 'quasisyntax))
           ((#\,)
            (case (reader-lookahead-char p)
              ((#\@)
               (reader-get-char p)
               (values 'abbrev 'unsyntax-splicing))
              (else (values 'abbrev 'unsyntax))))
           ((#\v)                       ;r6rs
            (let* ((c1 (and (eqv? (reader-lookahead-char p) #\u) (reader-get-char p)))
                   (c2 (and (eqv? c1 #\u) (eqv? (reader-lookahead-char p) #\8) (reader-get-char p)))
                   (c3 (and (eqv? c2 #\8) (eqv? (reader-lookahead-char p) #\() (reader-get-char p))))
              (cond ((and (eqv? c1 #\u) (eqv? c2 #\8) (eqv? c3 #\())
                     (assert-mode p "#vu8(" '(rnrs r6rs))
                     (values 'bytevector #f))
                    (else
                     (reader-warning p "Expected #vu8(")
                     (get-token p)))))
           ((#\u #\U)                   ;r7rs
            (let* ((c1 (and (eqv? (reader-lookahead-char p) #\8) (reader-get-char p)))
                   (c2 (and (eqv? c1 #\8) (eqv? (reader-lookahead-char p) #\() (reader-get-char p))))
              (cond ((and (eqv? c1 #\8) (eqv? c2 #\())
                     (assert-mode p "#u8(" '(rnrs r7rs))
                     (values 'bytevector #f))
                    (else
                     (reader-warning p "Expected #u8(")
                     (get-token p)))))
           ((#\;)                     ;s-expr/datum comment
            (let lp ((atmosphere '()))
              (let-values (((type token) (get-token p)))
                (cond ((eq? type 'eof)
                       (eof-warning p)
                       (values 'inline-comment (cons (reverse atmosphere) p)))
                      ((atmosphere? type)
                       (lp (cons (cons type token) atmosphere)))
                      (else
                       (let-values ([(d _) (handle-lexeme p type token #f #t)])
                         (values 'inline-comment (cons (reverse atmosphere) d))))))))
           ((#\|)                     ;nested comment
            (values 'nested-comment (get-nested-comment p)))
           ((#\!)                     ;#!r6rs etc
            (let ((next-char (reader-lookahead-char p)))
              (cond ((and (= (reader-saved-line p) 1) (memv next-char '(#\/ #\space)))
                     (let ((line (reader-saved-line p))
                           (column (reader-saved-column p)))
                       (values 'shebang `(,line ,column ,(get-line p)))))
                    ((and (char? next-char) (char-alphabetic? next-char))
                     (let-values (((type id) (get-token p)))
                       (cond
                         ((eq? type 'identifier)
                          (case id
                            ((r6rs)          ;r6rs.pdf
                             (assert-mode p "#!r6rs" '(rnrs r6rs))
                             (reader-mode-set! p 'r6rs))
                            ((fold-case)     ;r6rs-app.pdf
                             (assert-mode p "#!fold-case" '(rnrs r6rs r7rs))
                             (reader-fold-case?-set! p #t))
                            ((no-fold-case)  ;r6rs-app.pdf
                             (assert-mode p "#!no-fold-case" '(rnrs r6rs r7rs))
                             (reader-fold-case?-set! p #f))
                            ((r7rs)          ;oddly missing in r7rs
                             (assert-mode p "#!r7rs" '(rnrs))
                             (reader-mode-set! p 'r7rs))
                            ((false)         ;r2rs
                             (assert-mode p "#!false" '(rnrs r2rs)))
                            ((true)          ;r2rs
                             (assert-mode p "#!true" '(rnrs r2rs)))
                            (else
                             (reader-warning p "Invalid directive" type id)))
                          (cond ((assq id '((false . #f) (true . #t)))
                                 => (lambda (x) (values 'value (cdr x))))
                                (else
                                 (values 'directive id))))
                         (else
                          (reader-warning p "Expected an identifier after #!")
                          (get-token p)))))
                    ((eq? (reader-mode p) 'rnrs)
                     ;; Guile compat.
                     (get-token p)
                     (values 'comment (get-!-comment p)))
                    (else
                     (reader-warning p "Expected an identifier after #!")
                     (get-token p)))))
           ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E)
            (reader-get-number p (list c #\#)))
           ((#\t #\T)
            (unless (char-delimiter? p (reader-lookahead-char p))
              (if (memq (reader-mode p) '(rnrs r7rs))
                  (let* ((c1 (and (memv (reader-lookahead-char p) '(#\r #\R)) (reader-get-char p)))
                         (c2 (and c1 (memv (reader-lookahead-char p) '(#\u #\U)) (reader-get-char p)))
                         (c3 (and c2 (memv (reader-lookahead-char p) '(#\e #\E)) (reader-get-char p))))
                    (unless (and c1 c2 c3 (char-delimiter? p (reader-lookahead-char p)))
                      (reader-warning p "Expected #true")))
                  (reader-warning p "A delimiter is expected after #t")))
            (values 'value #t))
           ((#\f #\F)
            (unless (char-delimiter? p (reader-lookahead-char p))
              (if (memq (reader-mode p) '(rnrs r7rs))
                  (let* ((c1 (and (memv (reader-lookahead-char p) '(#\a #\A)) (reader-get-char p)))
                         (c2 (and c1 (memv (reader-lookahead-char p) '(#\l #\L)) (reader-get-char p)))
                         (c3 (and c2 (memv (reader-lookahead-char p) '(#\s #\S)) (reader-get-char p)))
                         (c4 (and c3 (memv (reader-lookahead-char p) '(#\e #\E)) (reader-get-char p))))
                    (unless (and c1 c2 c3 c4 (char-delimiter? p (reader-lookahead-char p)))
                      (reader-warning p "Expected #false" c1 c2 c3 c4)))
                  (reader-warning p "A delimiter is expected after #f")))
            (values 'value #f))
           ((#\\)
            (let lp ((char* '()))
              (let ((c (reader-lookahead-char p)))
                (cond ((and (pair? char*) (char-delimiter? p c))
                       (let ((char* (reverse char*)))
                         (cond ((null? char*)
                                (reader-warning p "Empty character name")
                                (values 'value #\xFFFD))
                               ((null? (cdr char*)) (values 'value (car char*)))
                               ((char=? (car char*) #\x)
                                (cond ((for-all (lambda (c)
                                                  (or (char<=? #\0 c #\9)
                                                      (char<=? #\a c #\f)
                                                      (char<=? #\A c #\F)))
                                                (cdr char*))
                                       (let ((sv (string->number (list->string (cdr char*)) 16)))
                                         (cond ((unicode-scalar-value? sv)
                                                (values 'value (integer->char sv)))
                                               (else
                                                (reader-warning p "Hex-escaped character outside valid range" sv)
                                                (values 'value #\xFFFD)))))
                                      (else
                                       (reader-warning p "Invalid character in hex-escaped character"
                                                       (list->string (cdr char*)))
                                       (values 'value #\xFFFD))))
                               (else
                                (let ((char-name (list->string char*))
                                      (char-names '(("nul" #\nul r6rs)
                                                    ("null" #\nul r7rs)
                                                    ("alarm" #\alarm r6rs r7rs)
                                                    ("backspace" #\backspace r6rs r7rs)
                                                    ("tab" #\tab r6rs r7rs)
                                                    ("linefeed" #\linefeed r6rs)
                                                    ("newline" #\linefeed r5rs r6rs r7rs)
                                                    ("vtab" #\vtab r6rs)
                                                    ("page" #\page r6rs)
                                                    ("return" #\return r6rs r7rs)
                                                    ("esc" #\esc r6rs)
                                                    ("escape" #\esc r7rs)
                                                    ("space" #\space r5rs r6rs r7rs)
                                                    ("delete" #\delete r6rs r7rs))))
                                  (cond
                                    ((or (assoc char-name char-names)
                                         (and (reader-fold-case? p)
                                              (assoc (string-foldcase char-name)
                                                     char-names)))
                                     => (lambda (char-data)
                                          (assert-mode p char-name (cons 'rnrs (cddr char-data)))
                                          (values 'value (cadr char-data))))
                                    (else
                                     (reader-warning p "Invalid character name" char-name)
                                     (values 'value #\xFFFD))))))))
                      ((and (null? char*) (eof-object? c))
                       (eof-warning p)
                       (values 'value #\xFFFD))
                      (else
                       (lp (cons (reader-get-char p) char*)))))))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (assert-mode p "#<n>=<datum> and #<n>#" '(rnrs r7rs))
            (let lp ((char* (list c)))
              (let ((next (reader-lookahead-char p)))
                (cond
                  ((eof-object? next)
                   (eof-warning p)
                   (reader-get-char p))
                  ((char<=? #\0 next #\9)
                   (lp (cons (reader-get-char p) char*)))
                  ((char=? next #\=)
                   (reader-get-char p)
                   (values 'label (string->number (list->string (reverse char*)) 10)))
                  ((char=? next #\#)
                   (reader-get-char p)
                   (values 'reference (string->number (list->string (reverse char*)) 10)))
                  (else
                   (reader-warning p "Expected #<n>=<datum> or #<n>#" next)
                   (get-token p))))))
           (else
            (reader-warning p "Invalid #-syntax" c)
            (get-token p)))))
      ((char=? c #\")
       (values 'value (reader-get-string p)))
      ((memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (reader-get-number p (list c)))
      ((memv c '(#\- #\+))            ;peculiar identifier
       (cond ((and (char=? c #\-) (eqv? #\> (reader-lookahead-char p))) ;->
              
              (reader-get-identifier p c #f))
             ((char-delimiter? p (reader-lookahead-char p))
              (values 'identifier (if (eqv? c #\-) '- '+)))
             (else
              (reader-get-number p (list c)))))
      ((char=? c #\.)                 ;peculiar identifier
       (cond ((char-delimiter? p (reader-lookahead-char p))
              (values 'dot #f))
             ((and (eq? (reader-mode p) 'r6rs)
                   (eqv? #\. (reader-lookahead-char p)))
              (reader-get-char p)            ;consume second dot
              (unless (eqv? #\. (reader-get-char p)) ;consume third dot
                (reader-warning p "Expected the ... identifier"))
              (unless (char-delimiter? p (reader-lookahead-char p))
                (reader-warning p "Expected the ... identifier"))
              (values 'identifier '...))
             (else
              (reader-get-number p (list c)))))
      ((or (char<=? #\a c #\z) (char<=? #\A c #\Z) ;<constituent> and <special initial>
           (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
           (and (memv (reader-mode p) '(rnrs r7rs))
                (or (eqv? c #\@) (memv c '(#\x200C #\x200D))))
           (and (> (char->integer c) 127)
                (memq (char-general-category c)
                      '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))
       (reader-get-identifier p c #f))
      ((char=? c #\\)                 ;<inline hex escape>
       (let ((c (reader-get-char p)))
         (cond ((eqv? c #\x)
                (reader-get-identifier p (get-inline-hex-escape p) #f))
               (else
                (cond ((eof-object? c)
                       (eof-warning p))
                      (else
                       (reader-warning p "Invalid character following \\")))
                (get-token p)))))
      (else
       (case c
         ((#\() (values 'openp #f))
         ((#\)) (values 'closep #f))
         ((#\[) (values 'openb #f))
         ((#\]) (values 'closeb #f))
         ((#\') (values 'abbrev 'quote))
         ((#\`) (values 'abbrev 'quasiquote))
         ((#\,)
          (case (reader-lookahead-char p)
            ((#\@)
             (reader-get-char p)
             (values 'abbrev 'unquote-splicing))
            (else (values 'abbrev 'unquote))))
         ((#\|)
          (assert-mode p "Quoted identifiers" '(rnrs r7rs))
          (reader-get-identifier p #f 'pipe))
         (else
          (reader-warning p "Invalid leading character" c)
          (get-token p)))))))

;;; Datum reader

;; <datum> → <lexeme datum>
;;          | <compound datum>
;; <lexeme datum> → <boolean> | <number>
;;          | <character> | <string> | <symbol>
;; <symbol> → <identifier>
;; <compound datum> → <list> | <vector> | <bytevector>
;; <list> → (<datum>*) | [<datum>*]
;;          | (<datum>+ . <datum>) | [<datum>+ . <datum>]
;;          | <abbreviation>
;; <abbreviation> → <abbrev prefix> <datum>
;; <abbrev prefix> → ' | ` | , | ,@
;;          | #' | #` | #, | #,@
;; <vector> → #(<datum>*)
;; <bytevector> → #vu8(<u8>*)
;; <u8> → 〈any <number> representing an exact
;;                    integer in {0, ..., 255}〉

(define (get-compound-datum p src terminator type labels)
  (define vec #f)                     ;TODO: ugly, should be rewritten
  (define vec^ #f)
  (let lp ((head '()) (head^ '()) (prev #f) (prev^ #f) (len 0))
    (let-values (((lextype x) (get-lexeme p)))
      (case lextype
        ((closep closeb eof)
         (unless (eq? lextype terminator)
           (if (eof-object? x)
               (eof-warning p)
               (reader-warning p "Mismatched parenthesis/brackets" lextype x terminator)))
         (case type
           ((vector)
            (let ((s (list->vector head))
                  (s^ (list->vector head^)))
              (set! vec s)
              (set! vec^ (annotate src s s^))
              (values vec vec^)))
           ((list)
            (values head (annotate src head head^)))
           ((bytevector)
            (let ((s (u8-list->bytevector head)))
              (values s (annotate src s s))))
           (else
            (reader-error p "Internal error in get-compound-datum" type))))
        ((dot)                          ;a dot like in (1 . 2)
         (cond
           ((eq? type 'list)
            (let*-values (((lextype x) (get-lexeme p))
                          ((d d^) (handle-lexeme p lextype x labels #t)))
              (let-values (((termtype _) (get-lexeme p)))
                (cond ((eq? termtype terminator))
                      ((eq? termtype 'eof)
                       (eof-warning p))
                      (else
                       (reader-warning p "Improperly terminated dot list"))))
              (cond ((pair? prev)
                     (cond ((eq? d^ 'reference)
                            (register-reference p labels d
                                                (lambda (d d^)
                                                  (set-cdr! prev d)
                                                  (set-cdr! prev^ d^))))
                           (else
                            (set-cdr! prev d)
                            (set-cdr! prev^ d^))))
                    (else
                     (reader-warning p "Unexpected dot")))
              (values head (annotate src head head^))))
           (else
            (reader-warning p "Dot used in non-list datum")
            (lp head head^ prev prev^ len))))
        (else
         (let-values (((d d^) (handle-lexeme p lextype x labels #t)))
           (cond
             ((and (eq? type 'bytevector)
                   (or (eq? d^ 'reference)
                       (not (and (fixnum? d) (fx<=? 0 d 255)))))
              (reader-warning p "Invalid datum in bytevector" x)
              (lp head head^ prev prev^ len))
             (else
              (let ((new-prev (cons d '()))
                    (new-prev^ (cons d^ '())))
                (when (pair? prev)
                  (set-cdr! prev new-prev)
                  (set-cdr! prev^ new-prev^))
                (when (eq? d^ 'reference)
                  (register-reference p labels d
                                      (if (eq? type 'vector)
                                          (lambda (d d^)
                                            (vector-set! vec len d)
                                            (vector-set! (annotation-expression vec^)
                                                         len d^))
                                          (lambda (d d^)
                                            (set-car! new-prev d)
                                            (set-car! new-prev^ d^)))))
                (if (pair? head)
                    (lp head head^ new-prev new-prev^ (fx+ len 1))
                    (lp new-prev new-prev^ new-prev new-prev^ (fx+ len 1))))))))))))

(define (handle-lexeme p lextype x labels allow-refs?)
  (let ((src (reader-source p)))
    (case lextype
      ((openp)
       (get-compound-datum p src 'closep 'list labels))
      ((openb)
       (assert-mode p "Square brackets" '(rnrs r6rs))
       (get-compound-datum p src 'closeb 'list labels))
      ((vector)
       (get-compound-datum p src 'closep 'vector labels))
      ((bytevector)
       ;; TODO: open-bytevector-output-port would be faster
       (get-compound-datum p src 'closep 'bytevector labels))
      ((value eof identifier)
       (values x (annotate src x x)))
      ((abbrev)
       (let-values (((type lex) (get-lexeme p)))
         (cond ((eq? type 'eof)
                (eof-warning p)
                (values lex lex))
               (else
                (let-values (((d d^) (handle-lexeme p type lex labels #t)))
                  (let ((s (list x d)))
                    (values s (annotate src s (list x d^)))))))))
      ((label)
       ;; The object that follows this label can be referred
       ;; back from elsewhere.
       (let*-values (((lextype lexeme) (get-lexeme p))
                     ((d d^) (handle-lexeme p lextype lexeme labels allow-refs?)))
         (register-label p labels x d d^)
         (values d d^)))
      (else
       (cond ((and allow-refs? (eq? lextype 'reference))
              (values x 'reference))    ;XXX: different return types
             (else
              ;; Ignore the shebang ("#!/" or "#! " at the start of files).
              ;; FIXME: should only work for programs.
              (unless (and (eq? lextype 'shebang) (eqv? (car x) 1) (eqv? (cadr x) 0))
                (reader-warning p "Unexpected lexeme" lextype x))
              (let-values (((lextype x) (get-lexeme p)))
                (handle-lexeme p lextype x labels allow-refs?))))))))


(define (read-annotated reader)
  (assert (reader? reader))
  (let ((labels (make-labels)))
    (let*-values (((type x) (get-lexeme reader))
                  ((_ d^) (handle-lexeme reader type x labels #f)))
      (resolve-labels reader labels)
      d^)))

(define (read-datum reader)
  (assert (reader? reader))
  (let ((labels (make-labels)))
    (let*-values (((type x) (get-lexeme reader))
                  ((d _) (handle-lexeme reader type x labels #f)))
      (resolve-labels reader labels)
      d)))

(define (make-labels)
  (make-core-hashtable 'eqv?))

(define (register-label p labels label datum annotated-datum)
  #f)

(define (register-reference _p labels label setter)
  #f)

(define (resolve-labels p labels)
  #f)

(define (get-port-reader p fn)
  (cond 
    [(port-reader p)]
    [else 
      (let ([reader (make-reader p (or fn (port-name p)))])
        (port-reader-set! p reader)
        reader)]))

(define (get-datum p)
  (read-datum (get-port-reader p #f)))


(define (read . port)
  (cond 
    [(null? port) (get-datum (current-input-port))]
    [(and (pair? port) (null? (cdr port)))
     (get-datum (car port))]
    [else
     (error 'read "too many arguments")]))