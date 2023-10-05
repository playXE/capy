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
    (when (eqv? c #\linefeed)
      (reader-line-set! reader (+ (reader-line reader) 1))
      (reader-column-set! reader -1))
    (reader-column-set! reader (+ (reader-column reader) 1))
    c))  

(define reader:type 0)
(define reader:port 1)
(define reader:filename 2)
(define reader:line 3)
(define reader:column 4)
(define reader:saved-line 5)
(define reader:saved-column 6)
(define reader:fold-case? 7)
(define reader:mode 8) ; a symbol: rnrs, r5rs, r6rs, r7rs
(define reader:tolerant? 9) ; tolerand to errors?

(define (make-reader port filename)
  (tuple 'type:reader port filename 1 0 1 0 #f 'rnrs #f))

(define (reader-port reader)
  (tuple-ref reader reader:port))

(define (reader-filename reader)
  (tuple-ref reader reader:filename))

(define (reader-line reader)
  (tuple-ref reader reader:line))

(define (reader-column reader)
  (tuple-ref reader reader:column))

(define (reader-saved-line reader)
  (tuple-ref reader reader:saved-line))

(define (reader-saved-column reader)
  (tuple-ref reader reader:saved-column))

(define (reader-fold-case? reader)
  (tuple-ref reader reader:fold-case?))

(define (reader-mode reader)
  (tuple-ref reader reader:mode))

(define (reader-tolerant? reader)
  (tuple-ref reader reader:tolerant?))

(define (reader-line-set! reader line)
  (tuple-set! reader reader:line line))

(define (reader-column-set! reader column)
  (tuple-set! reader reader:column column))

(define (reader-saved-line-set! reader line)
  (tuple-set! reader reader:saved-line line))

(define (reader-saved-column-set! reader column)
  (tuple-set! reader reader:saved-column column))

(define (reader-fold-case-set! reader fold-case?)
  (tuple-set! reader reader:fold-case? fold-case?))

(define (reader-mode-set! reader mode)
  (tuple-set! reader reader:mode mode))

(define (reader-tolerant-set! reader tolerant?)
  (tuple-set! reader reader:tolerant? tolerant?))

(define (reader-mark reader)
  (reader-saved-line-set! reader (reader-line reader))
  (reader-saved-column-set! reader (reader-column reader)))

(define (reader? x) 
  (and (tuple? x) (eq? (tuple-ref x reader:type) 'type:reader)))

(define annotation:type 0)
(define annotation:expression 1)
(define annotation:source 2)
(define annotation:stripped 3)

