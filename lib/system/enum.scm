; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Finite sets of symbols, and their use as enumeration types.
;
; $Id: enum.sch 5093 2007-11-06 23:33:06Z will $
;
; R6RS reference implementation, modified for Larceny by removing
; the define-enumeration syntax, tests, and library wrapper.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Procedural interface.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an arbitrary list of symbols, returns the universe
; consisting of those symbols, considered as a subset of itself.
; The canonical ordering of the symbols in that universe
; is the same as the ordering of the first appearance of
; each symbol in the list that was passed to make-enumeration.

(define (make-enumeration symbols)

  ; Given a list of symbols, returns a list without duplicates,
  ; in order of their first occurrences in the original list.

  (define (remove-duplicates symbols)
    (define (loop symbols canonical)
      (cond ((null? symbols)
             (reverse canonical))
            ((memq (car symbols) canonical)
             (loop (cdr symbols) canonical))
            (else
             (loop (cdr symbols)
                   (cons (car symbols) canonical)))))
    (loop symbols '()))

  (if (not (list? symbols))
      (error 'make-enumeration "Non-list passed to make-enumeration" symbols))
  (for-each (lambda (x)
              (if (not (symbol? x))
                  (error 'make-enumeration
                         "Non-symbol in list passed to make-enumeration" x)))
            symbols)

  (let* ((this '*)            ; will become this enumeration type
         (this-universe '*)   ; will become the universal set for this type
         (symbols (remove-duplicates symbols))
         (canonical-ordering (list->vector symbols)))

    (call-with-values
     (lambda ()
       (enumeration:hash-table-components (remove-duplicates symbols)))
     (lambda (vec0 vec1 modulus max-distance)

       ; Given an arbitrary Scheme value x,
       ; returns the canonical index of x if it belongs to this universe,
       ; or returns #f if x does not belong to this universe.

       (define (index-of x)
         (if (not (symbol? x))
             #f
             (lookup-index x
                           (mod (symbol-hash x) modulus)
                           max-distance)))

       (define (lookup-index sym i bound)
         (cond ((eq? sym (vector-ref vec0 i))
                (vector-ref vec1 i))
               ((> bound 0)
                (lookup-index sym (+ i 1) (- bound 1)))
               (else #f)))

       

       ; Given a list of symbols that belong to this universe,
       ; returns a set consisting of those symbols.

       (define (constructor syms)
         (let ((bits (constructor-bits syms 0)))
           (enumeration:make-set bits this)))

       (define (constructor-bits syms bits)
         (if (null? syms)
             bits
             (let ((index (index-of (car syms))))
               (if index
                   (constructor-bits
                    (cdr syms)
                    (bitwise-ior bits
                               (bitwise-arithmetic-shift-left 1 index)))
                   (error "anonymous set constructor"
                          "Illegal value passed to set constructor"
                          (car syms))))))

      

       ; Given a subset of this universe, returns its elements
       ; as a list in canonical order.
       ; FIXME:  The reversal could be avoided by reversing the
       ; bits at the beginning and doing a little book-keeping.

       (define (deconstructor set)
         (if (eq? this (enumeration:set-type set))
             (bits-deconstructor (enumeration:set-bits set) '())
             (error 'anonymous-set-deconstructor
                    "Illegal set passed to set deconstructor" set)))

       (define (bits-deconstructor bits syms)
         (if (= bits 0)
             (reverse syms)
             (let* ((i (bitwise-first-bit-set bits))
                    (sym (vector-ref canonical-ordering i)))
               (bits-deconstructor (bitwise-copy-bit bits i 0)
                                   (cons sym syms)))))

           (set! this
                 (enumeration:make-type
                  (lambda () this-universe)
                  (lambda (x) (index-of x))
                  (lambda (syms) (constructor syms))
                  (lambda (set) (deconstructor set))))

       (set! this-universe (constructor symbols))

       this-universe))))

; Given an enum-set, returns the set of all symbols that
; comprise the universe of its argument.

(define (enum-set-universe set)
  ((enumeration:type-universe (enumeration:set-type set))))

; Given an enum-set, returns a unary predicate that, given a symbol
; that is in the universe, returns its 0-origin index within the
; canonical ordering of the symbols in the universe; given a value
; not in the universe, the unary predicate returns #f.

(define (enum-set-indexer set)
  (enumeration:type-indexer (enumeration:set-type set)))

; Given an enum-set, returns a unary procedure that, given a
; list of symbols that belong to the universe, returns the subset
; of that universe that contains exactly the symbols in the list.

(define (enum-set-constructor set)
  (enumeration:type-constructor (enumeration:set-type set)))

; Given an enum-set, returns a list of the symbols that belong to
; its argument, in the canonical order that was specified when
; define-enumeration was used to define the enumeration type.

(define (enum-set->list set)
  ((enumeration:type-deconstructor (enumeration:set-type set))
   set))

; Given an arbitrary object and an enum-set,
; returns true if and only if its first argument
; is an element of its second argument.

(define (enum-set-member? x set)
  (let ((bits (enumeration:set-bits set))
        (i ((enum-set-indexer set) x)))
    (and i
         (bitwise-bit-set? bits i))))

; Given two enum-sets, returns true if and only if the universe of its
; first argument is a subset of the universe of its second argument
; (considered as sets of symbols) and every element of its first
; argument is a member of its second.

(define (enum-set-subset? set1 set2)
  (let ((type1 (enumeration:set-type set1))
        (type2 (enumeration:set-type set2))
        (bits1 (enumeration:set-bits set1))
        (bits2 (enumeration:set-bits set2)))
    (if (eq? type1 type2)
        (zero? (bitwise-and bits1 (bitwise-not bits2)))
        (let ((u1 ((enumeration:type-universe type1)))
              (u2 ((enumeration:type-universe type2))))
          (cond ((eq? u1 u2)
                 ; FIXME:  Can this code ever be executed?
                 (zero? (bitwise-and bits1 (bitwise-not bits2))))
                ((enum-set-subset? u1 (enum-set-projection u2 u1))
                 (enum-set-subset? set1 (enum-set-projection set2 set1)))
                (else #f))))))

; Given two enum-sets, returns true if and only if its first argument is a
; subset of its second and vice versa, as determined by the
; enum-set-subset? procedure.

(define (enum-set=? set1 set2)
  (and (enum-set-subset? set1 set2)
       (enum-set-subset? set2 set1)))

; Given two enumeration sets that share the same
; enumeration type as universe, returns their union.

(define (enum-set-union set1 set2)
  (let ((type1 (enumeration:set-type set1))
        (type2 (enumeration:set-type set2))
        (bits1 (enumeration:set-bits set1))
        (bits2 (enumeration:set-bits set2)))
    (cond ((eq? type1 type2)
           (enumeration:make-set (bitwise-ior bits1 bits2) type1))
          ; FIXME: Isn't this redundant with the previous test?
          ((eq? (enumeration:type-universe type1)
                (enumeration:type-universe type2))
           (enumeration:make-set (bitwise-ior bits1 bits2) type1))
          (else
           (error 'enum-set-union "Incompatible sets" set1 set2)))))

; Given two enumeration sets that share the same
; enumeration type as universe, returns their intersection.

(define (enum-set-intersection set1 set2)
  (let ((type1 (enumeration:set-type set1))
        (type2 (enumeration:set-type set2))
        (bits1 (enumeration:set-bits set1))
        (bits2 (enumeration:set-bits set2)))
    (cond ((eq? type1 type2)
           (enumeration:make-set (bitwise-and bits1 bits2) type1))
          ; FIXME: Isn't this redundant with the previous test?
          ((eq? (enumeration:type-universe type1)
                (enumeration:type-universe type2))
           (enumeration:make-set (bitwise-and bits1 bits2) type1))
          (else
           (error 'enum-set-intersection "Incompatible sets" set1 set2)))))

; Given two enumeration sets that share the same
; enumeration type as universe, returns their difference.

(define (enum-set-difference set1 set2)
  (let ((type1 (enumeration:set-type set1))
        (type2 (enumeration:set-type set2))
        (bits1 (enumeration:set-bits set1))
        (bits2 (enumeration:set-bits set2)))
    (cond ((eq? type1 type2)
           (enumeration:make-set
            (bitwise-and bits1 (bitwise-not bits2)) type1))
          ; FIXME: Isn't this redundant with the previous test?
          ((eq? (enumeration:type-universe type1)
                (enumeration:type-universe type2))
           (enumeration:make-set
            (bitwise-and bits1 (bitwise-not bits2)) type1))
          (else
           (error 'enum-set-difference "Incompatible sets" set1 set2)))))

; Given an enumeration set, returns
; its complement with respect to its universe.

(define (enum-set-complement set)
  (enum-set-difference (enum-set-universe set) set))

; Given two arbitrary enumeration sets, returns the projection
; of the first into the universe of the second, dropping any
; elements of the first that do not belong to the universe of
; the second.

(define (enum-set-projection set1 set2)
  (let ((type1 (enumeration:set-type set1))
        (type2 (enumeration:set-type set2))
        (bits1 (enumeration:set-bits set1))
        (bits2 (enumeration:set-bits set2)))
    (cond ((eq? type1 type2)
           set1)
          ; FIXME: Isn't this redundant with the previous test?
          ((eq? (enumeration:type-universe type1)
                (enumeration:type-universe type2))
           set1)
          ((and #f (enum-set-subset? set1 set2))
           (enumeration:make-set bits1 type2))
          (else
           ((enumeration:type-constructor type2)
            (filter (lambda (sym) (enum-set-member? sym set2))
                    (enum-set->list set1)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Internal data structures and help functions; should not be exported.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define enumeration:type
  (make-record-type-descriptor 'enumeration #f #f #f #f
   '#((immutable universe)       ; thunk that returns universal set
      (immutable indexer)        ; maps objects to indexes
      (immutable constructor)    ; maps lists of symbols to sets
      (immutable deconstructor)  ; maps sets to lists of symbols
      )))

(define enumeration:rcd (make-record-constructor-descriptor enumeration:type #f #f))

(define enumeration:make-type (record-constructor enumeration:rcd))
(define enumeration:type-universe      (record-accessor enumeration:type 0))
(define enumeration:type-indexer       (record-accessor enumeration:type 1))
(define enumeration:type-constructor   (record-accessor enumeration:type 2))
(define enumeration:type-deconstructor (record-accessor enumeration:type 3))

(define enumeration:set
  (make-record-type-descriptor 'enum-set #f #f #f #f
   '#((immutable bits)           ; exact non-negative integer
      (immutable universe-type)  ; an enumeration:type
      )))

(define enumeration:set-rcd (make-record-constructor-descriptor enumeration:set #f #f))

(define enumeration:make-set (record-constructor enumeration:set-rcd))
(define enumeration:set-bits (record-accessor enumeration:set 0))
(define enumeration:set-type (record-accessor enumeration:set 1))

; Given:
; a canonical list of symbols, without duplicates
;
; Returns four values that can be used to implement a hashed
; mapping from symbols to their indexes in the canonical list:
;
; a vector, each of whose elements is #f or a symbol from the list
; a vector containing the indexes of the corresponding symbols
; an exact integer power of 2, used as a modulus to reduce
;     a symbol's hash code to an index in the first vector
; an exact integer bound on how far a symbol may be from its hashed
;     position in the first vector
;
; This help function is adapted from a prototype implementation
; of fast symbol dispatch in the Twobit compiler.

(define (enumeration:hash-table-components symbols)
  (let* ((n (length symbols))
         (bits (exact (floor (* 2 (log (+ n 1))))))
         (m (expt 2 bits))
         (mask (- m 1))
         (vec0 (make-vector (* 2 m) #f))
         (vec1 (make-vector (* 2 m) 0))
         ; the maximum distance between the hash index
         ; and the actual location of a symbol
         (maxdistance 0))

    ; vec0 and vec1 are larger than necessary
    (define (trimmed-vectors)
      (let* ((n (+ m maxdistance 1))
             (v0 (make-vector n #f))
             (v1 (make-vector n 0)))
        (do ((i 0 (+ i 1)))
            ((= i n)
             (values v0 v1))
          (vector-set! v0 i (vector-ref vec0 i))
          (vector-set! v1 i (vector-ref vec1 i)))))

    (do ((symbols symbols (cdr symbols))
         (i 0 (+ i 1)))
        ((null? symbols))
      (let ((sym (car symbols)))
        (let loop ((h (bitwise-and mask (symbol-hash sym)))
                   (d 0))
          (if (vector-ref vec0 h)
              (loop (+ h 1) (+ d 1))
              (begin
               (if (> d maxdistance)
                   (set! maxdistance d))
               (vector-set! vec0 h sym)
               (vector-set! vec1 h i))))))

    (call-with-values
     (lambda () (trimmed-vectors))
     (lambda (v0 v1)
       (values v0 v1 m maxdistance)))))
