;;;===============================================================================
;;;
;;; MzScheme compatibility file:
;;;
;;; Uncomment the appropriate LOAD command in macros-core.scm
;;;
;;;===============================================================================

;; A number converted to string that uniquely identifies this run in the universe

(define (ex:unique-token) (number->string (current-seconds)))

;; The letrec black hole and corresponding setter.

(define ex:undefined (letrec ((x y) (y #f)) x))
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;; Just give this damn thing a binding

(define assertion-violation 
  (lambda args 
    (display 'assertion-violation)
    (newline)
    (for-each pretty-print args)
    (newline)
    (error)))

;; These are only partial implementations for specific use cases needed.
;; Full implementations should be provided by host implementation.

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

(define for-all andmap)

; filter already in MzScheme

;; Only the most minimal partial implementation of 
;; r6rs records as needed for our purposes.  
;; Note that most arguments are ignored.

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (call-with-values
      (lambda () (make-struct-type name #f (vector-length fields) 0))
    (lambda (type-descriptor
             full-constructor
             predicate
             generic-access
             generic-mutate)
      (list type-descriptor
            full-constructor
            predicate
            generic-access
            generic-mutate))))

(define (make-record-constructor-descriptor rtd parent-constructor-descriptor protocol)
  rtd)

(define (record-constructor cd) (cadr cd))
(define (record-predicate rtd)  (caddr rtd))
(define (record-accessor rtd k)
  (let ((generic-access (cadddr rtd)))
    (lambda (r) (generic-access r k))))



