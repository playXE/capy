;;;===============================================================================
    ;;;
    ;;; Scheme48 compatibility file:
    ;;;
    ;;; Uncomment the appropriate LOAD command in examples.scm
    ;;;
    ;;;===============================================================================

;; The file examples.scm can be run in an image in which
;; the following have been opened:

; ,open (modify records      (prefix r:))
; ,open (modify record-types (prefix rt:))
; ,open handle
; ,open simple-signals
; ,open time
; ,open (modify pp (prefix pretty:))
; ,open util

;; A numeric string that uniquely identifies this run in the universe
;; Essential for avoiding name conflicts when compiling separately
;; This is very risky and should be replaced!

(define (ex:unique-token)
  (number->string (run-time)))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;; Just give this damn thing a binding

(define assertion-violation
  (lambda args
    (apply error 'asserion-violation args)))

(define pretty-print pretty:p)

;; These are only partial implementations for specific use cases needed.
;; Full implementations should be provided by host implementation.

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

; from util

;(define (filter p? lst)
;  (if (null? lst)
;      '()
;      (if (p? (car lst))
;          (cons (car lst)
;                (filter p? (cdr lst)))
;          (filter p? (cdr lst)))))

; from util

(define for-all every)

;(define (for-all proc l . ls)
; (or (null? l)
;      (and (apply proc (car l) (map car ls))
;           (apply for-all proc (cdr l) (map cdr ls)))))

;; The best we can do in r5rs is make these no-ops

(define (file-exists? fn)
  #f)

(define (delete-file fn)
  (values))

;; Only the most minimal extremely partial implementation
;; of  r6rs records as needed for our specific use cases.
;; Note that most arguments are ignored.

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (let ((field-names (map cadr (vector->list fields))))
    (list (rt:make-record-type name field-names)
          field-names)))

(define (record-predicate rtd)
  (rt:record-predicate (car rtd)))

(define (make-record-constructor-descriptor rtd parent-constructor-descriptor protocol)
  rtd)

(define (record-constructor cd)
  (rt:record-constructor (car cd) (cadr cd)))

(define (record-accessor rtd k)
  (lambda (r) (r:record-ref r (+ k 1))))

;; A rudimentary r6rs REPL.
;; Run it and then enter import commands, definitions,
;; libraries, programs as if you were in r6rs.
;; The examples in examples.scm can be run this
;; way without having to wrap them in (ex:repl '( --- ))
;; Exit to the usual scheme48 repl via (exit).

(define (r6rs)
  (newline)
  (display "r6rs> ")
  (let ((exp (read)))
    ;; Fixme - use with-handlers properly
    (report-errors-as-warnings
     (lambda ()
       (or (equal? exp '(exit))
           (ex:repl (list exp))))
     "R6RS-error:")
    (or (equal? exp '(exit))
        (r6rs))))
 