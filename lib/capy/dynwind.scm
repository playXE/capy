(select-module capy.internal)

(define parent-exception-handler (make-parameter #f))
(define-in-module capy (current-exception-handler)
  (%wind-up-raise undefined undefined))

;; Raises exception `c` to the current exception handler.
;; If handler is returned, raises exception to parent exception handler.
;; If there is no exception handlers, invokes `%scheme-error` to propagate error to the runtime and terminate execution
(define (%raise c)
  (cond 
    [(current-exception-handler) => 
      (lambda (eh)
        (eh c)
        (cond 
          [(parent-exception-handler) (proc (%make-error 'raise "returned from non-continuable exception ~a" c))])
          [else (%scheme-error "returned from non-continuable exception ~a" c)])]
    [else (%scheme-error "uncaught exception: ~a" (if (exn? c) (exn-message c) c))]))

(define (%error . args)
  (let ([err (apply %make-error args)])
    (%raise err)))


;; Used to invoke Scheme functions from Rust safely with respect to
;; exception handling and dynamic-wind.
;; We expect *only* Rust code to call this function and propagate errors appropriately.
;; Calling this function from Scheme works but is not safe and all dynamic-winds 
;; before the call will be ignored.
(define-in-module capy.internal (%rust->scheme-trampoline callback args)
  (let ([err? #f] [result #f])
    (set! result (call/cc (lambda (k)
      (with-exception-handler (lambda (c)
        (set! err? #t)
        (k c))
        (lambda ()
          (apply callback args))))))
      ; raises uncaught-able exception in case of errors. 
        
      (if err? (%scheme-raise result) result)))


(define-in-module capy error %error)
(define-in-module capy raise %raise)
(define-in-module capy with-exception-handler (lambda (handler thunk)
  (%wind-up undefined undefined handler)
  (let ([result (thunk)])
    (%wind-down)
    result)))
(define-in-module capy raise-continuable (lambda (c)
  (cond 
    [(current-exception-handler) => 
      (lambda (eh)
        (let ([res (eh c)])
          (%wind-down)
          res))]
    [else (%scheme-error "uncaught exception: ~a" (if (exn? c) (exn-message c) c))])))
(define (%dynamic-wind before during after)
  (before)
  (%wind-up before after)
  (let ((result (during)))
      ((cdr (%wind-down)))
      result))

(define-in-module capy make-error %make-error)

(define-in-module capy (call-with-current-continuation f)
  (%unprotected-call/cc (lambda (k)
     (f 
      (lambda (arg) 
        (do ([base (%dynamic-wind-base k)])
             [(eqv? (%dynamic-wind-current) base)]
             ((cdr (%wind-down))))
        (do ([winders (%dynamic-winders k) (cdr winders)])
             [(null? winders) (k arg)]
             [(car (car winders))]
             (%wind-up (car (car winders)) (cdr (car winders)))))))))
(define-in-module capy call/cc call-with-current-continuation)
(define-in-module capy dynamic-wind %dynamic-wind)
(define-in-module capy return #f)
(define-in-module capy (raise-user-error . args)
  (raise (apply %make-user-error args)))
(define-in-module capy (raise-type-error . args)
  (raise (apply %make-type-error args)))
(define-in-module capy (raise-argument-error . args)
  (raise (apply %make-argument-error args)))
(define-in-module capy (raise-result-error . args)
  (raise (apply %make-result-error args)))
(define-in-module capy (raise-arguments-error . args)
  (raise (apply %make-argument-error args)))
(define-in-module capy (raise-range-error . args)
  (raise (apply %make-range-error args)))

(define-in-module capy (raise-arity-error . args)
  (raise (apply %make-arity-error args)))
(define-in-module capy (raise-result-arity-error . args)
  (raise (apply %make-result-arity-error args)))
(define-in-module capy (assert-unreachable)
  (raise (apply %make-assert-unreachable)))


(select-module capy)
