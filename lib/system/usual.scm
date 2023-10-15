(define-syntax do
    (syntax-rules ()
    ((do ((var init step ...) ...)
        (test expr ...)
            command ...)
     (letrec
        ((loop
            (lambda (var ...)
                (if test
                    (begin
                        (if #f #f)
                        expr ...)
                    (begin
                        command
                        ...
                        (loop (do "step" var step ...)
                        ...))))))
        (loop init ...)))
    ((do "step" x)
        x)
    ((do "step" x y)
        y)))
(define-syntax unless 
    (syntax-rules ()
        ((unless test-expr body-expr ...)
            (if (not test-expr)
                (begin body-expr ...)))))

(define-syntax when 
    (syntax-rules ()
        ((when test-expr body-expr ...)
            (if test-expr
                (begin body-expr ...)))))


(define-syntax and 
    (syntax-rules ()
        ((_) #t)
        ((_ a) a)
        ((_ a b ...) (if a (and b ...) #f))))

(define-syntax or 
    (syntax-rules ()
        ((_) #f)
        ((_ a) a)
        ((_ a b ...)
            (let ((temp a))
                (if temp temp (or b ...))))))

(define-syntax let*
    (syntax-rules ()
        ((let* () body1 body2 ...)
            (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
            body1 body2 ...)
            (let ((name1 val1))
                (let* ((name2 val2) ...)
                    body1 body2 ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind"
        (binding ...) () (begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     (let tmps body))
    ((let-values "bind" ((b0 e0)
        binding ...) tmps body)
    (let-values "mktmp" b0 e0 ()
        (binding ...) tmps body))
    ((let-values "mktmp" () e0 args
        bindings tmps body)
     (call-with-values
        (lambda () e0)
        (lambda args
            (let-values "bind"
                bindings tmps body))))
    ((let-values "mktmp" (a . b) e0 (arg ...)
        bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x)
        bindings (tmp ... (a x)) body))
    ((let-values "mktmp" a e0 (arg ...)
        bindings (tmp ...) body)
    (call-with-values
        (lambda () e0)
        (lambda (arg ... . x)
            (let-values "bind"
                bindings (tmp ... (a x)) body))))))

(define-syntax let*-values
    (syntax-rules ()
      ((let*-values () body0 body1 ...)
        (let () body0 body1 ...))
      ((let*-values (binding0 binding1 ...)
        body0 body1 ...)
       (let-values (binding0)
        (let*-values (binding1 ...)
            body0 body1 ...)))))
(define-syntax define-values
    (syntax-rules ()
        ((define-values () expr)
            (define dummy
                (call-with-values (lambda () expr)
                    (lambda args #f))))
        ((define-values (var) expr)
            (define var expr))
        ((define-values (var0 var1 ... varn) expr)
            (begin
                (define var0
                (call-with-values (lambda () expr)
                    list))
                (define var1
                    (let ((v (cadr var0)))
                    (set-cdr! var0 (cddr var0))
                        v)) ...
                (define varn
                    (let ((v (cadr var0)))
                (set! var0 (car var0))
                v))))
        ((define-values (var0 var1 ... . varn) expr)
            (begin
                (define var0
                (call-with-values (lambda () expr)
                    list))
                (define var1
                    (let ((v (cadr var0)))
                        (set-cdr! var0 (cddr var0))
                    v)) ...
                (define varn
                    (let ((v (cdr var0)))
                    (set! var0 (car var0))
                v))))
        ((define-values var expr)
            (define var
                (call-with-values (lambda () expr)
                    list)))))

(define-syntax cond
    (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
            (begin result1 result2 ...))
        ((cond (test => result))
            (let ((temp test))
                (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
            (let ((temp test))
                (if temp
                    (result temp)
                    (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
            (let ((temp test))
                (if temp
                    temp
                    (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
            (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
            clause1 clause2 ...)
            (if test
                (begin result1 result2 ...)
                (cond clause1 clause2 ...)))))


(define-syntax case
    (syntax-rules (else =>)
    ((case (key ...)
        clauses ...)
        (let ((atom-key (key ...)))
            (case atom-key clauses ...)))

    ((case key
        (else => result))
            (result key))
    ((case key
        (else result1 result2 ...))
        (begin result1 result2 ...))
    ((case key
        ((atoms ...) result1 result2 ...))
        (if (memv key '(atoms ...))
            (begin result1 result2 ...)))
    ((case key
        ((atoms ...) => result))
            (if (memv key '(atoms ...))
                (result key)))
    ((case key
        ((atoms ...) => result)
        clause clauses ...)
        (if (memv key '(atoms ...))
            (result key)
            (case key clause clauses ...)))
    ((case key
        ((atoms ...) result1 result2 ...)
        clause clauses ...)
        (if (memv key '(atoms ...))
            (begin result1 result2 ...)
            (case key clause clauses ...)))))

;; in order to be fully compliant quasiquote shouldn't cons unless
;; necessary, and punt to the implementation of quote, however I do
;; not do this for simplicity

(define-syntax quasiquote
  (syntax-rules ()
    ((quasiquote expr)
     (quasiquote-helper expr ()))))

(define-syntax quasiquote-helper
  (syntax-rules (quasiquote unquote unquote-splicing)
    ((quasiquote-helper (quasiquote expr) stack)
     (list (quote quasiquote)
           (quasiquote-helper expr (#f . stack))))
    ((quasiquote-helper (unquote expr) ())
     expr)
    ((quasiquote-helper (unquote expr) (_ . rest))
     (list (quote unquote)
           ;; quasiquote-helper here so that multiple argument unquote
           ;; or unquote-splicing gives an error
           (quasiquote-helper expr rest)))
    ((quasiquote-helper (unquote exprs ...) stack)
     (syntax-violation 'unquote
                       "Multiple arguments to unquote only allowed in a list or vector"
                       (unquote exprs ...)))
    ((quasiquote-helper (unquote-splicing exprs ...) stack)
      (syntax-violation 'unquote-splicing
                       "unquote-splicing forms only allowed in a list or vector"
                       (unquote-splicing exprs ...)))
    ((quasiquote-helper (car . cdr) stack)
     (list-helper (car . cdr) stack))
    ((quasiquote-helper expr stack)
     (quote expr))))

(define-syntax list-helper
  (syntax-rules (quasiquote unquote unquote-splicing)
    ;; Single argument unquote needs to be handled as it can appear at
    ;; the end of a list, however multiple arguments and unquote
    ;; splicing do not, as there is no outer list to splice into
    ((list-helper (unquote expr) ())
     expr)
    ((list-helper (unquote expr) (_ . rest))
     (list (quote unquote)
           (quasiquote-helper expr rest)))

    ((list-helper ((quasiquote expr) . cdr) stack)
     (cons (quasiquote-helper (quasiquote expr) stack)
           (list-helper cdr stack)))
    ;; unquote & unquote splicing in cars need to be handled here in
    ;; order to splice correctly
    ((list-helper ((unquote exprs ...) . cdr) ())
     (append (list exprs ...)
             (list-helper cdr ())))
    ((list-helper ((unquote . exprs) . cdr) (first . rest))
     ;; needs to use list-helper on exprs, so that we can splice into
     ;; unquote/unquote-splicing forms 
     (cons (cons (quote unquote) (list-helper exprs rest))
           (list-helper cdr (first . rest))))
    ((list-helper ((unquote-splicing exprs ...) . cdr) ())
     (append exprs ...
             (list-helper cdr ())))
    ((list-helper ((unquote-splicing . exprs) . cdr) (first . rest))
     (cons (cons (quote unquote-splicing) (list-helper exprs rest))
           (list-helper cdr (first . rest)))) ;; right?
    ;; otherwise just make sure each list element gets deal with at
    ;; the correct stack level
    ((list-helper (car . cdr) ())
     (cons (quasiquote-helper car ())
           (list-helper cdr ())))
    ((list-helper (car . cdr) (first . rest))
     (cons (quasiquote-helper car rest)
           (list-helper cdr (first . rest))))
    ((list-helper () stack)
     '())))

