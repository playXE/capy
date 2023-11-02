(define-syntax-rules let* ()
    [(let* ([var1 init1] [var init] ...) body ...)
        (let ([var1 init1])
            (let* ([var init] ...) body ...))]
    [(let* () body ...)
        (begin body ...)])


 (define-syntax-rules .finalize-quasiquote (quote unquote unquote-splicing)
    ((.finalize-quasiquote quote ?arg ?return)
     (.interpret-continuation ?return (quote ?arg)))
    ((.finalize-quasiquote unquote ?arg ?return)
     (.interpret-continuation ?return ?arg))
    ((.finalize-quasiquote unquote-splicing ?arg ?return)
     (syntax-error ",@ in illegal context" ?arg))
    ((.finalize-quasiquote ?mode ?arg ?return)
     (.interpret-continuation ?return (?mode . ?arg))))
 
 ; The first two "arguments" to .descend-quasiquote and to
 ; .descend-quasiquote-pair are always identical.
 
 (define-syntax-rules .descend-quasiquote (quasiquote unquote unquote-splicing)
    ((.descend-quasiquote `?y ?x ?level ?return)
     (.descend-quasiquote-pair ?x ?x (?level) ?return))
    ((.descend-quasiquote ,?y ?x () ?return)
     (.interpret-continuation ?return unquote ?y))
    ((.descend-quasiquote ,?y ?x (?level) ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote ,@?y ?x () ?return)
     (.interpret-continuation ?return unquote-splicing ?y))
    ((.descend-quasiquote ,@?y ?x (?level) ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote (?y . ?z) ?x ?level ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote #(?y ...) ?x ?level ?return)
     (.descend-quasiquote-vector ?x ?x ?level ?return))
    ((.descend-quasiquote ?y ?x ?level ?return)
     (.interpret-continuation ?return quote ?x)))
 
 (define-syntax-rules .descend-quasiquote-pair (quote unquote unquote-splicing)
    ((.descend-quasiquote-pair (?carx . ?cdrx) ?x ?level ?return)
     (.descend-quasiquote ?carx ?carx ?level (1 ?cdrx ?x ?level ?return))))
 
 (define-syntax-rules .descend-quasiquote-vector (quote)
    ((.descend-quasiquote-vector #(?y ...) ?x ?level ?return)
     (.descend-quasiquote (?y ...) (?y ...) ?level (6 ?x ?return))))
 
 ; Representations for continuations used here.
 ; Continuation types 0, 1, 2, and 6 take a mode and an expression.
 ; Continuation types -1, 3, 4, 5, and 7 take just an expression.
 ;
 ; (-1)
 ;     means no continuation
 ; (0)
 ;     means to call .finalize-quasiquote with no further continuation
 ; (1 ?cdrx ?x ?level ?return)
 ;     means a return from the call to .descend-quasiquote from
 ;     .descend-quasiquote-pair
 ; (2 ?car-mode ?car-arg ?x ?return)
 ;     means a return from the second call to .descend-quasiquote in
 ;     in Jonathan's code for .descend-quasiquote-pair
 ; (3 ?car-arg ?return)
 ;     means take the result and return an append of ?car-arg with it
 ; (4 ?cdr-mode ?cdr-arg ?return)
 ;     means take the result and call .finalize-quasiquote on ?cdr-mode
 ;     and ?cdr-arg with a continuation of type 5
 ; (5 ?car-result ?return)
 ;     means take the result and return a cons of ?car-result onto it
 ; (6 ?x ?return)
 ;     means a return from the call to .descend-quasiquote from
 ;     .descend-quasiquote-vector
 ; (7 ?return)
 ;     means take the result and return a call of list->vector on it
 
 (define-syntax-rules .interpret-continuation (quote unquote unquote-splicing)
    ((.interpret-continuation (-1) ?e) ?e)
    ((.interpret-continuation (0) ?mode ?arg)
     (.finalize-quasiquote ?mode ?arg (-1)))    
    ((.interpret-continuation (1 ?cdrx ?x ?level ?return) ?car-mode ?car-arg)
     (.descend-quasiquote ?cdrx
                          ?cdrx
                          ?level
                          (2 ?car-mode ?car-arg ?x ?return)))    
    ((.interpret-continuation (2 quote ?car-arg ?x ?return) quote ?cdr-arg)
     (.interpret-continuation ?return quote ?x))    
    ((.interpret-continuation (2 unquote-splicing ?car-arg ?x ?return) quote ())
     (.interpret-continuation ?return unquote ?car-arg))
    ((.interpret-continuation (2 unquote-splicing ?car-arg ?x ?return)
                              ?cdr-mode ?cdr-arg)
     (.finalize-quasiquote ?cdr-mode ?cdr-arg (3 ?car-arg ?return)))  
    ((.interpret-continuation (2 ?car-mode ?car-arg ?x ?return)
                              ?cdr-mode ?cdr-arg)
     (.finalize-quasiquote ?car-mode ?car-arg (4 ?cdr-mode ?cdr-arg ?return)))
      
    ((.interpret-continuation (3 ?car-arg ?return) ?e)
     (.interpret-continuation ?return append (?car-arg ?e)))
    ((.interpret-continuation (4 ?cdr-mode ?cdr-arg ?return) ?e1)
     (.finalize-quasiquote ?cdr-mode ?cdr-arg (5 ?e1 ?return)))
    ((.interpret-continuation (5 ?e1 ?return) ?e2)
     (.interpret-continuation ?return .cons (?e1 ?e2)))
    ((.interpret-continuation (6 ?x ?return) quote ?arg)
     (.interpret-continuation ?return quote ?x))
    ((.interpret-continuation (6 ?x ?return) ?mode ?arg)
     (.finalize-quasiquote ?mode ?arg (7 ?return)))
    ((.interpret-continuation (7 ?return) ?e)
     (.interpret-continuation ?return .list->vector (?e))))
 
 (define-syntax-rules quasiquote ()
    ((quasiquote ?x)
     (.descend-quasiquote ?x ?x () (0))))
