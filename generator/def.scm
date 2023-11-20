
(dsl-run 
    `(
        (structs-file . ,(car (command-line-arguments)))
        (macros-file  . ,(cadr (command-line-arguments)))
    )

(lambda (begin-section end-section op op-group)
    (begin-section 'Bytecode
        '(
            (emit-in-structs-file . #t)
            (emit-in-macro-file . #t)
            (macro-name-component . "BYTECODE")))

    (op-group 'comparison
        '(
            eq?
            string=?
            string<=?
            string>=?
            string>?
            string<?
            char=?
            char<=?
            char>=?
            char>?
            char<?
            bytevector=?)
        '(
            (args 
                (dst VirtualRegister)
                (lhs VirtualRegister)
                (rhs VirtualRegister))))
    (op-group 'comparison-with-operand-types
        '(
            equal?
            eqv?
        )
        '(
            (args 
                (dst VirtualRegister)
                (lhs VirtualRegister)
                (rhs VirtualRegister)
                )))
    (op-group 'numerical-comparison
        '(
            <?
            <=?
            >?
            >=?
            =?)
        '(
            (args 
                (dst VirtualRegister)
                (lhs VirtualRegister)
                (rhs VirtualRegister))))
    (op 'type-of 
        '(
            (args 
                (dest VirtualRegister)
                (src  VirtualRegister))))
    (op 'mov
        '(
            (args 
                (dest VirtualRegister)
                (src  VirtualRegister))))
    (op 'mov-smi
        '(
            (args 
                (dest VirtualRegister)
                (imm  i32))))
    (op 'ret 
        '(
            (args 
                (src VirtualRegister))))
    (op 'call
        '(
            (args 
                (dst VirtualRegister)
                (callee VirtualRegister)
                (argc usize)
                (argv usize))))
    (op 'tail-call
        '(
            (args 
                (dst VirtualRegister)
                (callee VirtualRegister)
                (argc usize)
                (argv usize))))
    (op 'nop '())
    (op 'wide16 '())
    (op 'wide32 '())
    (op 'enter  '())
    (op-group 'numeric-type-predicates 
        '(
            fixnum?
            flonum?
            exact?
            inexact?
            number?
            real?
            complex?
            rational?
            integer?
            exact-integer? 
            exact-nonnegative-integer?
            exact-positive-integer?
            inexact-real?)
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'jmp 
        '(
            (args 
                (offset isize))))
    (op-group 'test-branches
        '(
            jsmi
            jnsmi
            jheapobject
            jnheapobject
            jtrue
            jfalse 
        )
        '(
            (args 
                (offset isize)
                (src VirtualRegister))))
    (op 'tag 
        '(
            (docs 
"Tags heap object with `tag`")
            (args 
                (srcdst VirtualRegister)
                (tag u16))))
    (op 'get-tag 
        '(
            (docs 
"Fetches the tag of a heap object as SMI, feature bits are preserved in SMI")
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'get-tag-type
        '(
            (docs 
"Fetches the tag of a heap object as SMI, feature bits are not preserved in SMI")
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'vectorlike? 
        '(
            (docs
"Checks if `src` is an heap-object that is vector-like, stores #t or #f in `dst`")
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'bytevectorlike?
        '(
            (docs
"Checks if `src` is an heap-object that is bytevector-like, stores #t or #f in `dst`")
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'jtagged
        '(
            (docs 
"Jump to `offset` if `src` is an heap-object with tag equal to `tag`
# Note
Does not check feature-bits of CellTag, only first 14 bits are checked")
            (args 
                (offset isize)
                (src VirtualRegister)
                (tag VirtualRegister))))
    (op 'jtagged-imm
        '(
            (docs 
"Jump to `offset` if `src` is an heap-object with tag equal to `tag`
# Note
Does not check feature-bits of CellTag, only first 14 bits are checked")
            (args 
                (offset isize)
                (src VirtualRegister)
                (tag u16))))
    (op 'jvectorlike 
        '(
            (docs 
"Jump to `offset` if `src` is an heap-object that is vector-like")
            (args 
                (offset isize)
                (src VirtualRegister))))
    (op 'jbytevectorlike 
        '(
            (docs 
"Jump to `offset` if `src` is an heap-object that is bytevector-like")
            (args 
                (offset isize)
                (src VirtualRegister))))
    (op 'switch-imm 
        '(
            (docs 
"Switch over SMI value in `src`.
`jtable` must be a vector of SMIs indicating an offset
to jump to, `default` is where we go if no entry in jtable is present"
            )
            (args 
                (offset isize)
                (src VirtualRegister)
                (jtable VirtualRegister)
                (default isize))))
    
     
    (op-group 'predicates
        '(
            pair?
            null?
            vector?
            string?
            symbol?
            tuple?
            procedure?
            boolean?
            char?
            undefined?
            unspecified?
            eof-object?)
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'scm-ref-word 
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister)
                (index VirtualRegister))))
    (op 'scm-set-word 
        '(
            (args 
                (dst VirtualRegister)
                (index VirtualRegister)
                (value VirtualRegister))))
    (op 'scm-ref-word-by-id 
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister)
                (index usize))))
    (op 'scm-set-word-by-id
        '(
            (args 
                (dst VirtualRegister)
                (index usize)
                (value VirtualRegister))))    
    (op 'scm-ref 
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister)
                (index VirtualRegister))))
    (op 'scm-set 
        '(
            (args 
                (dst VirtualRegister)
                (index VirtualRegister)
                (value VirtualRegister))))
    (op 'scm-ref-by-id 
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister)
                (index usize))))
    (op 'scm-set-by-id
        '(
            (args 
                (dst VirtualRegister)
                (index usize)
                (value VirtualRegister))))

    (op 'allocate 
        '(
            (args 
                (dst VirtualRegister)
                (size VirtualRegister)
                (tag u16))))
    (op 'allocate-known 
        '(
            (args 
                (dst VirtualRegister)
                (size usize)
                (tag u16))))
    (op 'allocate-untagged
        '(
            (args 
                (dst VirtualRegister)
                (size VirtualRegister))))
    (op 'allocate-untagged-known 
        '(
            (args 
                (dst VirtualRegister)
                (size usize))))
    (op 'smi? 
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'heap-object? 
        '(
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'unbox-flonum 
        '(
            (docs 
                "Unboxes heap object into flonum and stores it in `dst`")
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    (op 'box-flonum 
        '(
            (docs 
                "Takes flonum from stack and boxes it into a heap object")
            (args 
                (dst VirtualRegister)
                (src VirtualRegister))))
    
    (end-section 'Bytecode)
))
