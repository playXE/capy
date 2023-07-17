(select-module system.foreign)
    (export 
        dlopen
        dlclose
        dlsym
        pointer->procedure
        bytevector->pointer
        pointer->bytevector
        string->pointer
        pointer->string
        pointer?
        pointer-address
        make-pointer
        scheme->pointer
        pointer->scheme
        dereference-pointer
        size-of
        align-of
        null-pointer?
        bytevector-pointer-ref
        bytevector-pointer-set!
        uint8
        sint8
        uint16
        sint16
        uint32
        sint32
        uint64
        sint64
        void
        align
        write-c-struct
        read-c-struct
        make-c-struct)
    (define (null-pointer? ptr) (= (pointer-address ptr) 0))
    
    (define bytevector-pointer-ref
        (case (size-of '*)
            [(8) (lambda (bv offset)
                    (make-pointer (bytevector-u64-native-ref bv offset)))]
            [(4) (lambda (bv offset)
                    (make-pointer (bytevector-u32-native-ref bv offset)))]
            [else (error 'bytevector-pointer-ref "what machine is this?")]))
    
    (define bytevector-pointer-set!
        (case (size-of '*)
            [(8) (lambda (bv offset ptr)
                    (bytevector-u64-native-set! bv offset (pointer-address ptr)))]
            [(4) (lambda (bv offset ptr)
                    (bytevector-u32-native-set! bv offset (pointer-address ptr)))]
            [else (error 'bytevector-pointer-set! "what machine is this?")]))

    (define *writers* 
        `((,float . ,bytevector-f32-native-set!)
         (,double . ,bytevector-f64-native-set!)
         (,sint8 . ,bytevector-s8-set!)
         (,uint8 . ,bytevector-u8-set!)
         (,sint16 . ,bytevector-s16-native-set!)
         (,uint16 . ,bytevector-u16-native-set!)
         (,sint32 . ,bytevector-s32-native-set!)
         (,uint32 . ,bytevector-u32-native-set!)
         (,sint64 . ,bytevector-s64-native-set!)
         (,uint64 . ,bytevector-u64-native-set!)
         (* . ,bytevector-u64-native-set!)))

    (define *readers* 
        `((,float . ,bytevector-f32-native-ref)
         (,double . ,bytevector-f64-native-ref)
         (,sint8 . ,bytevector-s8-ref)
         (,uint8 . ,bytevector-u8-ref)
         (,sint16 . ,bytevector-s16-native-ref)
         (,uint16 . ,bytevector-u16-native-ref)
         (,sint32 . ,bytevector-s32-native-ref)
         (,uint32 . ,bytevector-u32-native-ref)
         (,sint64 . ,bytevector-s64-native-ref)
         (,uint64 . ,bytevector-u64-native-ref)
         (* . ,bytevector-u64-native-ref)))

    (define (align off alignment)
        (+ (bitwise-ior (- off 1) (- alignment 1)) 1))
    (define (write-c-struct bv offset types vals)
        (let lp ([offset offset] [types types] [vals vals])
            (cond 
                [(not (pair? types))
                    (or (null? vals) (error 'write-c-struct "too many values"))]
                [(not (pair? vals))
                    (error 'write-c-struct "too few values")]
                [else 
                    (let* ([type (car types)]
                           [offset (align offset (align-of type))])
                        (if (pair? type)
                            (write-c-struct bv offset (car types) (car vals))
                            (begin 
                                ((assv-ref *writers* type) bv offset (car vals))))
                            (lp (+ offset (size-of type)) (cdr types) (cdr vals)))])))
    
    (define (read-c-struct bv offset types)
        (let lp ([offset offset] [types types] [vals '()])
            (cond 
                [(not (pair? types))
                    (reverse vals)]
                [else 
                    (let* ([type (car types)]
                           [offset (align offset (align-of type))])
                        (lp (+ offset (size-of type)) (cdr types) (cons (if (pair? type)
                                (read-c-struct bv offset (car types))
                                ((assv-ref *readers* type) bv offset))
                                vals)))])))
    (define (make-c-struct types vals)
        (let ([bv (make-bytevector (size-of types) 0)])
            (write-c-struct bv 0 types vals)
            (bytevector->pointer bv)))
    (define (parse-c-struct foreign types)
        (let ((size (fold (lambda (type total)
                            (+ (size-of type)
                                (align total (align-of type))))
                            0
                            types)))
            (read-c-struct (pointer->bytevector foreign size) 0 types)))


(define-module system.foreign-library
    (import system.foreign)
    (export 

        load-foreign-library
        foreign-library-function
        foreign-library-pointer
        foreign-library-filename
        foreign-library?)

    (struct foreign-library (filename handle))
    (define system-library-extensions
        (cond 
            [(string-contains? (%host-os) "darwin")
                '(".dylib" ".so" ".bundle")]
            [(string-contains? (%host-os) "macos")
                '(".dylib" ".so" ".bundle")]
            [(string-contains? (%host-os) "linux")
                '(".so")]
            [(string-contains? (%host-os) "windows")
                '(".dll")]
            [else '(".so")]))

    (define (has-extension? head exts)
        (match exts
            [() #f]
            [(ext . exts)
                (or (string-contains? head ext) (has-extension? head exts))]))
    (define (file-exists-with-extension? head exts)
        (if (has-extension? head exts)
            (and (file-exists? head)
                (let lp ([exts exts])
                    (match exts
                        [() #f]
                        [(ext . exts)
                            (let ([head (string-append head ext)])
                                (if (file-exists? head)
                                    head
                                    (lp exts)))])))))

    (define (file-exists-in-path-with-extension basename path exts)
        (match path 
            [() #f]
            [(dir . path)
                (or (file-exists-with-extension (in-vicinity dir basename) exts)
                    (file-exists-in-path-with-extension basename path exts))]))

    (define path-separator
        (case (%host-os)
            [(windows) ";"]
            [else ":"]))

    (define (parse-path var)
        (match (getenv var)
            [#f #f]
            ["" '()]
            [val (string-split val path-separator)]))

    (define (load-foreign-library filename)
        (foreign-library filename (dlopen filename)))
    (define (foreign-library-pointer lib name)
        (let ([handle (foreign-library-handle lib)])
            (dlsym handle name)))
            
    (define (foreign-library-function lib name return-type arg-types)
        (let ([handle (foreign-library-handle lib)])
            (pointer->procedure return-type (dlsym handle name) arg-types))))