(define-module raylib.raymath
    (require "system/foreign")
    (import system.foreign)
    (import system.foreign-library)

    (export 
        vector2 
        vector2? 
        vector2-x 
        vector2-y
        set-vector2-x!
        set-vector2-y!
        vector2->c-struct
        c-struct->vector2
        vector2-c-struct
        vector2->list
        list->vector2
        vector3
        vector3?
        vector3-x
        vector3-y
        vector3-z
        set-vector3-x!
        set-vector3-y!
        set-vector3-z!
        vector3->c-struct
        c-struct->vector3
        vector3-c-struct
        vector3->list
        list->vector3
        vector4
        vector4?
        vector4-x
        vector4-y
        vector4-z
        vector4-w
        set-vector4-x!
        set-vector4-y!
        set-vector4-z!
        set-vector4-w!
        vector4->c-struct
        c-struct->vector4
        vector4-c-struct
        vector4->list
        list->vector4)

    (struct vector2 (x y) #:constructor-name %vector2 #:mutable)

    (define (vector2 x y)
        (if (not (and (number? x) (number? y)))
            (error 'vector2 "x and y must be numbers got ~a and ~a" x y)
            (%vector2 x y)))

    (define vector2-c-struct (list float float))

    (define (vector2->c-struct this)
        (make-c-struct vector2-c-struct (list (exact->inexact (vector2-x this)) (exact->inexact (vector2-y this)))))

    (define (c-struct->vector2 ptr)
        (parse-c-struct ptr vector2-c-struct))    

    (define (vector2->list this)
        (list (vector2-x this) (vector2-y this)))
    
    (define (list->vector2 lst)
        (vector2 (car lst) (cadr lst)))


    (struct vector3 (x y z) #:constructor-name %vector3 #:mutable)

    (define (vector3 x y z)
        (%vector3 x y z))

    (define vector3-c-struct (list float float float))

    (define (vector3->c-struct this)
        (make-c-struct vector3-c-struct (list (vector3-x this) (vector3-y this) (vector3-z this))))
    
    (define (c-struct->vector3 ptr)
        (parse-c-struct ptr vector3-c-struct))
    
    (define (vector3->list this)
        (list (vector3-x this) (vector3-y this) (vector3-z this)))

    (define (list->vector3 lst)
        (vector3 (car lst) (cadr lst) (caddr lst)))

    (struct vector4 (x y z w) #:constructor-name %vector4 #:mutable)

    (define (vector4 x y z w)
        (%vector4 x y z w))

    (define vector4-c-struct (list float float float float))

    (define (vector4->c-struct this)
        (make-c-struct vector4-c-struct (list (vector4-x this) (vector4-y this) (vector4-z this) (vector4-w this))))
    
    (define (c-struct->vector4 ptr)
        (parse-c-struct ptr vector4-c-struct))
    
    (define (vector4->list this)
        (list (vector4-x this) (vector4-y this) (vector4-z this) (vector4-w this)))
    
    (define (list->vector4 lst)
        (vector4 (car lst) (cadr lst) (caddr lst) (cadddr lst)))
)

    