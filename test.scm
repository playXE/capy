(require "system/foreign")
(import system.foreign-library)
(import system.foreign)
(import capy.foreign.internal)

(define raylib (load-foreign-library "libraylib.so"))

(define init-window 
    (let ([raw-init-window (foreign-library-function raylib "InitWindow" void (list sint32 sint32 '*))])
        (lambda (width height title)
            (raw-init-window width height (string->pointer title)))))
(define close-window (foreign-library-function raylib "CloseWindow" void '()))
(define window-should-close? 
    (let ([raw-window-should-close? (foreign-library-function raylib "WindowShouldClose" uint8 '())])
        (lambda ()
            (= (raw-window-should-close?) 1))))

(struct color (r g b a) #:mutable #:constructor-name make-color-raw)

(define (make-color r g b a)
    (match (list r g b a)
        [((? exact-integer? r) (? exact-integer? g) (? exact-integer? b) (? exact-integer? a))
            (make-color-raw r g b a)]
        [_ (error 'make-color "Invalid color arguments")]))

(define (color->raylib c)
    (if (not (color? c))
        (error 'color->raylib "Argument is not a color: ~a" c))
    (let ([bv (make-bytevector 4)])
        (bytevector-u8-set! bv 0 (color-r c))
        (bytevector-u8-set! bv 1 (color-g c))
        (bytevector-u8-set! bv 2 (color-b c))
        (bytevector-u8-set! bv 3 (color-a c))
        (bytevector->pointer bv)))

(define clear-background 
    (let ([raw-clear-background (foreign-library-function raylib "ClearBackground" void (list (list uint8 uint8 uint8 uint8)))])
        (lambda (c)
            (raw-clear-background (color->raylib c)))))

(define begin-drawing (foreign-library-function raylib "BeginDrawing" void '()))
(define end-drawing (foreign-library-function raylib "EndDrawing" void '()))
(define set-target-fps 
    (let ([raw-set-target-fps (foreign-library-function raylib "SetTargetFPS" void (list sint32))])
        (lambda (fps)
            (raw-set-target-fps fps))))

(define get-fps 
    (let ([raw-get-fps (foreign-library-function raylib "GetFPS" sint32 '())])
        (lambda ()
            (raw-get-fps))))

(define get-frame-time 
    (let ([raw-get-frame-time (foreign-library-function raylib "GetFrameTime" float '())])
        (lambda ()
            (raw-get-frame-time))))

(define get-time 
    (let ([raw-get-time (foreign-library-function raylib "GetTime" double '())])
        (lambda ()
            (raw-get-time))))

(define is-key-pressed? 
    (let ([raw-is-key-pressed? (foreign-library-function raylib "IsKeyPressed" uint8 (list sint32))])
        (lambda (key)
            (= (raw-is-key-pressed? key) 1))))

(define is-key-down? 
    (let ([raw-is-key-down? (foreign-library-function raylib "IsKeyDown" uint8 (list sint32))])
        (lambda (key)
            (= (raw-is-key-down? key) 1))))

(define is-key-released? 
    (let ([raw-is-key-released? (foreign-library-function raylib "IsKeyReleased" uint8 (list sint32))])
        (lambda (key)
            (= (raw-is-key-released? key) 1))))

(define is-key-up? 
    (let ([raw-is-key-up? (foreign-library-function raylib "IsKeyUp" uint8 (list sint32))])
        (lambda (key)
            (= (raw-is-key-up? key) 1))))

(define draw-pixel
    (let ([raw-draw-pixel (foreign-library-function raylib "DrawPixel" void (list sint32 sint32 (list uint8 uint8 uint8 uint8)))])
        (lambda (x y c)
            (raw-draw-pixel x y (color->raylib c)))))
(define draw-line 
    (let ([raw-draw-line (foreign-library-function raylib "DrawLine" void (list sint32 sint32 sint32 sint32 (list uint8 uint8 uint8 uint8)))])
        (lambda (x1 y1 x2 y2 c)
            (raw-draw-line x1 y1 x2 y2 (color->raylib c)))))

(define draw-circle 
    (let ([raw-draw-circle (foreign-library-function raylib "DrawCircle" void (list sint32 sint32 float (list uint8 uint8 uint8 uint8)))])
        (lambda (x y radius c)
            (raw-draw-circle x y radius (color->raylib c)))))

(define draw-rectangle 
    (let ([raw-draw-rectangle (foreign-library-function raylib "DrawRectangle" void (list sint32 sint32 sint32 sint32 (list uint8 uint8 uint8 uint8)))])
        (lambda (x y width height c)
            (raw-draw-rectangle x y width height (color->raylib c)))))

(init-window 800 450 "Hello World")
(display (not (window-should-close?)))
(set-target-fps 30)
(let loop ()
    (if (not (window-should-close?))
        (begin 
            (begin-drawing)
            (clear-background (make-color 245 245 245 255))   
            (draw-circle 200 200 32.0 (make-color 128 200 200 255))
            (end-drawing)
            (loop))))