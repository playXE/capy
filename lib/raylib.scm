(define-module raylib
    ; require "system/foreign" feature,
    ; if it is not loaded yet look it up
    (require "system/foreign")
    (require "raylib/raymath")
    ; import modules from the feature
    (import system.foreign)
    (import system.foreign-library)
    (import raylib.raymath)
    (export-all)

    (define %raylib (load-foreign-library "libraylib.so"))

    (struct color (r g b a) #:mutable #:constructor-name %color)

    (define (color r g b a)
        (%color r g b a))

    (define color-c-struct (list uint8 uint8 uint8 uint8))

    (define (color->c-struct this)
        (make-c-struct color-c-struct (list (color-r this) (color-g this) (color-b this) (color-a this))))
    (define (c-struct->color ptr)
        (parse-c-struct ptr color-c-struct))

    (define (color->list this)
        (list (color-r this) (color-g this) (color-b this) (color-a this)))
    
    (define (list->color lst)
        (color (car lst) (cadr lst) (caddr lst) (cadddr lst)))

    (struct rectangle (x y width height) #:mutable #:constructor-name %rectangle)

    (define (rectangle x y width height)
        (%rectangle x y width height))

    (define rectangle-c-struct (list float float float float))

    (define (rectangle->c-struct this)
        (make-c-struct rectangle-c-struct (list (rectangle-x this) (rectangle-y this) (rectangle-width this) (rectangle-height this))))

    (define (c-struct->rectangle ptr)
        (parse-c-struct ptr rectangle-c-struct))
    
    (define (rectangle->list this)
        (list (rectangle-x this) (rectangle-y this) (rectangle-width this) (rectangle-height this)))
    
    (define (list->rectangle lst)
        (rectangle (car lst) (cadr lst) (caddr lst) (cadddr lst)))
    

    (struct camera2d (offset target rotation zoom) #:mutable #:constructor-name %camera2d)
    (define (camera2d offset target rotation zoom)
        (%camera2d offset target rotation zoom))

    (define camera2d-c-struct (list vector2-c-struct vector2-c-struct float float))

    (define (camera2d->c-struct this)
        (make-c-struct camera2d-c-struct (list (camera2d-offset this) (camera2d-target this) (camera2d-rotation this) (camera2d-zoom this))))
    
    (define (c-struct->camera2d ptr)
        (parse-c-struct ptr camera2d-c-struct))
    
    (define (camera2d->list this)
        (list (camera2d-offset this) (camera2d-target this) (camera2d-rotation this) (camera2d-zoom this)))
    
    (define (list->camera2d lst)
        (camera2d (car lst) (cadr lst) (caddr lst) (cadddr lst)))

    (struct camera3d (position target up fovy projection) #:mutable #:constructor-name %camera3d)

    (define (camera3d position target up fovy projection)
        (if (not (and (vector3? position) (vector3? target) (vector3? up) (flonum? fovy) (foreign-library-function? projection)))
            (error 'camera3d "invalid camera3d")
            (%camera3d position target up fovy projection)))
        
    (define camera3d-c-struct (list vector3-c-struct vector3-c-struct vector3-c-struct float float sint32))
    
    (define (camera3d->c-struct this)
        (make-c-struct camera3d-c-struct (list (camera3d-position this) (camera3d-target this) (camera3d-up this) (camera3d-fovy this) (camera3d-projection this))))
    
    (define (c-struct->camera3d ptr)
        (parse-c-struct ptr camera3d-c-struct))
    
    (define (camera3d->list this)
        (list (camera3d-position this) (camera3d-target this) (camera3d-up this) (camera3d-fovy this) (camera3d-projection this)))
    
    (define (list->camera3d lst)
        (camera3d (car lst) (cadr lst) (caddr lst) (cadddr lst) (car (cddddr lst))))

    (define key-null 0)
    (define key-apostrophe 39)
    (define key-comma 44)
    (define key-minus 45)
    (define key-period 46)
    (define key-slash 47)
    (define key-0 48)
    (define key-1 49)
    (define key-2 50)
    (define key-3 51)
    (define key-4 52)
    (define key-5 53)
    (define key-6 54)
    (define key-7 55)
    (define key-8 56)
    (define key-9 57)
    (define key-semicolon 59)
    (define key-equal 61)
    (define key-a 65)
    (define key-b 66)
    (define key-c 67)
    (define key-d 68)
    (define key-e 69)
    (define key-f 70)
    (define key-g 71)
    (define key-h 72)
    (define key-i 73)
    (define key-j 74)
    (define key-k 75)
    (define key-l 76)
    (define key-m 77)
    (define key-n 78)
    (define key-o 79)
    (define key-p 80)
    (define key-q 81)
    (define key-r 82)
    (define key-s 83)
    (define key-t 84)
    (define key-u 85)
    (define key-v 86)
    (define key-w 87)
    (define key-x 88)
    (define key-y 89)
    (define key-z 90)
    (define key-left-bracket 91)
    (define key-backslash 92)
    (define key-right-bracket 93)
    (define key-grave 96)

    (define key-space 32)
    (define key-escape 256)
    (define key-enter 257)
    (define key-tab 258)
    (define key-backspace 259)
    (define key-insert 260)
    (define key-delete 261)
    (define key-right 262)
    (define key-left 263)
    (define key-down 264)
    (define key-up 265)
    (define key-page-up 266)
    (define key-page-down 267)
    (define key-home 268)
    (define key-end 269)
    (define key-caps-lock 280)
    (define key-scroll-lock 281)
    (define key-num-lock 282)
    (define key-prforeign-library-function-screen 283)
    (define key-pause 284)
    (define key-f1 290)
    (define key-f2 291)
    (define key-f3 292)
    (define key-f4 293)
    (define key-f5 294)
    (define key-f6 295)
    (define key-f7 296)
    (define key-f8 297)
    (define key-f9 298)
    (define key-f10 299)
    (define key-f11 300)
    (define key-f12 301)
    (define key-left-shift 340)
    (define key-left-control 341)
    (define key-left-alt 342)
    (define key-left-super 343)
    (define key-right-shift 344)
    (define key-right-control 345)
    (define key-right-alt 346)
    (define key-right-super 347)
    (define key-kb-menu 348)
    (define key-left-bracket 349)
    

    (define init-window 
        (let ([raw-init-window (foreign-library-function %raylib "InitWindow" void (list sint32 sint32 '*))])
            (lambda (width height title)
                (raw-init-window width height (string->pointer title)))))

    (define window-should-close?
        (let ([raw-window-should-close? (foreign-library-function %raylib "WindowShouldClose" sint8 '())])
            (lambda ()
                (not (= (raw-window-should-close?) 0)))))

    (define window-ready? 
        (let ([raw-window-ready? (foreign-library-function %raylib "IsWindowReady" sint8 '())])
            (lambda ()
                (= (raw-window-ready?) 1))))
    (define window-fullscreen? 
        (let ([raw-window-fullscreen? (foreign-library-function %raylib "IsWindowFullscreen" sint8 '())])
            (lambda ()
                (= (raw-window-fullscreen?) 1))))
    (define window-hidden? 
        (let ([raw-window-hidden? (foreign-library-function %raylib "IsWindowHidden" sint8 '())])
            (lambda ()
                (= (raw-window-hidden?) 1))))
    
    (define window-minimzed? 
        (let ([raw-window-minimized? (foreign-library-function %raylib "IsWindowMinimized" sint8 '())])
            (lambda ()
                (= (raw-window-minimized?) 1))))
    
    (define window-maximized? 
        (let ([raw-window-maximized? (foreign-library-function %raylib "IsWindowMaximized" sint8 '())])
            (lambda ()
                (= (raw-window-maximized?) 1))))
    
    (define window-focused? 
        (let ([raw-window-focused? (foreign-library-function %raylib "IsWindowFocused" sint8 '())])
            (lambda ()
                (= (raw-window-focused?) 1))))
    
    (define toggle-fullscreen (foreign-library-function %raylib "ToggleFullscreen" void '()))
    (define maximize-window (foreign-library-function %raylib "MaximizeWindow" void '()))
    (define minimize-window (foreign-library-function %raylib "MinimizeWindow" void '()))
    (define restore-window (foreign-library-function %raylib "RestoreWindow" void '()))
    (define set-window-title 
        (let ([raw-set-window-title (foreign-library-function %raylib "SetWindowTitle" void (list '*))])
            (lambda (title)
                (raw-set-window-title (string->pointer title)))))
    
    (define set-window-position (foreign-library-function %raylib "SetWindowPosition" void (list sint32 sint32)))
    (define set-window-monitor (foreign-library-function %raylib "SetWindowMonitor" void (list sint32)))
    (define set-window-min-size (foreign-library-function %raylib "SetWindowMinSize" void (list sint32 sint32)))
    (define set-window-size (foreign-library-function %raylib "SetWindowSize" void (list sint32 sint32)))
    (define set-window-opacity (foreign-library-function %raylib "SetWindowOpacity" void (list float)))
    (define get-screen-width (foreign-library-function %raylib "GetScreenWidth" sint32 '()))
    (define get-screen-height (foreign-library-function %raylib "GetScreenHeight" sint32 '()))
    (define get-render-width (foreign-library-function %raylib "GetRenderWidth" sint32 '()))
    (define get-render-height (foreign-library-function %raylib "GetRenderHeight" sint32 '()))
    (define get-monitor-count (foreign-library-function %raylib "GetMonitorCount" sint32 '()))
    (define get-monitor-position
        (let ([raw-get-monitor-position (foreign-library-function %raylib "GetMonitorPosition" vector2-c-struct (list sint32))])
            (lambda (monitor)
                (c-struct->vector2 (raw-get-monitor-position monitor)))))
            
    (define get-monitor-width (foreign-library-function %raylib "GetMonitorWidth" sint32 (list sint32)))
    (define get-monitor-height (foreign-library-function %raylib "GetMonitorHeight" sint32 (list sint32)))
    (define get-monitor-physical-width (foreign-library-function %raylib "GetMonitorPhysicalWidth" sint32 (list sint32)))
    (define get-monitor-physical-height (foreign-library-function %raylib "GetMonitorPhysicalHeight" sint32 (list sint32)))
    (define get-window-position 
        (let ([raw-get-window-position (foreign-library-function %raylib "GetWindowPosition" vector2-c-struct '())])
            (lambda ()
                (c-struct->vector2 (raw-get-window-position)))))
    
    (define get-window-scale-dpi 
        (let ([raw-get-window-scale-dpi (foreign-library-function %raylib "GetWindowScaleDPI" vector2-c-struct '())])
            (lambda ()
                (c-struct->vector2 (raw-get-window-scale-dpi)))))
    
    (define get-monitor-name 
        (let ([raw-get-monitor-name (foreign-library-function %raylib "GetMonitorName" '* (list sint32))])
            (lambda (monitor)
                (pointer->string (raw-get-monitor-name monitor)))))

    (define set-clipboard-text 
        (let ([raw-set-clipboard-text (foreign-library-function %raylib "SetClipboardText" void (list '*))])
            (lambda (text)
                (raw-set-clipboard-text (string->pointer text)))))
    
    (define get-clipboard-text
        (let ([raw-get-clipboard-text (foreign-library-function %raylib "GetClipboardText" '* '())])
            (lambda ()
                (pointer->string (raw-get-clipboard-text)))))

    (define show-cursor (foreign-library-function %raylib "ShowCursor" void '()))
    (define hide-cursor (foreign-library-function %raylib "HideCursor" void '()))
    (define is-cursor-hidden 
        (let ([raw-is-cursor-hidden (foreign-library-function %raylib "IsCursorHidden" sint8 '())])
            (lambda ()
                (= (raw-is-cursor-hidden) 1))))

    (define enable-cursor (foreign-library-function %raylib "EnableCursor" void '()))
    (define disable-cursor (foreign-library-function %raylib "DisableCursor" void '()))
    (define cursor-on-screen?
        (let ([raw-cursor-on-screen? (foreign-library-function %raylib "IsCursorOnScreen" sint8 '())])
            (lambda ()
                (= (raw-cursor-on-screen?) 1))))
    
    (define clear-background 
        (let ([raw-clear-background (foreign-library-function %raylib "ClearBackground" void (list color-c-struct))])
            (lambda (color)
                (raw-clear-background (color->c-struct color)))))
    
    (define begin-drawing (foreign-library-function %raylib "BeginDrawing" void '()))
    (define end-drawing (foreign-library-function %raylib "EndDrawing" void '()))
    (define begin-mode-2d 
        (let ([raw-begin-mode-2d (foreign-library-function %raylib "BeginMode2D" void (list camera2d-c-struct))])
            (lambda (camera)
                (raw-begin-mode-2d (camera2d->c-struct camera)))))
    (define end-mode-2d (foreign-library-function %raylib "EndMode2D" void '()))
    (define begin-mode-3d 
        (let ([raw-begin-mode-3d (foreign-library-function %raylib "BeginMode3D" void (list camera3d-c-struct))])
            (lambda (camera)
                (raw-begin-mode-3d (camera3d->c-struct camera)))))
    (define end-mode-3d (foreign-library-function %raylib "EndMode3D" void '()))
    (define set-target-fps (foreign-library-function %raylib "SetTargetFPS" void (list sint32)))
    (define get-fps (foreign-library-function %raylib "GetFPS" sint32 '()))
    (define get-frame-time (foreign-library-function %raylib "GetFrameTime" float '()))
    (define get-time (foreign-library-function %raylib "GetTime" double '()))
    (define get-random-value (foreign-library-function %raylib "GetRandomValue" sint32 (list sint32 sint32)))

    (define key-pressed? 
        (let ([raw-key-pressed? (foreign-library-function %raylib "IsKeyPressed" sint8 (list sint32))])
            (lambda (key)
                (= (raw-key-pressed? key) 1))))
    
    (define key-down? 
        (let ([raw-key-down? (foreign-library-function %raylib "IsKeyDown" sint8 (list sint32))])
            (lambda (key)
                (= (raw-key-down? key) 1))))
        
    (define key-released? 
        (let ([raw-key-released? (foreign-library-function %raylib "IsKeyReleased" sint8 (list sint32))])
            (lambda (key)
                (= (raw-key-released? key) 1))))
    
    (define key-up?
        (let ([raw-key-up? (foreign-library-function %raylib "IsKeyUp" sint8 (list sint32))])
            (lambda (key)
                (= (raw-key-up? key) 1))))
    
    (define set-exit-key (foreign-library-function %raylib "SetExitKey" void (list sint32)))
    (define get-key-pressed (foreign-library-function %raylib "GetKeyPressed" sint32 '()))
    (define get-char-pressed (foreign-library-function %raylib "GetCharPressed" sint32 '()))
    (define draw-pixel 
        (let ([raw-draw-pixel (foreign-library-function %raylib "DrawPixel" void (list sint32 sint32 color-c-struct))])
            (lambda (x y color)
                (raw-draw-pixel x y (color->c-struct color)))))

    (define draw-pixel-v 
        (let ([raw-draw-pixel-v (foreign-library-function %raylib "DrawPixelV" void (list vector2-c-struct color-c-struct))])
            (lambda (position color)
                (raw-draw-pixel-v (vector2->c-struct position) (color->c-struct color)))))

    (define draw-line 
        (let ([raw-draw-line (foreign-library-function %raylib "DrawLine" void (list sint32 sint32 sint32 sint32 color-c-struct))])
            (lambda (start-x start-y end-x end-y color)
                (raw-draw-line start-x start-y end-x end-y (color->c-struct color)))))

    (define draw-line-v
        (let ([raw-draw-line-v (foreign-library-function %raylib "DrawLineV" void (list vector2-c-struct vector2-c-struct color-c-struct))])
            (lambda (start-position end-position color)
                (raw-draw-line-v (vector2->c-struct start-position) (vector2->c-struct end-position) (color->c-struct color)))))
    
    (define draw-line-ex 
        (let ([raw-draw-line-ex (foreign-library-function %raylib "DrawLineEx" void (list vector2-c-struct vector2-c-struct float color-c-struct))])
            (lambda (start-position end-position thick color)
                (raw-draw-line-ex (vector2->c-struct start-position) (vector2->c-struct end-position) thick (color->c-struct color)))))
    
    (define draw-line-bezier 
        (let ([raw-draw-line-bezier (foreign-library-function %raylib "DrawLineBezier" void (list vector2-c-struct vector2-c-struct float color-c-struct))])
            (lambda (start-position end-position thick color)
                (raw-draw-line-bezier (vector2->c-struct start-position) (vector2->c-struct end-position) thick (color->c-struct color)))))
    
    (define draw-line-bezier-quad
        (let ([raw-draw-line-bezier-quad (foreign-library-function %raylib "DrawLineBezierQuad" void (list vector2-c-struct vector2-c-struct vector2-c-struct float color-c-struct))])
            (lambda (start-position end-position control-position thick color)
                (raw-draw-line-bezier-quad (vector2->c-struct start-position) (vector2->c-struct end-position) (vector2->c-struct control-position) thick (color->c-struct color)))))
    
    (define draw-line-bezier-cubic
        (let ([raw-draw-line-bezier-cubic (foreign-library-function %raylib "DrawLineBezierCubic" void (list vector2-c-struct vector2-c-struct vector2-c-struct vector2-c-struct float color-c-struct))])
            (lambda (start-position end-position control-position-1 control-position-2 thick color)
                (raw-draw-line-bezier-cubic (vector2->c-struct start-position) (vector2->c-struct end-position) (vector2->c-struct control-position-1) (vector2->c-struct control-position-2) thick (color->c-struct color)))))

    (define (draw-line-strip points color)
        (for-each (lambda (point) (draw-line-v point color)) points))

    (define draw-circle 
        (let ([raw-draw-circle (foreign-library-function %raylib "DrawCircle" void (list sint32 sint32 float color-c-struct))])
            (lambda (center-x center-y radius color)
                (raw-draw-circle center-x center-y radius (color->c-struct color)))))
    
    (define draw-circle-sector
        (let ([raw-draw-circle-sector (foreign-library-function %raylib "DrawCircleSector" void (list vector2-c-struct float float float sint32 color-c-struct))])
            (lambda (center-position radius start-angle end-angle segments color)
                (raw-draw-circle-sector (vector2->c-struct center-position) radius start-angle end-angle segments (color->c-struct color)))))
    (define draw-circle-sector-lines
        (let ([raw-draw-circle-sector-lines (foreign-library-function %raylib "DrawCircleSectorLines" void (list vector2-c-struct float float float sint32 color-c-struct))])
            (lambda (center-position radius start-angle end-angle segments color)
                (raw-draw-circle-sector-lines (vector2->c-struct center-position) radius start-angle end-angle segments (color->c-struct color)))))

    (define draw-circle-gradient
        (let ([raw-draw-circle-gradient (foreign-library-function %raylib "DrawCircleGradient" void (list sint32 sint32 float color-c-struct color-c-struct))])
            (lambda (center-x center-y radius inner-color outer-color)
                (raw-draw-circle-gradient center-x center-y radius (color->c-struct inner-color) (color->c-struct outer-color)))))
    
    (define draw-circle-v
        (let ([raw-draw-circle-v (foreign-library-function %raylib "DrawCircleV" void (list vector2-c-struct float color-c-struct))])
            (lambda (center-position radius color)
                (raw-draw-circle-v (vector2->c-struct center-position) radius (color->c-struct color)))))
    (define draw-circle-lines
        (let ([raw-draw-circle-lines (foreign-library-function %raylib "DrawCircleLines" void (list sint32 sint32 float color-c-struct))])
            (lambda (center-x center-y radius color)
                (raw-draw-circle-lines center-x center-y radius (color->c-struct color)))))

    (define draw-ellipse 
        (let ([raw-draw-ellipse (foreign-library-function %raylib "DrawEllipse" void (list sint32 sint32 float float color-c-struct))])
            (lambda (center-x center-y radius-x radius-y color)
                (raw-draw-ellipse center-x center-y radius-x radius-y (color->c-struct color)))))
    
    (define draw-ellipse-lines
        (let ([raw-draw-ellipse-lines (foreign-library-function %raylib "DrawEllipseLines" void (list sint32 sint32 float float color-c-struct))])
            (lambda (center-x center-y radius-x radius-y color)
                (raw-draw-ellipse-lines center-x center-y radius-x radius-y (color->c-struct color)))))
    
    (define draw-ring 
        (let ([raw-draw-ring (foreign-library-function %raylib "DrawRing" void (list vector2-c-struct float float sint32 sint32 color-c-struct))])
            (lambda (center-position inner-radius outer-radius start-angle end-angle segments color)
                (raw-draw-ring (vector2->c-struct center-position) inner-radius outer-radius start-angle end-angle segments (color->c-struct color)))))
    
    (define draw-ring-lines
        (let ([raw-draw-ring-lines (foreign-library-function %raylib "DrawRingLines" void (list vector2-c-struct float float sint32 sint32 color-c-struct))])
            (lambda (center-position inner-radius outer-radius start-angle end-angle segments color)
                (raw-draw-ring-lines (vector2->c-struct center-position) inner-radius outer-radius start-angle end-angle segments (color->c-struct color)))))
    
    (define draw-rectangle 
        (let ([raw-draw-rectangle (foreign-library-function %raylib "DrawRectangle" void (list sint32 sint32 sint32 sint32 color-c-struct))])
            (lambda (x y width height color)
                (raw-draw-rectangle x y width height (color->c-struct color)))))
    
    (define draw-rectangle-v
        (let ([raw-draw-rectangle-v (foreign-library-function %raylib "DrawRectangleV" void (list vector2-c-struct vector2-c-struct color-c-struct))])
            (lambda (position size color)
                (raw-draw-rectangle-v (vector2->c-struct position) (vector2->c-struct size) (color->c-struct color)))))
    
    (define draw-rectangle-rec 
        (let ([raw-draw-rectangle-rec (foreign-library-function %raylib "DrawRectangleRec" void (list rectangle-c-struct color-c-struct))])
            (lambda (rectangle color)
                (raw-draw-rectangle-rec (rectangle->c-struct rectangle) (color->c-struct color)))))
    
    (define draw-rectangle-pro 
        (let ([raw-draw-rectangle-pro (foreign-library-function %raylib "DrawRectanglePro" void (list rectangle-c-struct vector2-c-struct float color-c-struct))])
            (lambda (rectangle position origin rotation color)
                (raw-draw-rectangle-pro (rectangle->c-struct rectangle) (rectangle->c-struct rectangle) (vector2->c-struct origin) rotation (color->c-struct color)))))

    (define draw-rectangle-gradient-v 
        (let ([raw-draw-rectangle-gradient-v (foreign-library-function %raylib "DrawRectangleGradientV" void (list sint32 sint32 sint32 sint32 color-c-struct color-c-struct))])
            (lambda (x y width height color1 color2)
                (raw-draw-rectangle-gradient-v x y width height (color->c-struct color1) (color->c-struct color2)))))
    (define draw-rectangle-gradient-h 
        (let ([raw-draw-rectangle-gradient-h (foreign-library-function %raylib "DrawRectangleGradientH" void (list sint32 sint32 sint32 sint32 color-c-struct color-c-struct))])
            (lambda (x y width height color1 color2)
                (raw-draw-rectangle-gradient-h x y width height (color->c-struct color1) (color->c-struct color2)))))

    (define draw-rectangle-gradient-ex 
        (let ([raw-draw-rectangle-gradient-ex (foreign-library-function %raylib "DrawRectangleGradientEx" void (list rectangle-c-struct color-c-struct color-c-struct color-c-struct color-c-struct))])
            (lambda (rectangle color1 color2 color3 color4)
                (raw-draw-rectangle-gradient-ex (rectangle->c-struct rectangle) (color->c-struct color1) (color->c-struct color2) (color->c-struct color3) (color->c-struct color4)))))

    (define draw-rectangle-lines 
        (let ([raw-draw-rectangle-lines (foreign-library-function %raylib "DrawRectangleLines" void (list sint32 sint32 sint32 sint32 color-c-struct))])
            (lambda (x y width height color)
                (raw-draw-rectangle-lines x y width height (color->c-struct color)))))
    (define draw-rectangle-lines 
        (let ([raw-draw-rectangle-lines-ex (foreign-library-function %raylib "DrawRectangleLines" void (list rectangle-c-struct float color-c-struct))])
            (lambda (rectangle thick color)
                (raw-draw-rectangle-lines-ex (rectangle->c-struct rectangle) thick (color->c-struct color)))))

    (define draw-rectangle-rounded 
        (let ([raw-draw-rectangle-rounded (foreign-library-function %raylib "DrawRectangleRounded" void (list rectangle-c-struct float sint32 sint32 color-c-struct))])
            (lambda (rectangle roundness segments color)
                (raw-draw-rectangle-rounded (rectangle->c-struct rectangle) roundness segments (color->c-struct color)))))
    (define draw-rectangle-rounded-lines 
        (let ([raw-draw-rectangle-rounded-lines (foreign-library-function %raylib "DrawRectangleRoundedLines" void (list rectangle-c-struct float sint32 sint32 color-c-struct))])
            (lambda (rectangle roundness segments color)
                (raw-draw-rectangle-rounded-lines (rectangle->c-struct rectangle) roundness segments (color->c-struct color)))))
    
    (define lightgray (color 200 200 200 255))
    (define gray (color 130 130 130 255))
    (define darkgray (color 80 80 80 255))
    (define yellow (color 253 249 0 255))
    (define gold (color 255 203 0 255))
    (define orange (color 255 161 0 255))
    (define pink (color 255 109 194 255))
    (define red (color 230 41 55 255))
    (define maroon (color 190 33 55 255))
    (define green (color 0 228 48 255))
    (define lime (color 0 158 47 255))
    (define darkgreen (color 0 117 44 255))
    (define skyblue (color 102 191 255 255))
    (define blue (color 0 121 241 255))
    (define darkblue (color 0 82 172 255))
    (define purple (color 200 122 255 255))
    (define violet (color 135 60 190 255))
    (define darkpurple (color 112 31 126 255))
    (define beige (color 211 176 131 255))
    (define brown (color 127 106 79 255))
    (define darkbrown (color 76 63 47 255))

    (define white (color 255 255 255 255))
    (define black (color 0 0 0 255))
    (define blank (color 0 0 0 0))
    (define magenta (color 255 0 255 255))
    (define raywhite (color 245 245 245 255))


    ; rtext
    (define draw-fps (foreign-library-function %raylib "DrawFPS" void (list sint32 sint32)))
    (define draw-text 
        (let ([raw-draw-text (foreign-library-function %raylib "DrawText" void (list '* sint32 sint32 sint32 color-c-struct))])
            (lambda (text x y size color)
                (raw-draw-text (string->pointer text) x y size (color->c-struct color)))))
)