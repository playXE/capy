(require "raylib")
(import raylib)
(import raylib.raymath)

(define screen-width 800)
(define screen-height 450)
(define square-size 31)
(define snake-length 256)
(struct snake (x y width height dx dy color) #:mutable)
(struct food (x y width height active? color) #:mutable)

(let (
    [frames-counter 0]
    [game-over? #f]
    [pause? #f]
    [fruit (food 0 0 0 0 #f blank)]
    [snake 
        (let loop ([v (make-vector 256)] [i 0])
            (if (< i 256)
                (begin
                    (vector-set! v i (snake 0 0 0 0 0 0 blank))
                    (loop v (+ i 1))
                )
                v
            ))]
    [snake-position 
        (let loop ([v (make-vector 256)] [i 0])
            (if (< i 256)
                (begin
                    (vector-set! v i (vector2 0 0))
                    (loop v (+ i 1))
                )
                v))]
    [allow-move? #f]
    [offset (vector2 0 0)]
    [counter-tail 0])

    (define (init-game)
        (set! frames-counter 0)
        (set! game-over? #f)
        (set! pause? #f)
        (set! counter-tail 1)
        (set! allow-move? #f)

        (set-vector2-x! offset (quotient screen-width square-size))
        (set-vector2-y! offset (quotient screen-height square-size))

        (do ([i 0 (+ i 1)])
            [(= i snake-length) i]
            (let ([snake (vector-ref snake i)])
                (vector-set! snake-position i (vector2 0 0))
                (set-snake-x! snake (quotient (vector2-x offset) 2))
                (set-snake-y! snake (quotient (vector2-y offset) 2))
                (set-snake-width! snake square-size)
                (set-snake-height! snake square-size)
                (set-snake-dx! snake square-size)
                (set-snake-dy! snake 0)
                (if (= i 0)
                    (set-snake-color! snake darkblue)
                    (set-snake-color! snake blue))))
        
        (set-food-width! fruit square-size)
        (set-food-height! fruit square-size)
        (set-food-color! fruit skyblue)
        (set-food-active?! fruit #f))
    
    (define (update-game)
        (if (not game-over?) 
            (begin
                (if (key-pressed? key-p)
                    (set! pause? (not pause?)))
                (unless pause?

                    (let ([head (vector-ref snake 0)])
                        
                        (if (and (key-pressed? key-right)
                                (and (= (snake-dx head) 0)
                                    allow-move?))
                            (begin 
                                
                                (set-snake-dx! head square-size)
                                (set-snake-dy! head 0)
                                (set! allow-move? #f)))
                        
                        (if (and (key-pressed? key-left)
                            (and (= (snake-dx head) 0)
                                allow-move?))
                            (begin 
                               
                                (set-snake-dx! head (- square-size))
                                (set-snake-dy! head 0)
                                (set! allow-move? #f)))
                        (if (and (key-pressed? key-down)
                            (and (= (snake-dy head) 0)
                                allow-move?))
                            (begin 
                                
                                (set-snake-dx! head 0)
                                (set-snake-dy! head square-size)
                                (set! allow-move? #f)))  
                        (if (and (key-pressed? key-up)
                            (and (= (snake-dy head) 0)
                                allow-move?))
                            (begin 
                                
                                (set-snake-dx! head 0)
                                (set-snake-dy! head (- square-size))
                                (set! allow-move? #f)))

                        ; snake movement
                        (do ([i 0 (+ i 1)])
                            [(= i counter-tail) i]
                            (vector-set! snake-position i (vector2 (snake-x (vector-ref snake i)) (snake-y (vector-ref snake i)))))

                        (if (= (remainder frames-counter 5) 0)
                            (do ([i 0 (+ i 1)])
                                [(= i counter-tail) i]
                                    (if (= i 0)
                                        (begin 
                                            (set-snake-x! head (+ (snake-x head) (snake-dx head)))
                                            (set-snake-y! head (+ (snake-y head) (snake-dy head)))
                                           
                                            (set! allow-move? #t))
                                        (begin 
                                            (set-snake-x! (vector-ref snake i) (vector2-x (vector-ref snake-position (- i 1))))
                                            (set-snake-y! (vector-ref snake i) (vector2-y (vector-ref snake-position (- i 1))))))))      

                        ; check wall collision
                        (if (or (>= (snake-x head) (- screen-width (vector2-x offset)))
                                (or (>= (snake-y head) (- screen-height (vector2-y offset)))
                                    (or (< (snake-x head) 0)
                                        (< (snake-y head) 0))))
                            (set! game-over? #t))

                        ; check collision with yourself
                        (do ([i 1 (+ i 1)])
                            [(= i counter-tail) i]
                            (if (and 
                                (= (snake-x head) (snake-x (vector-ref snake i)))
                                (= (snake-y head) (snake-y (vector-ref snake i))))
                                (set! game-over? #t)))
                        

                        (if (not (food-active? fruit))
                            (begin (set-food-active?! fruit #t)
                           
                            (let* (
                                [offset-x (quotient (vector2-x offset) 2)]
                                [offset-y (quotient (vector2-y offset) 2)]
                               
                                [max-width (- (quotient screen-width square-size) 1)]
                                [max-height (- (quotient screen-height square-size) 1)]
                                [x (+ (* (get-random-value 0 max-width) square-size) offset-x)]
                                [y (+ (* (get-random-value 0 max-height) square-size) offset-y)])
                               
                                (set-food-x! fruit x)
                                (set-food-y! fruit y)
                                )))
                        
                        ; collision with fruit
                        (if
                            (and 
                                (and 
                                    (< (snake-x head) (+ (food-x fruit) (food-width fruit)))
                                    (> (+ (snake-x head) (snake-width head)) (food-x fruit)))
                                (and 
                                    (< (snake-y head) (+ (food-y fruit) (food-height fruit)))
                                    (> (+ (snake-y head) (snake-height head)) (food-y fruit))))
                            (begin
                                (set-snake-x! (vector-ref snake counter-tail) (vector2-x (vector-ref snake-position (- counter-tail 1))))
                                (set-snake-y! (vector-ref snake counter-tail) (vector2-y (vector-ref snake-position (- counter-tail 1))))
                                (set! counter-tail (+ counter-tail 1))
                                (set-food-active?! fruit #f)
                            ))
                        


                        (set! frames-counter (+ frames-counter 1))      


                    )
            ))
            
        (if (key-pressed? key-enter)
            (begin 
                (init-game)
                (set! game-over? #f)
            )
        ))    
    )

    (define (draw-game)
        (begin-drawing)
        (clear-background raywhite)
        ; draw GC percentage
        (let-values ([(unused available max) (gc-stats)])
            (let ([used (- max available)])
                (set-window-title (format "Snake! | GC: ~a% | FPS: ~a" (quotient (* used 100) max) (get-fps)))))
        (if (not game-over?)
            (begin (do ([i 0 (+ i 1)])
                [(= i (+ (quotient screen-width square-size) 1)) i]
                (let ([offset-x (quotient (vector2-x offset) 2)]
                      [offset-y (quotient (vector2-y offset) 2)]
                      [i*square-size (* i square-size)])
                    (draw-line-v (vector2 (+ i*square-size offset-x) offset-y) (vector2 (+ i*square-size offset-x) (- screen-height offset-y)) lightgray)))
            (do ([i 0 (+ i 1)])
                [(= i (+ (quotient screen-height square-size) 1)) i]
                (let ([offset-x (quotient (vector2-x offset) 2)]
                      [offset-y (quotient (vector2-y offset) 2)]
                      [i*square-size (* i square-size)])
                    (draw-line-v (vector2 offset-x (+ i*square-size offset-y)) (vector2 (- screen-width offset-x) (+ i*square-size offset-y)) lightgray)))
            
            ; draw snake
            (do ([i 0 (+ i 1)])
                [(= i counter-tail) i]
                (let ([snake (vector-ref snake i)])
                    (draw-rectangle (snake-x snake) (snake-y snake) (snake-width snake) (snake-height snake) (snake-color snake))))
                
            (draw-rectangle (food-x fruit) (food-y fruit) (food-width fruit) (food-height fruit) (food-color fruit)))
            (draw-text "PRESS [ENTER] TO PLAY AGAIN" 230 200 20 gray))
        (end-drawing)
    )

    (init-window screen-width screen-height "Snake!")
    (init-game)
    (set-target-fps 60)
    (let loop ()
        (if (not (window-should-close?))
            (begin 
                (update-game)
                (draw-game)
                (loop))
            (display "window-should-close?\n")))
        
)