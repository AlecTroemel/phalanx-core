;; ------------------------------------------------------|
;; |                  Graphics & Input                   |
;; |                                                     |
;; ------------------------------------------------------|
(include :lib.gfx)

(var cursor {:action 2 :x 5 :y 5 :direction UP})

(fn update [] nil)

(fn cursor-movement-handler [key event]
    (match key
           RIGHT (tset cursor :x (+ cursor.x 1))
           LEFT (tset cursor :x (- cursor.x 1))
           UP (tset cursor :y (- cursor.y 1))
           DOWN (tset cursor :y (+ cursor.y 1))
           "x" (: fms event)))

(fn direction-handler [key]
    (match key
           "w" (tset cursor :direction UP)
           "s" (tset cursor :direction DOWN)
           "a" (tset cursor :direction LEFT)
           "d" (tset cursor :direction RIGHT)))

(fn keypressed [key]
    (match fms.current
           "selecting-action" (match key
                                     RIGHT (tset cursor :action (+ cursor.action 1))
                                     LEFT (tset cursor :action (- cursor.action 1))
                                     "x" (: fms (match cursor.action 1 :add 2 :move 3 :lineup)))
           ;; add action
           "placing-stone" (cursor-movement-handler key :place)
           ;; move action
           "picking-first-stone" (cursor-movement-handler key :pick)
           "placing-first-stone" (cursor-movement-handler key :place)
           "picking-second-stone" (cursor-movement-handler key :pick)
           "placing-second-stone" (cursor-movement-handler key :place)
           ;; push action
           "picking-push-line" (do
                                (cursor-movement-handler key :push)
                                (direction-handler key)))
    (match cursor
           {:action 0} (tset cursor :action 3)
           {:action 4} (tset cursor :action 1)
           {:x 0} (tset cursor :x 1)
           {:x 10} (tset cursor :x 9)
           {:y 0} (tset cursor :y 1)
           {:y 10} (tset cursor :y 9)))

(fn draw-board []
    ;; the board
    (for [i 1 9]
         (let [j (* 20 i) ]
           (gfx.line 20 j 180 j)    ;; x lines
           (gfx.line j 20 j 180)))  ;; y lines
    ;; the stones currently on the board
    (foreach-spot stones-board #(gfx.circle $1 (* $2 20) (* $3 20) 9))
    ;; temples
    (gfx.rect BLACK 10 10 20 20)
    (gfx.rect WHITE 170 170 20 20))

(fn draw-cursor []
    (gfx.circle current-turn (* cursor.x 20) (* cursor.y 20) 7))

(fn draw-ui []
    (gfx.print (.. "White remaining: " (free-stones-count WHITE stones-board))  200 10)
    (gfx.print (.. "Black remaining: " (free-stones-count BLACK stones-board))  200 30)
    (gfx.print (.. "Turn: " current-turn) 200 50)
    (gfx.print (.. "actions left: " current-action-counter) 200 110)
    (gfx.print (.. "state: " fms.current) 200 130)
    (each [i action (ipairs ["add" "move" "push"])]
          (let [x (+ 200 (* i 40))]
           (gfx.print action x 70)
            (when (= i cursor.action)
              (gfx.rect BLACK (- x 4) 69 39 20))))
    (match fms.current
           "selecting-action" nil
           ;; add action
           "placing-stone" (draw-cursor)
           ;; move action
           "picking-first-stone" (draw-cursor)
           "placing-first-stone" (draw-cursor)
           "picking-second-stone" (draw-cursor)
           "placing-second-stone" (draw-cursor)
           ;; push action
           "picking-push-line" (do
                                (draw-cursor)
                                (gfx.print (.. "direction: " cursor.direction) 200 90))))

(fn draw []
    (draw-board)
    (draw-ui))


{: draw : init : update : keypressed}
