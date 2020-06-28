;; ------------------------------------------------------|
;; |                  Graphics & Input                   |
;; |                                                     |
;; ------------------------------------------------------|
(include :lib.gfx)
(include :lib.globals)
(global inspect (require :lib.inspect))

(global col globals.col)
(global dir globals.dir)
(global phalanx (include :phalanx))

(var fms nil)

(var cursor {:action 2 :x 5 :y 5 :direction dir.UP})

(fn init []
    (set fms (phalanx.init-board)))

(fn update [] nil)

(fn cursor-movement-handler [key event]
    (match key
           dir.RIGHT (tset cursor :x (+ cursor.x 1))
           dir.LEFT (tset cursor :x (- cursor.x 1))
           dir.UP (tset cursor :y (- cursor.y 1))
           dir.DOWN (tset cursor :y (+ cursor.y 1))
           "x" (: fms event cursor)))

(fn direction-handler [key]
    (match key
           "w" (tset cursor :direction dir.UP)
           "s" (tset cursor :direction dir.DOWN)
           "a" (tset cursor :direction dir.LEFT)
           "d" (tset cursor :direction dir.RIGHT)))

(fn keypressed [key]
    (match fms.current
           "selecting-action" (match key
                                     dir.RIGHT (tset cursor :action (+ cursor.action 1))
                                     dir.LEFT (tset cursor :action (- cursor.action 1))
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
    (each [x line (pairs fms.state.board)]
          (each [y color (pairs line)]
                (gfx.circle color (* x 20) (* y 20) 9)))
    ;; temples
    (gfx.rect col.BLACK 10 10 20 20)
    (gfx.rect col.WHITE 170 170 20 20))

(fn draw-cursor []
    (gfx.circle fms.state.current-turn (* cursor.x 20) (* cursor.y 20) 7))

(fn draw-ui []
    ;; (gfx.print (.. "White remaining: " (fms.functs.free-stones-count col.WHITE))  200 10)
    ;; (gfx.print (.. "Black remaining: " (fms.functs.free-stones-count col.BLACK))  200 30)
    (gfx.print (.. "Turn: " fms.state.current-turn) 200 50)
    (gfx.print (.. "actions left: " fms.state.current-turn-action-counter) 200 110)
    (gfx.print (.. "state: " fms.current) 200 130)
    (each [i action (ipairs ["add" "move" "push"])]
          (let [x (+ 200 (* i 40))]
           (gfx.print action x 70)
            (when (= i cursor.action)
              (gfx.rect col.BLACK (- x 4) 69 39 20))))
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
