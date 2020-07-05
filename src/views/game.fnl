;; ------------------------------------------------------|
;; |                  Graphics & Input                   |
;; |                                                     |
;; ------------------------------------------------------|
(include :lib.gfx)
(include :lib.globals)
(global lume (require :lib.lume))
(global inspect (require :lib.inspect))

(global col globals.col)
(global dir globals.dir)
(global phalanx (include :phalanx))
(global ai (include :ai))
(global player-color col.BLACK)

(var fsm nil)

(var cursor {:action 2 :pos {:x 5 :y 5} :direction dir.UP})

(fn init []
    (set fsm (phalanx.init-board)))

(fn update []
    (let [color (phalanx.other player-color)]
      (when (= color fsm.state.current-turn)
        (let [action (ai.pick-action color fsm.state.board)]
          (match action.event
                 "add" (do
                        (fsm:add)
                        (fsm:place action))
                 "move" (do
                         (fsm:move)
                         (fsm:pick action.from)
                         (fsm:place action.to))
                 "push" (do
                         (fsm:lineup)
                         (fsm:push action action.direction)))))))

(fn cursor-movement-handler [key event]
    (match key
           dir.RIGHT (tset cursor.pos :x (+ cursor.pos.x 1))
           dir.LEFT (tset cursor.pos :x (- cursor.pos.x 1))
           dir.UP (tset cursor.pos :y (- cursor.pos.y 1))
           dir.DOWN (tset cursor.pos :y (+ cursor.pos.y 1))
           "x" (: fsm event cursor.pos cursor.direction)
           "b" (: fsm :undoTransition)))

(fn direction-handler [key]
    (match key
           "w" (tset cursor :direction dir.UP)
           "s" (tset cursor :direction dir.DOWN)
           "a" (tset cursor :direction dir.LEFT)
           "d" (tset cursor :direction dir.RIGHT)))

(fn keypressed [key]
    (when (= player-color fsm.state.current-turn)
      (match fsm.current
           "selecting-action" (match key
                                     dir.RIGHT (tset cursor :action (+ cursor.action 1))
                                     dir.LEFT (tset cursor :action (- cursor.action 1))
                                     "x" (: fsm
                                            (match cursor.action 1 :add 2 :move 3 :lineup)
                                            cursor.pos cursor.direction)
                                     "b" (: fsm :undoTransition))
           "placing-stone" (cursor-movement-handler key :place)
           "picking-stone" (cursor-movement-handler key :pick)
           "lining-up-push" (do
                             (cursor-movement-handler key :push)
                             (direction-handler key))))
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
    (lume.each fsm.state.board
               (lambda [spot] (gfx.circle spot.color (* spot.x 20) (* spot.y 20) 9)))
    ;; temples
    (gfx.rect col.BLACK 10 10 20 20)
    (gfx.rect col.WHITE 170 170 20 20))

(fn draw-cursor []
    (gfx.circle fsm.state.current-turn (* cursor.pos.x 20) (* cursor.pos.y 20) 7))

(fn draw-ui []
    ;; (gfx.print (.. "White remaining: " (fsm.functs.free-stones-count col.WHITE))  200 10)
    ;; (gfx.print (.. "Black remaining: " (fsm.functs.free-stones-count col.BLACK))  200 30)
    (gfx.print (.. "Turn: " fsm.state.current-turn) 200 50)
    (gfx.print (.. "actions left: " fsm.state.current-turn-action-counter) 200 110)
    (gfx.print (.. "state: " fsm.current) 200 130)
    (each [i action (ipairs ["add" "move" "push"])]
          (let [x (+ 200 (* i 40))]
           (gfx.print action x 70)
            (when (= i cursor.action)
              (gfx.rect col.BLACK (- x 4) 69 39 20))))
    (match fsm.current
           "selecting-action" nil
           "placing-stone" (draw-cursor)
           "picking-stone" (draw-cursor)
           "lining-up-push" (do
                             (draw-cursor)
                             (gfx.print (.. "direction: " cursor.direction) 200 90))))

(fn draw []
    (draw-board)
    (draw-ui))


{: draw : init : update : keypressed}
