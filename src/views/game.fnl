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

;; ------------------------------------------------------|
;; |                    3D wireframe                     |
;; |                                                     |
;; | https://petercollingridge.appspot.com/3D-tutorial/rotating-objects |
;; ------------------------------------------------------|
(global NODE-SIZE 1)

(var BOARD-TILT 10)
(var board-rotate 0)
(var board-rotate-vel 0)
(var objects [])

(fn rotate-x [nodes theta]
    "rotate nodes around the x axis"
    (let [sin-t (math.sin theta)
          cos-t (math.cos theta)]
      (lume.map nodes (lambda [node]
                        (lume.merge node
                                    {:y (- (* node.y cos-t) (* node.z sin-t))
                                     :z (+ (* node.z cos-t) (* node.y sin-t))})))))

(fn rotate-y [nodes theta]
    "rotate nodes around the y axis"
    (let [sin-t (math.sin theta)
          cos-t (math.cos theta)]
      (lume.map nodes (lambda [node]
                        (lume.merge node
                                    {:x (- (* node.x cos-t) (* node.z sin-t))
                                     :z (+ (* node.z cos-t) (* node.x sin-t))})))))

(fn rotate-z [nodes theta]
    "rotate nodes around the z axis"
    (let [sin-t (math.sin theta)
          cos-t (math.cos theta)]
      (lume.map nodes (lambda [node]
                        (lume.merge node
                                    {:x (- (* node.x cos-t) (* node.y sin-t))
                                     :y (+ (* node.y cos-t) (* node.x sin-t))})))))

(fn create-cuboid [x y z w h d]
    (let [w (/ w 2)
          h (/ h 2)
          d (/ d 2)]
      {:nodes [{:x (- x w) :y (- y h) :z (- z d)}
               {:x (- x w) :y (- y h) :z (+ z d)}
               {:x (- x w) :y (+ y h) :z (- z d)}
               {:x (- x w) :y (+ y h) :z (+ z d)}
               {:x (+ x w) :y (- y h) :z (- z d)}
               {:x (+ x w) :y (- y h) :z (+ z d)}
               {:x (+ x w) :y (+ y h) :z (- z d)}
               {:x (+ x w) :y (+ y h) :z (+ z d)}]
       :edges [{:from 1 :to 2} {:from 2 :to 4} {:from 4 :to 3} {:from 3 :to 1}
               {:from 5 :to 6} {:from 6 :to 8} {:from 8 :to 7} {:from 7 :to 5}
               {:from 1 :to 5} {:from 2 :to 6} {:from 3 :to 7} {:from 4 :to 8}]}))

(fn create-grid [x y z count dist sep]
    (let [nodes []
          edges []]
      (for [i 1 (* count 4) 4]
           (let [sep (* (/ (- i 1) 4) sep)]
             (lume.push nodes
                        {:x x : y :z (+ z sep)}
                        {:x (+ x dist) : y  :z (+ z sep)}
                        {:x (+ x sep) : y :z z}
                        {:x (+ x sep) : y :z (+ z dist)}))
           (lume.push edges
                      {:from i :to (+ i 1)}
                      {:from (+ i 2) :to (+ i 3)}))
      {: nodes : edges}))

;; ------------------------------------------------------|
;; |               lets actually draw the game           |
;; |                                                     |
;; ------------------------------------------------------|

(fn init []
    (lume.push objects
               (create-cuboid 0 0 0 200 4 200) ;; board
               (create-cuboid -80 20 -80 10 30 10) ;; goal 1
               (create-cuboid 80 20 80 10 30 10) ;; goal 2
               (create-grid -80 5 -80 9 160 20))
    (set fsm (phalanx.init-board)))

(fn update []
    (set board-rotate-vel (if (love.keyboard.isDown "q") 0.05
                              (love.keyboard.isDown "e") -0.05
                              (/ board-rotate-vel 1.15)))
    (set board-rotate (+ board-rotate board-rotate-vel))
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
    (love.graphics.translate 200 120) ;; Translate to center of screen
    ;; the board
    (lume.each objects
               (lambda [obj]
                 (let [nodes (rotate-x (rotate-y obj.nodes board-rotate) BOARD-TILT)
                       edges obj.edges]
                   (lume.each nodes
                              (lambda [node] (gfx.circle "white" node.x node.y NODE-SIZE)))
                   (lume.each edges
                              (lambda [edge]
                                (let [node1 (. nodes edge.from)
                                      node2 (. nodes edge.to)]
                                  (gfx.line node1.x node1.y node2.x node2.y)))))))
    ;; the stones
    (let [nodes (lume.map fsm.state.board
                          (lambda [stone] {:x (- (* stone.x 20) 100)
                                           :y 10
                                           :z (- (* stone.y 20) 100)
                                           :color stone.color}))]
      (lume.each (rotate-x (rotate-y nodes board-rotate) BOARD-TILT)
                 (lambda [spot]
                   (gfx.circle spot.color spot.x spot.y 7))))
    (love.graphics.translate -200 -120)) ;; Translate back to normal coords

(fn draw-cursor []
    (let [pos {:x (- (* cursor.pos.x 20) 100)
               :y 10
               :z (- (* cursor.pos.y 20) 100)}
          pos (lume.first (rotate-x (rotate-y [pos] board-rotate) BOARD-TILT))]
      (love.graphics.translate 200 120)
      (gfx.circle fsm.state.current-turn pos.x pos.y 7)
      (love.graphics.translate -200 -120)
      ))

(fn draw-ui []
    ;; (gfx.print (.. "White remaining: " (fsm.functs.free-stones-count col.WHITE))  200 10)
    ;; (gfx.print (.. "Black remaining: " (fsm.functs.free-stones-count col.BLACK))  200 30)
    (gfx.print (.. "Turn: " fsm.state.current-turn) 20 200)
    (gfx.print (.. "actions left: " fsm.state.current-turn-action-counter) 20 220)
    (gfx.print (.. "state: " fsm.current) 20 10)
    (each [i action (ipairs ["add" "move" "push"])]
          (let [x (+ 200 (* i 40))]
           (gfx.print action x 200)
            (when (= i cursor.action)
              (gfx.rect col.BLACK (- x 4) 199 39 20))))
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
