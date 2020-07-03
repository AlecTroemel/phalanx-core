(include :lib.globals)
(global col globals.col)
(global dir globals.dir)

(global lume (require :lib.lume))
(global machine (require :lib.fsm))
(global inspect (require :lib.inspect))



;; ----------------------------------------------
;; |           Immutable functions              |
;; |                                            |
;; ----------------------------------------------

(fn only [color board]
    "return the stones for the given color"
    (lume.filter board #(= (. $1 :color) color)))

(fn free-stones-count [color board]
    "the number of remain stones (max of 9) looking at the stone-map"
    (- 9 (lume.count (only color board))))

(fn stone-at [x y board]
    "returns  (values stone board-index)"
    (lume.match board (lambda [stone]
                        (and (= stone.x x)
                             (= stone.y y)))))

(fn color-at [x y board]
    "the color at the coords"
    (match (pick-values 1 (stone-at x y board))
           nil nil
           spot spot.color))

(fn other [color]
    "black to white and white to black"
    (match color
           col.WHITE col.BLACK
           col.BLACK col.WHITE
           _ nil))

(fn in-bounds [coord]
    (let [{: x : y} coord]
      (and (> x 0)
           (<= x globals.map-size)
           (> y 0)
           (<= y globals.map-size)
           (not (and (= x 1) (= y 1)))    ;; exclude black temple
           (not (and (= x 9) (= y 9)))))) ;; exclude white temple

(fn find-neighbors [x y color board]
    "return the orthogonal neighbors which are the desired color. passing nil for color will find open neighbors"
    (let [neighbors [{ :x (+ x 1) :y y}
                     { :x (- x 1) :y y}
                     { :x x :y (+ y 1)}
                     { :x x :y (- y 1)}]]
      (lume.filter neighbors
                   (lambda [neighbor]
                     (and (= color (color-at neighbor.x neighbor.y board))
                          (in-bounds neighbor))))))

(fn possible-adds [color board]
    "possible locations for the add action for a color on a map"
    (local moves [])
    (lume.each
     (only color board)
     (lambda [stone]
       (lume.each (find-neighbors stone.x stone.y nil board)
                  #(table.insert moves $1))))
    (lume.unique moves))

(fn is-possible-add [x y color board]
    "check if the x y coords and a color is a valid add move"
    (lume.any (possible-adds color board)
              (lambda [stone]
                (and (= stone.x x) (= stone.y y)))))

(fn army-at [x y color board]
    "find all connected pieces (an army) for a color starting at a position"
    (fn army-tail [x y seen]
        (each [i spot (pairs (find-neighbors x y color board))]
              (when (~= color (color-at spot.x spot.y seen))
                (table.insert seen {:x spot.x :y spot.y :color color})
                (lume.merge seen (army-tail spot.x spot.y seen))))
        seen)
    (army-tail x y board))

(fn opposite [direction]
    (match direction
           dir.LEFT dir.RIGHT
           dir.RIGHT dir.LEFT
           dir.UP dir.DOWN
           dir.DOWN dir.UP))

(fn direction-iters [direction]
    (match direction
           dir.LEFT (values #(- $1 1) #$1)
           dir.RIGHT (values #(+ $1 1) #$1)
           dir.UP (values #$1 #(- $1 1))
           dir.DOWN (values #$1 #(+ $1 1))))

(fn get-starting-position [x y color direction board]
    (let [opponate-color (other color)
                         (x-iter y-iter) (direction-iters (opposite direction))] ;; NOTE: we need to look backwords
      (fn get-starting-position-tail [x y]
          (match (color-at (x-iter x) (y-iter y) board)
                 nil (values x y)
                 opponate-color (values x y)
                 color (get-starting-position-tail (x-iter x) (y-iter y))))
      (get-starting-position-tail x y)))

(fn is-possible-push [start-x start-y color direction board]
    "check if pushing a line starting from a point for a color in a direction is valid"
    (let [(start-x start-y) (get-starting-position start-x start-y color direction board)
          (x-iter y-iter) (direction-iters direction)
          opponate-color (other color)]
      (fn is-possible-push-tail [x y ally-count opponate-count]
          (match (color-at x y board)
                 color (if (> opponate-count 0)
                           false ;; you've blocked yourself
                           (is-possible-push-tail (x-iter x) (y-iter y) (+ 1 ally-count) opponate-count))
                 opponate-color (is-possible-push-tail (x-iter x) (y-iter y) ally-count (+ 1 opponate-count))
                 nil (and (> opponate-count 0) ;; must actually touch opponate
                          (>  ally-count opponate-count))))  ;; must be longer then opponate
      (if (= color (color-at start-x start-y board))
          (is-possible-push-tail start-x start-y 0 0)
          false))) ;; must start on your own color

(fn place-stone [x y color old-board]
    "return a new board with a stone added given coords"
    (let [new-board (lume.deepclone old-board)]
      (table.insert new-board {: x : y : color}) new-board))

(fn remove-stone [x y old-board]
    "return a new board with a stone removed at given coords"
    (lume.reject old-board (lambda [spot]
                             (and (= x spot.x)
                                  (= y spot.y)))))

(fn push [x y color direction old-board]
    "return a new board after a push action at the given coords and direction"
    (let [new-board (lume.deepclone old-board)
                    (start-x start-y) (get-starting-position x y color direction new-board)
                    (x-iter y-iter) (direction-iters direction)]
      (fn push-line-tail [x y board]
          (match (stone-at x y old-board)
                 (next-stone index) (do
                                     (tset new-board index {:x (x-iter x) :y (y-iter y) :color next-stone.color})
                                     (when (~= nil next-stone.color)
                                       (push-line-tail (x-iter x) (y-iter y) next-stone.color)))))
      (push-line-tail start-x start-y) new-board))

(fn remove-dead-stones [old-board]
    "return a board with all dead/isolated stones removed"
    (lume.reject old-board (lambda [stone]
                             (or (= 0 (# (find-neighbors stone.x stone.y stone.color old-board)))
                                 (not (in-bounds stone))))))

(fn neighbor-of [x y x-goal y-goal]
    "check if (x,y) is a neighbor of (x-goal, y-goal)"
    (let [neighbors [{ :x (+ x 1) :y y}
                     { :x (- x 1) :y y}
                     { :x x :y (+ y 1)}
                     { :x x :y (- y 1)}]]
      (lume.any neighbors (lambda [spot]
                            (and (= spot.x x-goal)
                                 (= spot.y y-goal))))))

(fn touching-temple [color board]
    "check if color is touching the temple"
    (let [x-goal (if (= color col.BLACK) 1 9)
          y-goal (if (= color col.BLACK) 1 9)]
      (lume.any board (lambda [spot]
                        (and (= spot.color color)
                             (neighbor-of spot.x spot.y x-goal y-goal))))))

(fn game-over [board]
    "return color that has won the game, else false"
    (if
     ;; Win by evisceration
     (= 9 (free-stones-count col.BLACK board)) col.WHITE
     (= 9 (free-stones-count col.WHITE board)) col.BLACK
     ;; Win by touching the temple
     (touching-temple col.BLACK board) col.BLACK
     (touching-temple col.WHITE board) col.WHITE
     ;; game is STILL ON
     false))

;; ------------------------------------------------------|
;; |       Finite state machine & Global State           |
;; |                                                     |
;; ------------------------------------------------------|

(fn take-an-action [self]
    (tset self.state :current-turn-action-counter (- self.state.current-turn-action-counter 1)))

(fn give-an-action [self]
    (tset self.state :current-turn-action-counter (+ self.state.current-turn-action-counter 1)))

(fn onenter-selecting-action [self]
    (tset self.state :board (remove-dead-stones self.state.board))
    (when (= self.state.current-turn-action-counter 0)
      (tset self.state :current-turn (other self.state.current-turn))
      (tset self.state :current-turn-action-counter 2)
      (self:clearHistory))
    ;; TODO: this is broken
    ;; (let [winner (game-over self.state.board)]
    ;;   (when winner (self.endgame winner)))
    )

(fn onbefore-add [self]
    (if (> (free-stones-count self.state.current-turn self.state.board) 0)
        (take-an-action self)
        false))

(fn onundo-add [self]
    (give-an-action self))

(fn onbefore-move [self _event from]
    (when (= from :selecting-action) (take-an-action self)))

(fn onundo-move [self]
    (give-an-action self))

(fn onbefore-pick [self _event _from _to x y]
    (if (= (color-at x y self.state.board) self.state.current-turn)
        (do
         (tset self.state :board (remove-stone x y self.state.board))
         (tset self.state :army (army-at x y self.state.current-turn self.state.board)))
        false))

(fn onundo-pick [self _event _from _to x y]
    (tset self.state :board (place-stone x y self.state.curent-turn self.state.board))
    ;; (tset self.state :army (army-at x y self.state.current-turn self.state.board)))
    )

(fn onbefore-place [self _event _from _to x y]
    (let [board (or self.state.army self.state.board)]
      (if (is-possible-add x y self.state.current-turn board)
          (do
           (tset self.state :board (place-stone x y self.state.current-turn self.state.board))
           (tset self :army nil))
          false)))

(fn onundo-place [self _event _from _to x y]
    (tset self.state :board (remove-stone x y self.state.board)))

(fn onbefore-lineup [self] (take-an-action self))

(fn onbefore-push [self _event _from _to x y direction]
    (if (is-possible-push x y self.state.current-turn direction self.state.board)
        (tset self.state :board (push x y self.state.current-turn direction self.state.board))
        false))

(fn onenter-game-over [self event from to winner]
    (print winner))

;; output is the game
(fn init-board []
    (machine.create {:state {:army nil ;; used for limiting move actions
                             :current-turn col.BLACK
                             :current-turn-action-counter 2
                             :board [{:x 2 :y 2 :color col.WHITE}
                                     {:x 2 :y 3 :color col.WHITE}
                                     {:x 3 :y 2 :color col.WHITE}
                                     {:x 3 :y 3 :color col.WHITE}
                                     {:x 7 :y 7 :color col.BLACK}
                                     {:x 7 :y 8 :color col.BLACK}
                                     {:x 8 :y 7 :color col.BLACK}
                                     {:x 8 :y 8 :color col.BLACK}]}
                     :initial "selecting-action"
                     :events [;; Move
                              {:name "move" :from "selecting-action" :to "picking-first-stone"}
                              {:name "pick" :from "picking-first-stone" :to "placing-first-stone"}
                              {:name "place" :from "placing-first-stone" :to "picking-second-stone"}
                              {:name "pick" :from "picking-second-stone" :to "placing-second-stone"}
                              ;; Add
                              {:name "add" :from "selecting-action" :to "placing-stone"}
                              ;; Add / Move
                              {:name "place" :from ["placing-stone" "placing-second-stone"] :to "selecting-action"}
                              ;; Push
                              {:name "lineup" :from "selecting-action" :to "picking-push-line"}
                              {:name "push" :from "picking-push-line" :to "selecting-action"}
                              ;; Gameover
                              {:name "endgame" :from "selecting-action" :to "game-over"}]
                     :callbacks {: onenter-selecting-action
                                 : onenter-game-over
                                 : onbefore-add : onundo-add
                                 : onbefore-move : onundo-move
                                 : onbefore-pick : onundo-pick
                                 : onbefore-place : onundo-place
                                 : onbefore-lineup
                                 : onbefore-push
                                   }}))

{: init-board}
