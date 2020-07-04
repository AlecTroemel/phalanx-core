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

(fn loc= [a b]
    (and (= a.x b.x)
         (= a.y b.y)))

(fn only [color board]
    "return the stones for the given color"
    (lume.filter board #(= (. $1 :color) color)))

(fn free-stones-count [color board]
    "the number of remain stones (max of 9) looking at the stone-map"
    (- 9 (lume.count (only color board))))

(fn stone-at [pos board]
    "returns  (values stone board-index)"
    (lume.match board (lambda [stone] (loc= stone pos))))

(fn color-at [pos board]
    "the color at the pos"
    (match (pick-values 1 (stone-at pos board))
           nil nil
           spot spot.color))

(fn other [color]
    "black to white and white to black"
    (match color
           col.WHITE col.BLACK
           col.BLACK col.WHITE
           _ nil))

(fn in-bounds [pos]
    "are the xy poss inside the bounds of the board (temples are out of bounds)"
    (and (> pos.x 0)
         (<= pos.x globals.map-size)
         (> pos.y 0)
         (<= pos.y globals.map-size)
         (not (loc= pos {:x 1 :y 1})) ;; exclude black temple
         (not (loc= pos {:x 9 :y 9})))) ;; exclude white temple

(fn neighbors-of [pos board]
    [{:x (+ pos.x 1) :y pos.y}
     {:x (- pos.x 1) :y pos.y}
     {:x pos.x :y (+ pos.y 1)}
     {:x pos.x :y (- pos.y 1)}])

(fn is-neighbor-of [pos-a pos-b board]
    "check if pos-a is a neighbor of pos b"
    (lume.any (neighbors-of pos-a board) #(loc= $1 pos-b)))

(fn valid-neighbors [pos color board]
    "return the orthogonal neighbors which are the desired color. passing nil for color will find open neighbors"
    (lume.filter (neighbors-of pos)
                 #(and (= color (color-at $1 board)
                        (in-bounds $1)))))

(fn opposite [direction]
    "the opposite orthogonal direction"
    (match direction
           dir.LEFT dir.RIGHT
           dir.RIGHT dir.LEFT
           dir.UP dir.DOWN
           dir.DOWN dir.UP))

(fn direction-iter [direction]
    "function to move a pos in a direction"
    (match direction
           dir.LEFT  #(lume.merge $1 {:x (- (. $1 :x) 1)})
           dir.RIGHT #(lume.merge $1 {:x (+ (. $1 :x) 1)})
           dir.UP    #(lume.merge $1 {:y (- (. $1 :y) 1)})
           dir.DOWN  #(lume.merge $1 {:y (+ (. $1 :y) 1)})))

(fn remove-stone [x y old-board]
    "return a new board with a stone removed at given poss"
    (lume.reject old-board (lambda [spot] (loc= spot {: x : y}))))

(fn place-stone [x y color old-board]
    "return a new board with a stone added given poss"
    (let [new-board (lume.deepclone old-board)]
      (table.insert new-board {: x : y : color})
      (lume.unique new-board)))

(fn place-stones [stones old-board]
    (let [new-board (lume.deepclone old-board)]
      (lume.each stones #(table.insert new-board $1))
      (lume.unique new-board)))

(fn remove-stones [stones old-board]
    "return a new board with all the stones given removed"
    (lume.unique (lume.reject old-board
                              (lambda [pos] (lume.any stones #(loc= $1 pos))))))

(fn get-starting-position [pos color direction board]
    "the furthest stone away from opponate on the push line"
    (let [opponate-color (other color)
          iter (direction-iter (opposite direction))] ;; NOTE: we need to look backwords
      (fn get-starting-position-tail [pos]
          (match (color-at (iter pos) board)
                 nil pos
                 opponate-color pos
                 color (get-starting-position-tail (iter pos))))
      (get-starting-position-tail pos)))

(fn is-possible-push [starting-pos color direction board]
    "check if pushing a line starting from a point for a color in a direction is valid"
    (let [starting-pos (get-starting-position starting-pos color direction board)
          iter (direction-iter direction)
          opponate-color (other color)]
      (fn is-possible-push-tail [pos ally-count opponate-count]
          (match (color-at pos board)
                 color (if (> opponate-count 0)
                           false ;; you've blocked yourself
                           (is-possible-push-tail (iter pos) (+ 1 ally-count) opponate-count))
                 opponate-color (is-possible-push-tail (iter pos) ally-count (+ 1 opponate-count))
                 nil (and (> opponate-count 0) ;; must actually touch opponate
                          (>  ally-count opponate-count))))  ;; must be longer then opponate
      (if (= color (color-at starting-pos board))
          (is-possible-push-tail starting-pos 0 0)
          false))) ;; must start on your own color

(fn possible-adds [color board]
    "possible locations for the add action for a color on a map.
     [{event:'add' x:2 y:3}]"
    (lume.reduce (only color board)
                 (lambda [acc stone]
                   (lume.each (valid-neighbors stone nil board)
                              (lambda [poss]
                                (when (not (lume.any acc #(loc= $1 poss)))
                                  (lume.push acc (lume.merge poss {:event "add"})))))
                   acc)
                 {}))

(fn possible-moves [color board]
    "possible moves for the board, a move consists of 'from' and 'to' locations
     [{event: 'move', from: {x y}, to: {x y}}]"
    (lume.reduce (only color board)
                 (lambda [acc from]
                   (lume.each (possible-adds color (remove-stone from.x from.y board))
                              (lambda [to] (lume.push acc {:event "move" : from : to})))
                   acc)))

(fn possible-pushes [color board]
    "list of all possible pushes for a color on the board
    [{event:'push' x:1 y:2 direction:'UP'}]"
    (let [board (only color board)
          pushes {}]
      (lume.each board
                 (lambda [stone]
                   (lume.each [dir.LEFT dir.RIGHT dir.UP dir.DOWN]
                              (lambda [direction]
                                (when (is-possible-push stone.x stone.y color direction board)
                                  (table.insert pushes (lume.extend stone {:direction direction :event "push"})))))))
      (lume.unique pushes)))

(fn is-possible-add [pos color board]
    "check if the pos and a color is a valid add move"
    (lume.any (possible-adds color board) #(loc= $1 pos)))

(fn army-at [pos color board]
    "find all connected pieces (an army) for a color starting at a position"
    (fn army-tail [pos seen-stones]
        (lume.each (valid-neighbors pos color board)
                   (lambda [stone]
                     (when (not (stone-at stone seen-stones))
                       (table.insert seen-stones (lume.merge stone {: color}))
                       (lume.concat seen-stones (army-tail stone seen-stones)))))
        seen-stones)
    (army-tail pos []))


(fn dead-stones [board]
    "list of all the dead/isolated stones on the board"
    (lume.filter board (lambda [stone]
                         (or (= 0 (# (valid-neighbors stone stone.color board)))
                             (not (in-bounds stone))))))

(fn push [starting-pos color direction old-board]
    "return a new board after a push action at the given pos and direction"
    (let [new-board (lume.deepclone old-board)
          start-pos (get-starting-position starting-pos color direction new-board)
          iter (direction-iter direction)]
      (fn push-line-tail [pos]
          (match (stone-at pos old-board)
                 (next-stone index) (do
                                     (tset new-board index (lume.merge pos {:color next-stone.color}))
                                     (when (~= nil next-stone.color)
                                       (push-line-tail (iter pos) next-stone.color)))))
      (push-line-tail starting-pos)
      (lume.unique new-board)))

(fn undo-push [starting-pos color direction old-board]
    "return a new board after a push action at the given pos and direction has been undone. This requires some special attention/tweaks"
    (let [new-board (lume.deepclone old-board)
          iter (direction-iter direction)
          starting-pos (get-starting-position (iter startin-pos) color direction new-board)
          opp-iter (direction-iter (opposite direction))]
      (fn pushnt-line-tail [pos board]
          (match (stone-at pos old-board)
                 (next-stone index) (do
                                     (tset new-board index (lume.merge (opp-iter pos) {:color next-stone.color}))
                                     (when (~= nil next-stone.color)
                                       (pushnt-line-tail (iter pos) next-stone.color)))))
      (pushnt-line-tail starting-pos)
      (lume.unique new-board)))

(fn goal-position [color]
    (match color
           col.BLACK {:x 1 :y 9}
           col.WHITE {:x 1 :y 9}))

(fn touching-temple [color board]
    "check if color is touching the temple"
    (lume.any board
              #(and (= (. $1 :color) color)
                    (is-neighbor-of $1 (goal-position color)))))

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
;; |       Finite State Machine & Global State           |
;; |      Imparative code below, enter with causion      |
;; |                                                     |
;; ------------------------------------------------------|

(fn take-an-action [self]
    (tset self.state :current-turn-action-counter (- self.state.current-turn-action-counter 1)))

(fn give-an-action [self]
    (tset self.state :current-turn-action-counter (+ self.state.current-turn-action-counter 1)))

(fn onenter-selecting-action [self]
    (tset self.state :army nil)
    (let [dead-stones (dead-stones self.state.board)]
      (when (> (# dead-stones) 0) (self:clean dead-stones)))
    (when (= self.state.current-turn-action-counter 0)
      (tset self.state :current-turn (other self.state.current-turn))
      (tset self.state :current-turn-action-counter 2)
      (self:clearHistory))
    (let [winner (game-over self.state.board)]
      (when winner (self:endgame winner))))

(fn onbefore-clean [self _event _from _to dead-stones]
    "an intermediate transition to put the removed stones into the history stack"
    (tset self.state :board (remove-stones dead-stones self.state.board)))

(fn onundo-clean [self _event _from _to reborn-stones]
    "an intermediate transition to revive dead stones from the history stack, undoes the next transition as well"
    (tset self.state :board (place-stones reborn-stones self.state.board))
    (self:undoTransition))

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
        (tset self.state :board (remove-stone x y self.state.board))
        false))

(fn onundo-pick [self _event _from _to x y]
    (tset self.state :board (place-stone x y self.state.current-turn self.state.board)))

(fn onenter-placing-stone [self _event _from _to x y]
    (when (= (type x) "number")
      (self:setarmy (army-at x y self.state.current-turn self.state.board))))

(fn onbefore-place [self _event _from _to x y]
    (let [board (or self.state.army self.state.board)]
      (if (is-possible-add x y self.state.current-turn board)
          (tset self.state :board (place-stone x y self.state.current-turn self.state.board))
          false)))

(fn onbefore-setarmy [self _event _from _to army]
    (tset self.state :army army))

(fn onundo-setarmy [self _event _from _to army]
    "an intermediate transition to revive past army state from the history stack, undoes the next transition as well"
    (tset self.state :army army)
    (self:undoTransition))

(fn onundo-place [self _event _from _to x y]
    (tset self.state :board (remove-stone x y self.state.board)))

(fn onbefore-lineup [self] (take-an-action self))
(fn onundo-lineup [self] (give-an-action self))

(fn onbefore-push [self _event _from _to x y direction]
    (if (is-possible-push x y self.state.current-turn direction self.state.board)
        (tset self.state :board (push x y self.state.current-turn direction self.state.board))
        false))

(fn onundo-push [self _even _from _to x y direction]
    (tset self.state :board (undo-push x y self.state.current-turn direction self.state.board)))

(fn onenter-game-over [self event from to winner]
    (print winner))

(fn init-board [?board ?turn]
    (machine.create {:state {:army nil ;; used for limiting move actions
                             :current-turn-action-counter 2
                             :current-turn (or ?turn col.BLACK)
                             :board (or ?board
                                        [{:x 2 :y 2 :color col.WHITE}
                                         {:x 2 :y 3 :color col.WHITE}
                                         {:x 3 :y 2 :color col.WHITE}
                                         {:x 3 :y 3 :color col.WHITE}
                                         {:x 7 :y 7 :color col.BLACK}
                                         {:x 7 :y 8 :color col.BLACK}
                                         {:x 8 :y 7 :color col.BLACK}
                                         {:x 8 :y 8 :color col.BLACK}])}
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
                              {:name "endgame" :from "selecting-action" :to "game-over"}
                              ;; remove dead stones (is an action so they are stored in history)
                              {:name "clean" :from "selecting-action" :to "selecting-action"}
                              ;; store army changes in history
                              {:name "setarmy" :from "placing-first-stone" :to "placing-first-stone"}
                              {:name "setarmy" :from "placing-second-stone" :to "placing-second-stone"}]
                     :callbacks {: onenter-selecting-action
                                 : onbefore-clean   : onundo-clean
                                 : onbefore-add     : onundo-add
                                 : onbefore-move    : onundo-move
                                 : onbefore-pick    : onundo-pick
                                 : onbefore-place   : onundo-place
                                 : onbefore-lineup  : onundo-lineup
                                 : onbefore-push    : onundo-push
                                 : onbefore-setarmy : onundo-setarmy
                                 :onenter-placing-first-stone onenter-placing-stone
                                 :onenter-placing-second-stone onenter-placing-stone
                                 : onenter-game-over}}))

{
 ;; used in views/game
 : init-board

 ;; used in ai
 : only
 : other
 : touching-temple
 : goal-position
 : possible-adds
 : possible-moves
 : possible-pushes
 : place-stone
 : remove-stone
 : push}
