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
(fn free-stones-count [color board]
    "the number of remain stones (max of 9) looking at the stone-map"
    (- 9 (lume.reduce board
                      (lambda [acc row]
                        (+ acc (lume.count (lume.filter row nil) #(= $1 color))))
                      0)))

(fn color-at [x y stone-map] (-?> stone-map (. x) (. y)))

(fn color-other [color]
    (match color
           col.WHITE col.BLACK
           col.BLACK col.WHITE
           _ nil))

(fn foreach-spot [stone-map iteratee]
    "call func(color, x,y) for each location on the stone map"
    (each [x line (pairs stone-map)]
          (each [y color (pairs line)]
                (iteratee color x y))))

(fn in-bounds [x y]
    (and (> x 0)
         (<= x globals.map-size)
         (> y 0)
         (<= y globals.map-size)
         (not (and (= x 1) (= y 1)))   ;; black temple
         (not (and (= x 9) (= y 9))))) ;; white temple

(fn find-neighbors [x y color stone-map]
    "return the orthogonal neighbors which are the desired color. passing nil for color will find open neighbors"
    (let [neighbors [{ :x (+ x 1) :y y}
                      { :x (- x 1) :y y}
                     { :x x :y (+ y 1)}
                     { :x x :y (- y 1)}]]
      (lume.filter neighbors #(and
                               (= color (color-at (. $1 "x") (. $1 "y") stone-map))
                               (in-bounds (. $1 "x")  (. $1 "y") )))))

(fn possible-adds [color stone-map]
    "possible locations for the add action for a color on a map"
    (local moves [])
    (foreach-spot stone-map
                  #(when (= $1 color)
                    (each [i spot (pairs (find-neighbors $2 $3 nil stone-map))]
                     (table.insert moves spot))))
    (lume.unique moves))

(fn is-possible-add [x y color stone-map]
    (lume.any (possible-adds color stone-map)
              #(and (= (. $1 :x) x)
                (= (. $1 :y) y))))

(fn army-at [x y color stone-map]
    "find all connected pieces (an army) for a color starting at a position, returns a board"
    (fn army-tail [x y seen]
        (each [i spot (pairs (find-neighbors x y color stone-map))]
              (when (~= color (color-at spot.x spot.y seen))
                (tset (. seen spot.x) spot.y color)
                (lume.merge seen (army-tail spot.x spot.y seen))))
        seen)
    (var temp-board [])
    (for [i 1 globals.map-size] (tset temp-board i []))
    (army-tail x y temp-board))

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

(fn get-starting-position [x y color direction stone-map]
    (let [opponate-color (color-other color)
                         (x-iter y-iter) (direction-iters (opposite direction))] ;; NOTE: we need to look backwords
      (fn get-starting-position-tail [x y]
          (match (color-at (x-iter x) (y-iter y) stone-map)
                 nil (values x y)
                 opponate-color (values x y)
                 color (get-starting-position-tail (x-iter x) (y-iter y))))
      (get-starting-position-tail x y)))

(fn is-possible-push [start-x start-y color direction stone-map]
    "check if pushing a line starting from a point for a color in a direction is valid"
    (let [(start-x start-y) (get-starting-position start-x start-y color direction stone-map)
          (x-iter y-iter) (direction-iters direction)
          opponate-color (color-other color)]
      (fn is-possible-push-tail [x y ally-count opponate-count]
          (match (color-at x y stone-map)
                 color (if (> opponate-count 0)
                           false ;; you've blocked yourself
                           (is-possible-push-tail (x-iter x) (y-iter y) (+ 1 ally-count) opponate-count))
                 opponate-color (is-possible-push-tail (x-iter x) (y-iter y) ally-count (+ 1 opponate-count))
                 nil (and (> opponate-count 0) ;; must actually touch opponate
                          (>  ally-count opponate-count))))  ;; must be longer then opponate
      (if (= color (color-at start-x start-y stone-map))
          (is-possible-push-tail start-x start-y 0 0)
          false))) ;; must start on your own color

(fn place-stone [x y color old-board]
    "return a new board with a stone added given coords"
    (let [new-board (lume.deepclone old-board)]
      (tset (. new-board x) y color) new-board))

(fn remove-stone [x y old-board]
    "return a new board with a stone removed at given coords"
    (let [new-board (lume.deepclone old-board)]
      (tset (. new-board x) y nil) new-board))

(fn push [x y color direction old-board]
    "return a new board after a push action at the given coords and direction"
    (let [new-board (lume.deepclone old-board)
                    (start-x start-y) (get-starting-position x y color direction new-board)
                    (x-iter y-iter) (direction-iters direction)]
      (fn push-line-tail [x y prev-color]
          (let [next-color (color-at x y new-board)]
            (tset (. new-board x) y prev-color)
            (when (~= nil next-color)
              (push-line-tail (x-iter x) (y-iter y) next-color))))
      (push-line-tail start-x start-y nil)
      new-board))


(fn remove-dead-stones [old-board]
    "return a board with all dead/isolated stones removed"
    (local new-board (lume.deepclone old-board))
    (foreach-spot old-board
                  (lambda [color x y]
                    (when (or (= 0 (# (find-neighbors x y color old-board)))
                              (not (in-bounds x y)))
                      (tset (. new-board x) y nil))))
    new-board)

(fn neighbor-of [x y x-goal y-goal]
    "check if (x,y) is a neighbor of (x-goal, y-goal)"
    (let [neighbors [{ :x (+ x 1) :y y}
                      { :x (- x 1) :y y}
                     { :x x :y (+ y 1)}
                     { :x x :y (- y 1)}]]
      (lume.any neighbors (lambda [spot]
                            (and (= x-goal (. spot :x))
                                 (= y-goal (. spot :y)))))))

(fn touching-temple [color board]
    "check if color is touching the temple"
    (let [x-goal (if (= color col.BLACK) 1 9)
          y-goal (if (= color col.BLACK) 1 9)]
      (var touching false)
      (foreach-spot board (lambda [c x y]
                            (when (and (= c color)
                                       (neighbor-of x y x-goal y-goal))
                              (set touching true))))
      touching))

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

(fn onenter-selecting-action [self]
    (tset self.state :board (remove-dead-stones self.state.board))
    (when (= self.state.current-turn-action-counter 0)
      (tset self.state :current-turn (color-other self.state.current-turn))
      (tset self.state :current-turn-action-counter 2))
    ;; (let [winner (game-over self.state.board)]
    ;;   (when winner (self.endgame winner)))
    )

(fn onbefore-add [self]
    (if (> (free-stones-count self.state.current-turn self.state.board) 0)
        (take-an-action self)
        false))

(fn onbefore-move [self _event from]
    (when (= from :selecting-action) (take-an-action self)))

(fn onbefore-pick [self _event _from _to coords]
    (let [{: x : y} coords]
      (if (= (color-at x y self.state.board) self.state.current-turn)
          (do
           (tset self.state :board (remove-stone x y self.state.board))
           (tset self.state :army (army-at x y self.state.current-turn self.state.board)))
          false)))

(fn onbefore-place [self _event _from _to coords]
    (let [{: x : y } coords
          board (or self.state.army self.state.board)]
      (if (is-possible-add x y self.state.current-turn board)
          (do
           (tset self.state :board (place-stone x y self.state.current-turn self.state.board))
           (tset self :army nil))
          false)))

(fn onbefore-push [self _event _from _to coords]
    (let [{: x : y : direction} coords]
      (if (is-possible-push x y self.state.current-turn direction self.state.board)
          (tset self.state :board (push x y self.state.current-turn direction self.state.board))
          false)))

(fn onenter-game-over [self event from to winner]
    (print winner))

;; output is the game
(fn init-board []
    (machine.create {:state {:army nil ;; used for limiting move actions
                             :current-turn col.BLACK
                             :current-turn-action-counter 2
                             :board {1 {}
                                       2 {2 col.WHITE 3 col.WHITE}
                                       3 {2 col.WHITE 3 col.WHITE}
                                       4 {}
                                       5 {}
                                       5 {}
                                       7 {7 col.BLACK 8 col.BLACK}
                                       8 {7 col.BLACK 8 col.BLACK}
                                       9 {}}}
                            ;; :functs {:free-stones-count (lambda [color]
                            ;;                               (free-stones-count color self.state.state.board))}
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
                                 : onbefore-add
                                 : onbefore-move
                                 : onbefore-pick
                                 : onbefore-place
                                 :onbefore-lineup take-an-action
                                 : onbefore-push
                                 : onenter-game-over}}))

{: init-board}
