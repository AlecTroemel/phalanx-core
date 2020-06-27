(global lume (require :lib.lume))
(global machine (require :lib.fsm))
(global inspect (require :lib.inspect))
(global map-size 9)
(global BLACK "black")
(global WHITE "white")
(global UP "up")
(global DOWN "down")
(global LEFT "left")
(global RIGHT "right")


;; ----------------------------------------------
;; |           Immutable functions              |
;; |                                            |
;; ----------------------------------------------
(fn free-stones-count [color stone-map]
    "the number of remain stones (max of 9) looking at the stone-map"
    (- 9 (lume.reduce stone-map
                      #(+ $1 (lume.count (lume.filter $2 nil) #(= $1 color)))
                      0)))

(fn color-at [x y stone-map] (-?> stone-map (. x) (. y)))

(fn color-other [color]
    (match color
           WHITE BLACK
           BLACK WHITE
           _ nil))

(fn foreach-spot [stone-map iteratee]
    "call func(color, x,y) for each location on the stone map"
    (each [x line (pairs stone-map)]
          (each [y color (pairs line)]
                (iteratee color x y))))

(fn in-bounds [x y]
    (and (> x 0)
         (<= x map-size)
         (> y 0)
         (<= y map-size)
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
    (for [i 1 map-size] (tset temp-board i []))
    (army-tail x y temp-board))

(fn opposite [direction]
    (match direction
           LEFT RIGHT
           RIGHT LEFT
           UP DOWN
           DOWN UP))

(fn direction-iters [direction]
    (match direction
           LEFT (values #(- $1 1) #$1)
           RIGHT (values #(+ $1 1) #$1)
           UP (values #$1 #(- $1 1))
           DOWN (values #$1 #(+ $1 1))))

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
    (let [x-goal (if (= color BLACK) 1 9)
          y-goal (if (= color BLACK) 1 9)]
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
     (= 9 (free-stones-count BLACK board)) WHITE
     (= 9 (free-stones-count WHITE board)) BLACK
     ;; Win by touching the temple
     (touching-temple BLACK board) BLACK
     (touching-temple WHITE board) WHITE
     ;; game is STILL ON
     false))


;; ------------------------------------------------------|
;; |       Finite state machine & Global State           |
;; |                                                     |
;; ------------------------------------------------------|

(fn take-an-action [self]
    (tset self :current-turn-action-counter (- self.current-turn-action-counter 1)))

(var fms (machine.create {:army nil ;; used for limiting move actions
                          :current-turn BLACK
                          :current-turn-action-counter 2
                          :board {1 {}
                                  2 {2 WHITE 3 WHITE}
                                  3 {2 WHITE 3 WHITE}
                                  4 {}
                                  5 {}
                                  5 {}
                                  7 {7 BLACK 8 BLACK}
                                  8 {7 BLACK 8 BLACK}
                                  9 {}}
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
                          :callbacks {:onenter-selecting-action
                                      #(do
                                        (set self.board (remove-dead-stones self.board))
                                        (when (= self.current-turn-action-counter 0)
                                          (set self.current-turn (color-other self.current-turn))
                                          (set self.current-turn-action-counter 2))
                                        (let [winner (game-over self.board)]
                                          (: $1 :endgame winner)))

                                      :onbefore-add
                                      #(if (> (free-stones-count self.current-turn self.board) 0)
                                        (take-an-action $1)
                                        false)

                                      :onbefore-move #(when (= $3 :selecting-action) (take-an-action $1))

                                      ;; :onenter-placing-stone #(do (tset cursor :x 5) (tset cursor :y 5))

                                      :onbefore-pick
                                      (lambda [self _event _from _to coords]
                                        (let [{: x : y} coords]
                                          (if (= (color-at x y self.board) self.current-turn)
                                              (do
                                               (set self.board (remove-stone x y self.board))
                                               (tset self :army (army-at x y self.current-turn self.board)))
                                              false)))

                                      :onbefore-place
                                      (lambda [self event from to coords]
                                        (let [{: x : y } coords
                                              board (or (. self :army) self.board)]
                                          (if (is-possible-add x y self.current-turn board)
                                              (do
                                               (set self.board (place-stone x y self.current-turn self.board))
                                               (tset self :army nil))
                                              false)))

                                      :onbefore-lineup take-an-action

                                      :onbefore-push
                                      (lambda [self event from to coords]
                                        (let [{: x : y : direction} coords]
                                          (if (is-possible-push x y self.current-turn direction self.board)
                                              (set self.board (push x y self.current-turn direction self.board))
                                              false)))


                                      :onenter-game-over
                                      (lambda [self event from to winner]
                                        (print winner))}}))
