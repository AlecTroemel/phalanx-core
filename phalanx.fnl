(include :lib.gfx)
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

(fn find-neighbors [x y color stone-map]
    "return the orthogonal neighbors which are the desired color. passing nil for color will find open neighbors"
    (let [neighbors [{ :x (+ x 1) :y y}
                     { :x (- x 1) :y y}
                     { :x x :y (+ y 1)}
                     { :x x :y (- y 1)}]]
      (lume.filter neighbors #(and
                               (= color (color-at (. $1 "x") (. $1 "y") stone-map))
                               (> (. $1 "x") 0)
                               (<= (. $1 "x") map-size)
                               (> (. $1 "y") 0)
                               (<= (. $1 "y") map-size)))))

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

;; ------------------------------------------------------|
;; |       Finite state machine & Global State           |
;; |                                                     |
;; ------------------------------------------------------|
(local stones-board []) ;; nil, 0=white, 1=black
(var current-turn BLACK) ;; or WHITE
(var current-action-counter 2) ;; 2 actions per turn
(var cursor {:action 2 :x 5 :y 5 :direction UP})

(fn init-stones []
    ;; initialize each row of board
    (for [i 1 map-size] (tset stones-board i []))
    ;; black & white starting positions
    (tset stones-board 2 {1 WHITE 2 WHITE 3 WHITE})
    (tset stones-board 3 {2 WHITE 3 WHITE})
    (tset stones-board 7 {7 BLACK 8 BLACK})
    (tset stones-board 8 {7 BLACK 8 BLACK}))

(fn init []
    (init-stones)
    (set current-turn BLACK)
    (set current-action-counter 2))

(fn on-enter-selecting-action []
    (tset cursor :action 1)
    (when (= current-action-counter 0)
      (set current-turn (color-other current-turn))
      (set current-action-counter 2)))

(fn on-before-add []
    (if (> (free-stones-count current-turn stones-board) 0)
        (set current-action-counter (- current-action-counter 1))
        false))

(fn on-before-move [self event from]
    (when (= from :selecting-action)
      (set current-action-counter (- current-action-counter 1))))

(fn on-enter-placing-stone []
    (tset cursor :x 5)
    (tset cursor :y 5))

(fn on-before-pick []
    (if (= (color-at cursor.x cursor.y stones-board) current-turn)
        (do
         ;; remove stone
         (tset (. stones-board cursor.x) cursor.y nil)
         (tset cursor :army (army-at cursor.x cursor.y current-turn stones-board)))
        false))

(fn on-before-place []
    (if (is-possible-add cursor.x cursor.y current-turn (or (. cursor :army) stones-board))
        (do
         ;; add stone
         (tset (. stones-board cursor.x) cursor.y current-turn)
         (tset cursor :army nil))
        false))

(fn on-before-lineup []
    (set current-action-counter (- current-action-counter 1)))

(fn on-before-push []
    (if (is-possible-push cursor.x cursor.y current-turn cursor.direction stones-board)
        ;; Push stones
        (let [(start-x start-y) (get-starting-position cursor.x cursor.y current-turn cursor.direction stones-board)
              (x-iter y-iter) (direction-iters cursor.direction)]
          (fn push-line-tail [x y prev-color]
              (let [next-color (color-at x y stones-board)]
                (tset (. stones-board x) y prev-color)
                (when (~= nil next-color)
                  (push-line-tail (x-iter x) (y-iter y) next-color))))
          (push-line-tail start-x start-y nil))
        false))



(var fms (machine.create {:initial "selecting-action"
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
                                   {:name "push" :from "picking-push-line" :to "selecting-action"}]
                          :callbacks {:onenter-selecting-action on-enter-selecting-action
                                      :onbefore-add on-before-add
                                      :onbefore-move on-before-move
                                      :onenter-placing-stone on-enter-placing-stone
                                      :onbefore-pick on-before-pick
                                      :onbefore-place on-before-place
                                      :onbefore-linup on-before-lineup
                                      :onbefore-push on-before-push}}))

;; ------------------------------------------------------|
;; |                  Graphics & Input                   |
;; |                                                     |
;; ------------------------------------------------------|
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
           {:action 4} (tset cursor :action  1)
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
