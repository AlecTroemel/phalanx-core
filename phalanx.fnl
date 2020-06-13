(include :lib.gfx)
(global lume (require :lib.lume))
(global machine (require :lib.fsm))
(global inspect (require :lib.inspect))
(global map-size 9)

;; ----------------------------------------------
;; | Immutable, function functions for the game |
;; |                                            |
;; ----------------------------------------------
(fn free-stones-count [color stone-map]
    "the number of remain stones (max of 9) looking at the stone-map"
    (- 9 (lume.reduce stone-map
                      #(+ $1 (lume.count (lume.filter $2 nil) #(= $1 color)))
                      0)))

(fn color-at [x y stone-map] (. (. stone-map x) y))

(fn foreach-spot [stone-map iteratee]
    "call func(color, x,y) for each location on the stone map"
    (each [x line (pairs stone-map)]
          (each [y color (pairs line)]
                (iteratee color x y))))

(fn open-neighbors [x y stone-map]
    "return the orthogonal neighbors which are open (nil)"
    (let [neighbors [{ :x (+ x 1) :y y}
                     { :x (- x 1) :y y}
                     { :x x :y (+ y 1)}
                     { :x x :y (- y 1)}]]
      (lume.filter neighbors #(and
                               (= nil (color-at (. $1 "x") (. $1 "y") stone-map))
                               (> (. $1 "x") 0)
                               (< (. $1 "x") map-size)
                               (> (. $1 "y") 0)
                               (< (. $1 "y") map-size)))))

(fn possible-adds [color stone-map]
    "possible locations for the add action for a color on a map"
    (local moves [])
    (foreach-spot stone-map
                  #(when (= $1 color)
                    (each [i spot (pairs (open-neighbors $2 $3 stone-map))]
                     (table.insert moves spot))))
    (lume.unique moves))

(fn is-possible-add [x y color stone-map]
    (lume.any (possible-adds color stone-map)
              #(and (= (. $1 :x) x)
                    (= (. $1 :y) y))))

;; ------------------------------------------------------|
;; |       Finite state machine & Global State           |
;; |                                                     |
;; ------------------------------------------------------|
(local stones-board []) ;; nil, 0=white, 1=black
(var current-turn "black") ;; or "white"
(var current-action-counter 2) ;; 2 actions per turn
(var cursor {:action 2 :x 5 :y 5})

(fn init-stones []
    ;; initialize each row of board
    (for [i 1 map-size] (tset stones-board i []))
    ;; black & white starting positions
    (tset stones-board 2 {1 "white" 2 "white" 3 "white"})
    (tset stones-board 3 {2 "white" 3 "white"})
    (tset stones-board 7 {7 "black" 8 "black"})
    (tset stones-board 8 {7 "black" 8 "black"}))

(fn init []
    (init-stones)
    (set current-turn "black")
    (set current-action-counter 2))

(fn on-enter-selecting-action []
    (tset cursor :action 1)
    (when (= current-action-counter 0)
      (set current-turn (if (= current-turn "white") "black" "white"))
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
        (tset (. stones-board cursor.x) cursor.y nil) ;; remove stone
        false))

(fn on-before-place []
    (if (is-possible-add cursor.x cursor.y current-turn stones-board)
        (tset (. stones-board cursor.x) cursor.y current-turn) ;; add stone
        false))


(var fms (machine.create {:initial "selecting-action"
                          :events [{:name "add" :from "selecting-action" :to "placing-stone"}
                                   {:name "move" :from "selecting-action" :to "picking-first-stone"}
                                   {:name "pick" :from "picking-first-stone" :to "placing-first-stone"}
                                   {:name "place" :from "placing-first-stone" :to "picking-second-stone"}
                                   {:name "pick" :from "picking-second-stone" :to "placing-second-stone"}
                                   {:name "place" :from ["placing-stone" "placing-second-stone"] :to "selecting-action"}]
                          :callbacks {:onenter-selecting-action on-enter-selecting-action
                                      :onbefore-add on-before-add
                                      :onbefore-move on-before-move
                                      :onenter-placing-stone on-enter-placing-stone
                                      :onbefore-pick on-before-pick
                                      :onbefore-place on-before-place}}))


;; ------------------------------------------------------|
;; |                  Graphics & Input                   |
;; |                                                     |
;; ------------------------------------------------------|
(fn update [] nil)

(fn cursor-movement [key event]
    (match key
           "right" (tset cursor :x (+ cursor.x 1))
           "left" (tset cursor :x (- cursor.x 1))
           "up" (tset cursor :y (- cursor.y 1))
           "down" (tset cursor :y (+ cursor.y 1))
           "x" (: fms event)))

(fn keypressed [key]
    (match fms.current
           "selecting-action" (match key
                                     "right" (tset cursor :action (+ cursor.action 1))
                                     "left" (tset cursor :action (- cursor.action 1))
                                     "x" (: fms (match cursor.action 1 :add 2 :move 3 :add)))
           ;; add action
           "placing-stone" (cursor-movement key :place)
           ;; move action
           "picking-first-stone" (cursor-movement key :pick)
           "placing-first-stone" (cursor-movement key :place)
           "picking-second-stone" (cursor-movement key :pick)
           "placing-second-stone" (cursor-movement key :place))
    (match cursor
           {:action 0} (tset cursor :action 3)
           {:action 4} (tset cursor :action  1)
           {:x 0} (tset cursor :x 1)
           {:x 10} (tset cursor :x 9)
           {:y 0} (tset cursor :y 1)
           {:y 10} (tset cursor :y 9))
    )

(fn draw-board []
    ;; the board
    (for [i 1 9]
         (let [j (* 20 i) ]
           (gfx.line 20 j 180 j)    ;; x lines
           (gfx.line j 20 j 180)))  ;; y lines
    ;; the stones currently on the board
    (foreach-spot stones-board #(gfx.circle $1 (* $2 20) (* $3 20) 9))
    ;; temples
    (gfx.rect "black" 10 10 20 20)
    (gfx.rect "white" 170 170 20 20))

(fn draw-cursor []
    (gfx.circle current-turn (* cursor.x 20) (* cursor.y 20) 7))

(fn draw-ui []
    (gfx.print (.. "White remaining: " (free-stones-count "white" stones-board))  200 10)
    (gfx.print (.. "Black remaining: " (free-stones-count "black" stones-board))  200 30)
    (gfx.print (.. "Turn: " current-turn) 200 50)
    (gfx.print (.. "state: " fms.current) 200 90)
    (gfx.print (.. "actions left: " current-action-counter) 200 110)
    (each [i action (ipairs ["add" "move" "push"])]
          (let [x (+ 200 (* i 40))]
           (gfx.print action x 70)
            (when (= i cursor.action)
              (gfx.rect "black" (- x 4) 69 39 20))))
    (match fms.current
           "selecting-action" nil
           ;; add action
           "placing-stone" (draw-cursor)
           ;; move action
           "picking-first-stone" (draw-cursor)
           "placing-first-stone" (draw-cursor)
           "picking-second-stone" (draw-cursor)
           "placing-second-stone" (draw-cursor)))

(fn draw []
    (draw-board)
    (draw-ui))


{: draw : init : update : keypressed}
