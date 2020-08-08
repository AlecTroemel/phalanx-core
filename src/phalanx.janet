(defn flip [thing]
  "flip color or dir, black<->white, left<->right, up<->down"
  (match thing
     :white :black
     :black :white
     :up :down
     :down :up
     :left :right
     :right :left))

(defn starting-board []
  @{[1 1] :white
    [1 2] :white
    [2 1] :white
    [2 2] :white
    [6 6] :black
    [6 7] :black
    [7 6] :black
    [7 7] :black})

(defn print-board [board]
  (print "|---- BOARD ----|")
  (for i 0 9
    (for j 0 9
      (prin "|" (match (board [j i])
                   :white "W"
                   :black "B"
                   _ " ")))
    (print ""))
  (print "|---------------|"))

(defn only [color board]
  "return the stones for a given color"
  (table ;(mapcat |(if (= color (get $ 1)) $ [])
                  (pairs board))))

(defn free-stone-count [color board]
  "the number of remain stones (max of 9) looking at the stone-map"
  (- 9 (get (frequencies board) color 0)))

(defn temple-position [color]
  "position of the colors goal"
  (match color
    :black [0 0]
    :white [8 8]))

(defn in-bounds? [pos]
  "are the xy pos inside the bounds of the board (temples are out of bounds)"
  (let [[x y] pos]
    (and (>= x 0) (< x 9)
         (>= y 0) (< y 9)
         (not= pos (temple-position :black))
         (not= pos (temple-position :white)))))

(defn neighbors-of [pos]
  "list of orthogonal neighbors of pos"
  (let [[x y] pos]
    [[(inc x) y]
     [(dec x) y]
     [x (inc y)]
     [x (dec y)]]))

(defn neighbors? [pos-a pos-b]
  "check if pos-a is an orthogonal in bounds neighbor of pos-b"
  (not= nil (find |(= $ pos-b) (neighbors-of pos-a))))

(defn valid-neighbors [pos color board]
  "return the orthogonal neighbors which are the desired color.
   passing nil for color will find open neighbors"
  (filter |(and (= color (get board $ nil)) (in-bounds? $))
          (neighbors-of pos)))

(defn direction-iter [direction]
  "return a function to move a pos in a directio"
  (match direction
    :left  |[(dec ($ 0)) ($ 1)]
    :right |[(inc ($ 0)) ($ 1)]
    :up    |[($ 0) (dec ($ 1))]
    :down  |[($ 0) (inc ($ 1))]))

(defn remove-stone [pos board]
  "return a new board with a stone removed at given pos"
  (put (table/clone board) pos nil))

(defn remove-stones [poss board]
  "return a new board with all the stones given removed (recursively)"
  (if (= (length poss) 1)
    (remove-stone (first poss) board)
    (remove-stones (drop 1 poss)
                   (remove-stone (first poss) board))))

(defn possible-adds [color board]
  "list of all possible valid add move. A valid add is a pos that is
   - adjacent to color
   - in bounds
   - currently unoccupied
   [(:add (x y))]"
  (distinct
   (mapcat |(map |(tuple :add $)
                 (valid-neighbors $ nil board))
           (keys (only color board)))))

(defn possible-add? [pos color board]
  "check if pos is valid add. see possible-adds for rules"
  (find |(= [:add pos] $)
        (possible-adds color board)))

(defn add-stone [pos color board]
  "return a new board with a stone added given pos"
  (when (not (possible-add? pos color board))
    (error "not a valid add"))
  (put (table/clone board) pos color))

(defn add-stones [poss color board]
  "return a new board with all the stones given added (recursively)"
  (if (= (length poss) 1)
    (add-stone (first poss) color board)
    (add-stones (drop 1 poss)
                color
                (add-stone (first poss) color board))))

(defn army-at [pos color board]
  "find all connected pieces (an army) for a color starting at a position. Returns board"
  (let [board (only color board)
        army-board @{}]
    (defn army-at-rec [pos]
      (each neighbor (valid-neighbors pos color board)
        (when (not (army-board neighbor))
          (put army-board neighbor color)
          (army-at-rec neighbor)))
      army-board)pos
    (army-at-rec pos)))

(defn possible-moves [color board]
  "possible moves for the board, a move consists of...
   - from: current stone on board
   - to: valid add on a open (nil) location on board resulting from removing from stone
  [(:move (x y) (x2 y2))]"
  (distinct
   (mapcat (fn [from]
             (mapcat (fn [to] [[:move from (to 1)]])
                     (possible-adds
                      color
                      (remove-stone from (army-at from color board)))))
           (keys (only color board)))))

(defn possible-move? [from to color board]
  "check if pos is valid add. see possible-moves for rules"
  (find |(= [:move from to] $)
        (possible-moves color board)))

(defn move-stone [from to color board]
  "return a board with the stone moved"
  (when (not (possible-move? from to color board))
    (error "not a valid move"))
  (add-stone to color (remove-stone from board)))

(defn starting-position [pos dir color board]
  "the furthest stone away from opponate on the push line (recursive)"
  (let [next-pos ((direction-iter (flip dir)) pos)]
    (cond
      (= nil (board next-pos)) pos
      (= (flip color) (board next-pos)) pos
      (= color (board next-pos))
      (starting-position next-pos dir color board))))

# NOTE: unlike add and move, the logic to determine a valid push is in the single possible-push?
#       function instead of the "all possible" list generator...it was just easier that way
(defn possible-push? [pos dir color board]
  "check if pos and dir is possible push for color on board"
  (let [pos (starting-position pos dir color board)
        iter (direction-iter dir)]
    (defn possible-push-tail [pos ally-count opponate-count]
      (cond
        # next spot is a stone stone of your own color
        (= color (board pos))
        (if (> opponate-count 0)
          (> ally-count opponate-count) # your squishing into yourself
          (possible-push-tail (iter pos) (inc ally-count) opponate-count))

        # next spot is a stone stone of opponates color
        (= (flip color) (board pos))
        (possible-push-tail (iter pos) ally-count (inc opponate-count))

        # next spot is empty (default case)
        (and (> opponate-count 0) # must be touching opponate
             (> ally-count opponate-count)))) # must be longer then opponate
    (if (= color (board pos))
      (possible-push-tail pos 0 0)
      false))) # must start on own color

(defn possible-pushes [color board]
  "list of all possible pushes for a color on the board
  [:push (x y) :dir]"
  (let [pushes @[]]
    (loop [[pos color] :pairs board]
      (each dir [:left :right :up :down]
        (when (possible-push? pos dir color board)
          (array/push pushes [:push pos dir]))))
    pushes))

(defn push-stones [pos dir color old-board]
  "return a new board after a push action at the given pos and direction"
  (when (not (possible-push? pos dir color old-board))
    (error "not a valid push"))
  (let [new-board (table/clone old-board)
        pos (starting-position pos dir color new-board)
        iter (direction-iter dir)]
    (defn push-line-tail [pos prev-pos]
      "NOTE: this mutates new-board"
      (if (and (= (flip color) (old-board pos)) # handle squishing case
               (= color (old-board (iter pos))))
        (put new-board (iter pos) color)
        (do
          (put new-board pos prev-pos)
          (when (old-board pos)
            (push-line-tail (iter pos) (old-board pos))))))
    (push-line-tail pos nil)
    new-board))

(defn dead? [pos color board]
  "check if a stone is dead, it is dead if it
  1. is not in bounds
  2. has no friendly valid neighbors"
  (or (= 0 (length (valid-neighbors pos color board)))
      (not (in-bounds? pos))))

(defn remove-dead-stones [board]
  "new board with all dead/isolated stones removed from the board"
  (table ;(mapcat |(if (dead? ($ 0) ($ 1) board) [] $)
                  (pairs board))))

(defn touching-temple? [color board]
  "check if color is touching the temple"
  (any? (map |(neighbors? $ (temple-position color))
             (keys (only color board)))))

(defn winner [board]
  "return color that has won the game, else nil"
  (cond
    (= 9 (free-stone-count :black board)) :white
    (= 9 (free-stone-count :white board)) :black
    (touching-temple? :black board) :blackg
    (touching-temple? :white board) :white))

(defn execute [action params color board]
  "returns a board where the action was executed"
  (match action
    :add (add-stone (params 0) color board)
    :move (move-stone (params 0) (params 1) color board)
    :push (push-stones (params 0) (params 1) color board)))
