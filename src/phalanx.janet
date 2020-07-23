(def UP "UP")
(def DOWN "DOWN")
(def LEFT "LEFT")
(def RIGHT "RIGHT")
(def BLACK "BLACK")
(def WHITE "WHITE")

(defn flip [thing]
  "flip color or dir, black<->white, left<->right, up<->down"
  (match thing
    "WHITE" BLACK
    "BLACK" WHITE
    "UP"    DOWN
    "DOWN"  UP
    "LEFT"  RIGHT
    "RIGHT" LEFT))

(def test-board @{{:x 2 :y 2} WHITE
                  {:x 2 :y 3} WHITE
                  {:x 3 :y 2} WHITE
                  {:x 3 :y 3} WHITE
                  {:x 7 :y 7} BLACK
                  {:x 7 :y 8} BLACK
                  {:x 8 :y 7} BLACK
                  {:x 8 :y 8} BLACK})

(defn only [color board]
  "return the stones for a given color"
  (let [new-board @{}]
    (loop [[pos col] :in (pairs board)]
      (when (= col color)
        (put new-board pos col)))
    new-board))

(defn free-stone-count [color board]
  "the number of remain stones (max of 9) looking at the stone-map"
  (- 9 ((frequencies board) color)))

(defn in-bounds [pos]
  "are the xy pos inside the bounds of the board (temples are out of bounds)"
  (and (> (pos :x) 0) (<= (pos :x) 9)
       (> (pos :y) 0) (<= (pos :y) 9)
       (not= pos [1 1]) # black temple
       (not= pos [9 9]))) # white temple

(defn neighbors-of [pos]
  "list orthogonal in bounds neighbors of pos"
  @[{:x (inc (pos :x)) :y (pos :y)}
    {:x (dec (pos :x)) :y (pos :y)}
    {:x (pos :x) :y (inc (pos :y))}
    {:x (pos :x) :y (dec (pos :y))}])

(defn are-neighbors [pos-a pos-b]
  "check if pos-a is an orthogonal in bounds neighbor of pos-b"
  (not= nil (find |(= $ pos-b) (neighbors-of pos-a))))

(defn valid-neighbors [pos color board]
  "return the orthogonal neighbors which are the desired color.
   passing nil for color will find open neighbors"
  (filter |(and (= color (get board $ nil)) (in-bounds $))
          (neighbors-of pos)))

(defn direction-iter [direction]
  "return a function to move a pos in a directio"
  (match direction
    "LEFT"  |(merge $ {:x (dec (get $ :x))})
    "RIGHT" |(merge $ {:x (inc (get $ :x))})
    "UP"    |(merge $ {:y (dec (get $ :y))})
    "DOWN"  |(merge $ {:y (inc (get $ :y))})))

(defn remove-stone [pos board]
  "return a new board with a stone removed at given pos"
  (put (table/clone board) pos nil))

(defn remove-stones [poss board]
  "return a new board with all the stones given removed (recursively)"
  (if (= (length poss) 1)
    (remove-stone (first poss) board)
    (remove-stones (array/slice poss 1 -1) (remove-stone (first poss) board))))

(defn add-stone [pos color board]
  "return a new board with a stone added given pos"
  (put (table/clone board) pos color))

(defn add-stones [poss color board]
  "return a new board with all the stones given added (recursively)"
  (if (= (length poss) 1)
    (add-stone (first poss) color board)
    (add-stones (array/slice poss 1 -1)
                color
                (add-stone (first poss) color board))))

(defn possible-adds [color board]
  "return list of all possible adds for a color on the board. [{:event :x :y}]"
  nil)

(defn is-possible-add [pos color board]
  "check if the pos and a color is a valid add move. that means is the pos
   - adjacent to color
   - in bounds
   - currently unoccupied"
  nil)

# (each n (valid-neighbors {:x 3 :y 4} nil test-board)
#   (print "x:" (get n :x) " y:" (get n :y)))
# (print (get ((direction-iter UP) {:x 1 :y 3}) :y))
# (print (are-neighbors [3 4] [3 2]))
# (print (flip UP))

# (print (length (remove-stone {:x 3 :y 3} test-board)))
# (print (length test-board))
