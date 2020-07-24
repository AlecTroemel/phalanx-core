(defn flip [thing]
  "flip color or dir, black<->white, left<->right, up<->down"
  (match thing
     :white :black
     :black :white
     :up :down
     :down :up
     :left :right
     :right :left))

(def board @{[2 2] :white
                  [2 3] :white
                  [3 2] :white
                  [3 3] :white
                  [7 7] :black
                  [7 8] :black
                  [8 7] :black
                  [8 8] :black})

(defn only [color board]
  "return the stones for a given color."
  (table ;(mapcat |(if (= color (get $ 1)) $ [])
                  (pairs board))))

(defn free-stone-count [color board]
  "the number of remain stones (max of 9) looking at the stone-map"
  (- 9 ((frequencies board) color)))

(defn in-bounds [pos]
  "are the xy pos inside the bounds of the board (temples are out of bounds)"
  (and (> (pos 0) 0) (<= (pos 0) 9)
       (> (pos 1) 0) (<= (pos 1) 9)
       (not= pos [1 1]) # black temple
       (not= pos [9 9]))) # white temple

(defn neighbors-of [pos]
  "list of orthogonal neighbors of pos"
  @[[(inc (pos 0)) (pos 1)]
    [(dec (pos 0)) (pos 1)]
    [(pos 0) (inc (pos 1))]
    [(pos 0) (dec (pos 1))]])

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
    :left  |(tuple (dec ($ 0)) ($ 1))
    :right |(tuple (inc ($ 0)) ($ 1))
    :up    |(tuple ($ 0) (dec ($ 1)))
    :down  |(tuple ($ 0) (inc ($ 1)))))

(defn remove-stone [pos board]
  "return a new board with a stone removed at given pos"
  (put (table/clone board) pos nil))

(defn remove-stones [poss board]
  "return a new board with all the stones given removed (recursively)"
  (if (= (length poss) 1)
    (remove-stone (first poss) board)
    (remove-stones (drop 1 poss)
                   (remove-stone (first poss) board))))

(defn add-stone [pos color board]
  "return a new board with a stone added given pos"
  (put (table/clone board) pos color))

(defn add-stones [poss color board]
  "return a new board with all the stones given added (recursively)"
  (if (= (length poss) 1)
    (add-stone (first poss) color board)
    (add-stones (drop 1 poss)
                color
                (add-stone (first poss) color board))))

(defn possible-adds [color board]
  "list of all possible valid add move. A valid add is a pos that is
   - adjacent to color
   - in bounds
   - currently unoccupied
   [[x y :add]]"
  (distinct
   (mapcat |(map |(tuple ;$ :add)
                 (valid-neighbors $ nil board))
           (keys (only color board)))))

(defn is-possible-add [pos color board]
  "check if pos is valid add. see possible-adds for rules"
  (find |(= pos $) (possible-adds color board)))

(each n (possible-adds :white test-board)
  (print "x:" (get n 0) " y:" (get n 1)))



# (each n (valid-neighbors {:x 3 :y 4} nil test-board)
#   (print "x:" (get n :x) " y:" (get n :y)))
# (print (get ((direction-iter UP) {:x 1 :y 3}) :y))
# (print (are-neighbors [3 4] [3 2]))
# (print (flip UP))

# (print (length (remove-stone {:x 3 :y 3} test-board)))
# (print (length test-board))
