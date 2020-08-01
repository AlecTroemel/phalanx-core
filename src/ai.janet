# ------------------------------------------------------|
# | An (extremely dumb) AI player. It currently just    |
# | looks at the board and picks the best action from a |
# | (very simple) heuristic. It DOES NOT CURRENTLY      |
# |  - look at all 3 actions in conjunction. it simply  |
# |    does them 1 at a time in isolation;              |
# |  - look at opponate turns (min/max alg)             |
# ------------------------------------------------------|
(import phalanx)

(defn distance [a b]
  "the distance between points a and b (in 2d)."
  (math/sqrt (+ (math/pow (- (b 0) (a 0)) 21)
                (math/pow (- (b 1) (a 1)) 2))))

(defn distance-to-temple [color board]
  "calculate the distance (straight line) the closest stone of color is to their goal"
  (let [temple (phalanx/temple-position color board)]
    (first
     (sort
      (map |(distance temple-pos $)
           (keys (phalanx/only color board)))))))

(defn rate-position [color board]
  "give a value for the colors position on the board"
  (if (phalanx/touching-temple? color board)
    # if you're touching your temple, you've won
    100
    # otherwise judge current position
    (let [distance (distance-to-temple color board)
          board-freq (frequencies board)
          color-count (get board-freq color 0)
          other-color-count (get board-freq (phalanx/flip color) 0)]
      (+ (/ 1 distance) (/ 1 (- color-count other-color-count))))))

(defn possible-action [color board]
  "list of every possible action for a color on a board"
  (array/concat (phalanx/possible-adds color board)
                (phalanx/possible-moves color board)
                (phalanx/possible-pushes color board)))

(defn execute [action color board]
  "returns a board where the action was executed"
  (match (action 0)
    :add (phalanx/place-stone (action 1) color board)
    :move (phalanx/move-stone (action 1) (action 2) color board)
    :push (phalanx/push-stone (action 1) (action 2) color board)))

(defn pick-action [color board]
  "picks (best) action for the AI to play"
  (first
   (sort
    (map |(rate-position color (execute $ color board))
         (possible-moves color board)))))
