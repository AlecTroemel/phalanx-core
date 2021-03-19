# ------------------------------------------------------|
# | implimentation of Zobrist hashing                   |
# | Each position on the board get a random number.     |
# | then a unique key for a boards posistion can be     |
# | generated from OR'ing those random numbers.         |
# | https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-5-zobrist-hashing/
# ------------------------------------------------------|

(defn init [size]
  (let [rng (math/rng (os/time))
        z-tab @{:white @{} :black @{}}]
    (loop [side :keys z-tab]
      (for x 0 size
        (for y 0 size
          (put (z-tab side) [x y] (math/rng-int rng)))))
    (put z-tab :white-to-move (math/rng-int rng))
    z-tab))

(defn get-hash [z-tab side-to-move board]
  (bxor
   (splice (map |(let [(pos side) $]
                   (get (z-tab side) pos))
                (pairs board)))
   (if (= side-to-move :white)
     (z-tab :white-to-move)
     0)))
