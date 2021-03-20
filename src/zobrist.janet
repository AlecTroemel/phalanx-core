# ------------------------------------------------------|
# | implimentation of Zobrist hashing                   |
# | Each position on the board get a random number.     |
# | then a unique key for a boards posistion can be     |
# | generated from OR'ing those random numbers.         |
# | https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-5-zobrist-hashing/
# ------------------------------------------------------|

(defn- get-hash [self state]
  (let [board (state :board)
        side-to-move (state :side-to-move)]
    (bxor
     (splice (map |(let [(pos side) $] (get ((self :tab) side) pos)) (pairs board)))
     (if (= side-to-move :white) ((self :tab) :white-to-move) 0))))

# (defn- get-set [self state default-exp]
#   (let [key (:get-hash self state)
#         result (get-in self [:history key])
#        ]
#     (if result result
#         (do
#           ))))

(defn init [size]
  (let [rng (math/rng (os/time))
        z-tab @{:white @{} :black @{}}]
    (loop [side :keys z-tab]
      (for x 0 size
        (for y 0 size
          (put (z-tab side) [x y] (math/rng-int rng)))))
    (put z-tab :white-to-move (math/rng-int rng))

    @{:history @{}
      :tab z-tab
      :get-hash get-hash}))
