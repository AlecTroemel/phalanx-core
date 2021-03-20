(import tester :prefix "" :exit false)
(import ./../src/phalanx :prefix "")

(deftest
  # flip
  (test "flip: white"
        (= :black (flip :white)))
  (test "flip: black"
        (= :white (flip :black)))
  (test "flip: left"
        (= :right (flip :left)))
  (test "flip: right"
        (= :left (flip :right)))
  (test "flip: up"
        (= :down (flip :up)))
  (test "flip: down"
        (= :up (flip :down)))

  # starting-board
  (test "starting-board: returns correct length board"
        (= 8 (length (starting-board))))

  # only
  (test "only: limit to 1 color"
        (deep= @{[2 2] :white}
               (only :white @{[2 2] :white
                              [2 3] :black})))

  # free-stone-count
  (test "free-stone-count"
        (= (- 9 3)
           (free-stone-count :white {[2 2] :white
                                     [3 4] :white
                                     [5 5] :white})))

  # in-bounds?
  (test "in-bounds?: in bounds"
        (in-bounds? [5 4]))
  (test "in-bounds?: OOB, left border"
        (not (in-bounds? [-1 4])))
  (test "in-bounds?: OOB, right border"
        (not (in-bounds? [9 4])))
  (test "in-bounds?: OOB, top border"
        (not (in-bounds? [4 -1])))
  (test "in-bounds?: OOB,  bottom border"
        (not (in-bounds? [4 9])))
  (test "in-bounds?: OOB, black temple"
        (not (in-bounds? [0 0])))
  (test "in-bounds?: OOB, white temple"
        (not (in-bounds? [8 8])))

  # neighbors-of
  (test "neighbors-of"
        (=
         (neighbors-of [3 5])
         [[4 5] [2 5] [3 6] [3 4]]))

  #neighbors?
  (test "neighbors?: are"
        (neighbors? [3 5] [3 6]))
  (test "neighbors?: are not"
        (not (neighbors? [3 5] [8 7])))

  # valid-neighbors
  (test "valid-neighbors: open neighbors in a crowded board"
        (deep= @[[0 3]]
               (valid-neighbors [0 4] :white {[0 3] :white
                                              [1 4] :black})))

  # direction-iter
  (test "direction-iter: left"
        (= [0 1] ((direction-iter :left) [1 1])))
  (test "direction-iter: right"
        (= [2 1] ((direction-iter :right) [1 1])))
  (test "direction-iter: up"
        (= [1 0] ((direction-iter :up) [1 1])))
  (test "direction-iter: down"
        (= [1 2] ((direction-iter :down) [1 1])))

  # remove-stone
  (test "remove-stone: works"
        (deep= @{[2 2] :white}
               (remove-stone [2 3] @{[2 2] :white
                                     [2 3] :black})))
  # remove-stones
  (test "remove-stones: works"
        (deep= @{[2 2] :white}
               (remove-stones [[3 3] [2 3]]
                             @{[2 2] :white
                               [2 3] :black
                               [3 3] :black})))

  # add-stone
  (test "add-stone: works"
        (deep= (add-stone [2 3] :white @{[2 2] :white})
               @{[2 2] :white [2 3] :white}))

  # add-stones
  (test "add-stones: works"
        (deep= @{[2 2] :white [2 3] :white [3 3] :white}
               (add-stones [[2 3] [3 3]] :white @{[2 2] :white})))

  # possible-adds
  (test "possible-adds: simple case"
        (deep= @[[:add [1 3]] [:add [0 2]]]
               (possible-adds :white @{[0 3] :white [0 4] :black})))

  # possible-add?
  (test "possible-add?: it is"
        (possible-add? [1 3] :white @{[0 3] :white [0 4] :black}))
  (test "possible-add?: it aint"
        (not (possible-add? [4 4] :white @{[0 3] :white
                                           [0 4] :black})))

  # army-at
  (test "army-at: simple"
        (deep= @{[2 3] :white [2 2] :white}
               (army-at [2 2] :white @{[2 2] :white
                                       [2 3] :white
                                       [3 3] :black
                                       [5 5] :white})))

  # possible-moves
  (test "possible-moves: simple"
        (deep= (possible-moves :white @{[2 2] :white
                                        [2 3] :white
                                        [3 2] :white
                                        [3 3] :black})
               @[[:move [3 2] [3 2]]
                 [:move [3 2] [1 2]]
                 [:move [3 2] [2 1]]
                 [:move [3 2] [3 3]]
                 [:move [3 2] [1 3]]
                 [:move [3 2] [2 4]]
                 [:move [2 2] [4 2]]
                 [:move [2 2] [2 2]]
                 [:move [2 2] [3 3]]
                 [:move [2 2] [3 1]]
                 [:move [2 2] [1 3]]
                 [:move [2 2] [2 4]]
                 [:move [2 3] [1 2]]
                 [:move [2 3] [2 3]]
                 [:move [2 3] [2 1]]
                 [:move [2 3] [4 2]]
                 [:move [2 3] [3 3]]
                 [:move [2 3] [3 1]]]))

  # possible-move?
  (test "possible-move?: it is"
        (possible-move? [2 2] [3 1] :white @{[2 2] :white
                                               [2 3] :white
                                               [3 2] :white
                                               [3 3] :black}))
  (test "possible-move?: it aint"
        (not (possible-move? [2 4] [6 1] :white @{[2 2] :white
                                                    [2 3] :white
                                                    [3 2] :white
                                                    [3 3] :black})))

  # dead?
  (test "dead: its alive"
        (not (dead? [1 1] :white @{[1 1] :white
                                   [1 2] :white})))
  (test "dead: no friendly neighbors"
        (dead? [1 1] :white @{[1 1] :white}))
  (test "dead: off the board"
        (dead? [-1 1] :white @{[-1 1] :white
                               [0 1] :white}))

  # remove-dead-stones
  (test "remove-dead-stones: simple case"
        (deep= @{[4 4] :white
                 [4 5] :white}
               (remove-dead-stones @{[1 1] :white
                                     [4 4] :white
                                     [4 5] :white})))


  # winner
  (test "winner: white by elimination"
        (= :white
           (winner @{[1 2] :white [2 2] :white})))
  (test "winner: black by elimination"
        (= :black
           (winner @{[1 2] :black [2 2] :black})))
  (test "winner: black by temple"
        (= :black
           (winner @{[1 0] :black
                     [1 1] :black
                     [4 4] :white
                     [4 5] :white})))
  (test "winner: white by temple"
        (= :white
           (winner @{[8 7] :white
                     [7 7] :white
                     [4 4] :black
                     [4 5] :black})))

  # starting-position
  (test "starting-position: up to opponate stone"
        (= [2 3]
           (starting-position [2 2] :up  :white @{[2 2] :white
                                                 [2 3] :white
                                                 [2 4] :black})))
  (test "starting-position: up to open space"
        (= [2 3]
           (starting-position [2 2] :up :white @{[2 2] :white
                                                 [2 3] :white})))

  # possible-push?
  (test "possible-push?: simple case"
        (possible-push? [2 2] :up :white @{[1 1] :black
                                           [2 1] :black
                                           [2 2] :white
                                           [2 3] :white
                                           [2 4] :white}))
  (test "possible-push?: squish into yourself"
        (possible-push? [2 2] :up :white @{[1 0] :white
                                           [2 0] :white
                                           [1 1] :black
                                           [2 1] :black
                                           [2 2] :white
                                           [2 3] :white
                                           [2 4] :white}))
  (test "possible-push?: push off board"
        (possible-push? [2 2] :up :white @{[1 0] :black
                                           [2 0] :black
                                           [2 1] :white
                                           [2 2] :white
                                           [2 3] :white}))
  (test "possible-push?: not touching opponate"
        (not (possible-push? [2 2] :up :white @{[2 1] :white
                                                [2 2] :white
                                                [2 3] :white})))
  (test "possible-push?: not longer then opponate"
        (not (possible-push? [2 2] :up :white @{[2 0] :black
                                                [2 1] :black
                                                [2 2] :white
                                                [2 3] :white})))
  (test "possible-push?: bad starting pos"
        (not (possible-push? [2 2] :up :white @{})))

  # possible-pushes
  (test "possible-pushes: all directions"
        (deep= (possible-pushes :white @{[4 4] :white
                                         [4 5] :white
                                         [5 4] :white
                                         [5 5] :white
                                         [5 3] :black
                                         [6 4] :black
                                         [3 5] :black
                                         [4 6] :black})
               @[[:push [4 4] :right]
                 [:push [4 4] :down]
                 [:push [4 5] :left]
                 [:push [4 5] :down]
                 [:push [5 4] :right]
                 [:push [5 4] :up]
                 [:push [5 5] :left]
                 [:push [5 5] :up]]))

  # push-stones
  (test "push-stones: simple case"
        (deep= (push-stones [2 2] :up :white @{[1 1] :black
                                               [2 1] :black
                                               [2 2] :white
                                               [2 3] :white
                                               [2 4] :white})
               @{[1 1] :black
                 [2 0] :black
                 [2 1] :white
                 [2 2] :white
                 [2 3] :white}))

  # move
  (test "move: simple case"
        (deep= (move-stone [2 2] [3 1] :white @{[2 2] :white
                                                [2 1] :white
                                                [2 0] :white})
               @{[2 0] :white
                 [2 1] :white
                 [3 1] :white}))
  )
