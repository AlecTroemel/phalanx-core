(import /phalanx)

(defn- state-execute [self action]
  "returns a state where the action was executed on the states board"
  (let [board (self :board)
        turn-count (self :turn-count)
        turn-remaining-actions (self :turn-remaining-actions)
        side-to-move (self :side-to-move)

        # actually exectue the action on the board
        action-type (first action)
        action-params (array/slice action 1)
        new-board (match action-type
                    :add  (phalanx/add-stone (splice action-params) side-to-move board)
                    :move (phalanx/move-stone (splice action-params) side-to-move board)
                    :push (phalanx/push-stones (splice action-params) side-to-move board))

        # Manage the turns after make the move
        flipping-sides (= turn-remaining-actions 1)
        new-turn-count (if flipping-sides (+ 1 turn-count) turn-count)
        new-side-to-move (if flipping-sides (phalanx/flip side-to-move) side-to-move)
        new-turn-remaining-actions (if flipping-sides
                                     (-> self
                                         (get (match new-side-to-move
                                                :white :white-conf
                                                :black :black-conf))
                                         (get :moves-per-turn))
                                     (- turn-remaining-actions 1))

        # going second gives your an extra actions on your first turn
        new-turn-remaining-actions (if (= turn-count 1)
                                     (+ 1 new-turn-remaining-actions)
                                     new-turn-remaining-actions)]
    (merge
     self
     {:board new-board
      :side-to-move new-side-to-move
      :turn-count new-turn-count
      :turn-remaining-actions new-turn-remaining-actions})))



(defn init []
  @{:board-size 9
    :board (phalanx/starting-board)
    :black-conf @{:control (keyword "ai") :moves-per-turn 3}
    :white-conf @{:control (keyword "ai") :moves-per-turn 3}
    :side-to-move (keyword "black")
    :turn-count 0
    :turn-remaining-actions 3
    :execute state-execute})
