# ----  Testing  ----
(import /state)
(import /ai)
(import /phalanx)

(var gamestate (state/init))
(def ai (ai/init))

(while (nil? (phalanx/winner (gamestate :board)))
  (phalanx/print-board (gamestate :board))
  (def actions (:pick-actions ai gamestate (gamestate :side-to-move)))
  (each action actions
    (set gamestate (:execute gamestate (action 1)))))

(phalanx/print-board (gamestate :board))
(print "TURNS: " (gamestate :turn-count))
(print "WINNER: " (phalanx/winner (gamestate :board)))
