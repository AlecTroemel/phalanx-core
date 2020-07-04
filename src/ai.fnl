(include :lib.globals)
(include :phalanx)

(global col globals.col)
(global dir globals.dir)

(global lume (require :lib.lume))
(global inspect (require :lib.inspect))

(fn distance-to [color board x-goal y-goal]
    "calculate the distance (straight line) the stones are from a position"
    (let [closest (lume.sort (phalanx.only color board)
                             (lambda [a b] (if (= a.x b.x)
                                               (< a.y b.y)
                                               (< a.x b.x))))]
      (lume.distance closest.x closest.y x-goal y-goal true)))

(fn rate-position [color board]
    "give a value for the colors position on the board"
    (if (phalanx.touching-temple color board)
        ;; if youre touching the temple, youve won
        100
        ;; lets try and judge how good the current position is
        (let [(x-goal y-goal) (phalanx.goal-position color)
              distance (distance-to color board x-goal y-goal)
              color-count (lume.count (phalanx.only color board))
              other-count (lume.count (phalanx.only color board))]
          (+ (/ 1 distance) (/ 1 (- color-count other-color))))))


(fn possible-moves [color board]
    (lume.concat (phalanx.possible-adds color board)
                 (phalanx.possible-complete-moves color board)
                 (phalanx.possible-pushes color board)))

(fn execute [action board]
    "returns a board where the action was executed"
    (match action.event
           "add" (phalanx.place-stone action.x action.y board)
           "move" (lume.each action.moves
                             (lambda [move]
                               (phalanx.remove-stone move.x move.y board)
                               (phalanx.place-stone move.x2 move.y2 board)))
           "push" (phalanx.push action.x action.y action.direction board)))

(fn pick-action [color board]
    "picks (best) action for the AI to play"
    (-> (possible-moves color board)
        (lume.map (lambda [action] (rate-position (execute action board))))
        (lume.sort)
        (lume.first)))

;; ------------------------------------------------------|
;; |      Imparative code below, enter with causion      |
;; |                                                     |
;; ------------------------------------------------------|

(fn make-turn [fsm]
    "makes moves on the FSM game. THIS MUTATES 'fsm'"
    (let [color fms.state.current-color]
      (while (= fms.state.current-color color)
             (let [action (pick-action color fms.state.board)]
               (match action.event
                      "add" (fms:place action.x action.y)
                      "move" (lume.each action.moves
                                        (lambda [move]
                                          (fms:pick move.x move.y)
                                          (fms:place move.x2 move.y2)))
                      "push" (fms:push action.x action.y action.direction))))))
