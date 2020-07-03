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
    (let [(x-goal y-goal) (phalanx.goal-position color)
          distance (distance-to color board x-goal y-goal)
          color-count (lume.count (phalanx.only color board))
          other-count (lume.count (phalanx.only color board))]
      (+ (/ 1 distance) (/ 1 (- color-count other-color)))))



(fn possible-moves [color board]
    "TODO"
    [])

(fn possible-pushes [color board]
    "TODO"
    [])


(fn possible-moves [color board]
    (lume.concat (phalanx.possible-adds color board)
                 (phalanx.possible-moves color board)
                 (phalanx.possible-pushes color board)))

(fn execute [move fsm ]
    (let [(event params) (values move)]
      "TODO"
      ))

(fn handle-computer [color board]
    "picks moves for the AI to play"
    (-> (possible-moves color board)
        (lume.map (lambda [move] (rate-position (execute move))))
        (lume.sort)
        (lume.first)))
