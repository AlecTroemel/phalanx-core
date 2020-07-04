(include :lib.globals)

(global phalanx (include :phalanx))

(global col globals.col)
(global dir globals.dir)

(global lume (require :lib.lume))
(global inspect (require :lib.inspect))

(global mem-distance (lume.memoize lume.distance))
(global distance-to-goal (lume.memoize
                          (fn distance-to-inner [color board]
                              "calculate the distance (straight line) the stones are from a position"
                              (let [(x-goal y-goal) (phalanx.goal-position color)]
                                (let [closest (lume.first (lume.sort (phalanx.only color board)
                                                                     (lambda [a b] (if (= a.x b.x)
                                                                                       (< a.y b.y)
                                                                                       (< a.x b.x)))))]
                                  (mem-distance closest.x closest.y x-goal y-goal true))))))

(fn rate-position [color board]
    "give a value for the colors position on the board"
    (if (phalanx.touching-temple color board)
        ;; if youre touching the temple, youve won
        100
        ;; lets try and judge how good the current position is
        (let [distance (distance-to-goal color board)
              color-count (lume.count (phalanx.only color board))
              other-color-count (lume.count (phalanx.only (phalanx.other color) board))]
          (+ (/ 1 distance) (/ 1 (- color-count other-color-count))))))


(fn possible-moves [color board]
    (lume.concat (phalanx.possible-adds color board)
                 (phalanx.possible-moves color board)
                 (phalanx.possible-pushes color board)))

(fn execute [action color board]
    "returns a board where the action was executed"
    (match action.event
           "add" (phalanx.place-stone action.x action.y color board)
           "move" (->> board
                       (phalanx.remove-stone (. (. action.moves 1) :x ) (. (. action.moves 1) :y ))
                       (phalanx.place-stone (. (. action.moves 1) :x2 ) (. (. action.moves 1) :y2 ) color)
                       (phalanx.remove-stone (. (. action.moves 2) :x ) (. (. action.moves 2) :y ))
                       (phalanx.place-stone (. (. action.moves 2) :x2 ) (. (. action.moves 2) :y2 ) color))
           "push" (phalanx.push action.x action.y action.direction color board)))

(fn pick-action [color board]
    "picks (best) action for the AI to play"
    (lume.first (lume.sort (possible-moves color board)
                           (lambda [move-a move-b]
                             (> (rate-position color (execute move-a color board))
                                (rate-position color (execute move-b color board)))))))

{: pick-action}
