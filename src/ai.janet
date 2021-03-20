# ------------------------------------------------------|
# | An AI player. Uses the minmax algorith with:        |
# |  - Alpha beta pruning                               |
# |  - Zobrist hashing                                  |
# ------------------------------------------------------|

(import /phalanx)
(import /zobrist)
(import /state)

(defn- distance [a b]
  "the distance between points a and b (in 2d)."
  (math/sqrt (+ (math/pow (- (b 0) (a 0)) 2)
                (math/pow (- (b 1) (a 1)) 2))))

(defn- closest-distance-to-temple [color board]
  "find the closest distance (in a straight line) of a colors stones to their goal"
  (let [temple (phalanx/temple-position color)]
    (first
     (sort
      (map |(distance temple $)
           (keys (phalanx/only color board)))))))

(defn- rate-position [color board]
  "give a value for the colors position on the board"
  (cond
      (phalanx/touching-temple? color board) 100 # you've won
      (phalanx/touching-temple? (phalanx/flip color) board) (- 100) # they've won

      # otherwise judge current position
      (let [# closer to temple => closer to 1
            distance-weight 10
            distance (closest-distance-to-temple color board)
            distance-score (/ 1 distance)

            # more stones then opponant you have => closer to 1
            # if opponant has more stones then you, then this will be negative
            color-count-weight 40
            board-freq (frequencies board)
            color-count (get board-freq color 0)
            other-color-count (get board-freq (phalanx/flip color) 0)
            color-count-score (/ (- color-count other-color-count) 9)]
        (+
         (* distance-score distance-weight)
         (* color-count-score color-count-weight)))))

(defn- possible-actions [color board]
  "list of every possible action for a color on a board.
   NOTE: to optimize AI (apha beta pruning), better moves should come first
         in general adds are better then pushes, which are better then moves
   TODO: another optimization could be done to favor actions closer to your goal"
  (array/concat (phalanx/possible-adds color board)
                (phalanx/possible-pushes color board)
                (phalanx/possible-moves color board)))

# https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning#Pseudocode
# DONE: impliment state
# DONE: return list of actions taken with score
# DONE: add zobrist hashing history
# TODO: limit zobrist history length https://adamberent.com/2019/03/02/transposition-table-and-zobrist-hashing/
(defn- alphabeta [self state depth alpha beta side-to-max seen-actions]
  "minmax + alpha-beta + zobrist-hashing"
  (cond
    # Base case
    (or (= depth 0) (phalanx/winner (state :board)))
    {:score (rate-position (state :side-to-move) (state :board))
     :actions seen-actions}

    # Maximize
    (= (state :side-to-move) side-to-max)
    (let [z-key (:get-hash (self :zobrist) state)
          z-result (get-in self [:zobrist :history z-key])]
      (if z-result
        {:score (z-result :score)
         :actions [(splice seen-actions) (z-result :action)]}
        (do
          (var alpha alpha)
          (var max-eval @{:score math/-inf :actions nil})
          (loop [action
                 :in (possible-actions (state :side-to-move) (state :board))
                 :until (<= beta alpha)
                 :let [new-seen-actions [(splice seen-actions) [(state :side-to-move) action]]
                       child-state (:execute state action)
                       child-result (:alphabeta self child-state (- depth 1) alpha beta side-to-max new-seen-actions)]]
            (when (> (child-result :score) (max-eval :score))
              (set max-eval child-result))
            (set alpha (max alpha (max-eval :score))))

          (put-in self [:zobrist :history z-key] {:score (max-eval :score) :action (last (max-eval :actions))})
          max-eval)))

    # Minimize (side to max not states "current turn")
    (let [z-key (:get-hash (self :zobrist) state)
          z-result (get-in self [:zobrist :history z-key])]
      (if z-result
        {:score (z-result :score)
         :actions [(splice seen-actions) (z-result :action)]}
        (do
          (var beta beta)
          (var min-eval @{:score math/inf :actions nil})
          (loop [action
                 :in (possible-actions (state :side-to-move) (state :board))
                 :until (<= beta alpha)
                 :let [new-seen-actions [(splice seen-actions) [(state :side-to-move) action]]
                       child-state (:execute state action)
                       child-result (:alphabeta self child-state (- depth 1) alpha beta side-to-max new-seen-actions)]]
            (when (< (child-result :score) (min-eval :score))
              (set min-eval child-result))
            (set beta (min beta (min-eval :score))))
          (put-in self [:zobrist :history z-key] {:score (min-eval :score) :action (last (min-eval :actions))})
          min-eval)))))

(defn- pick-actions [self state side-to-max]
  (get (:alphabeta self state 1 math/-inf math/inf side-to-max []) :actions))

(defn init []
  @{:zobrist (zobrist/init 9)
    :alphabeta alphabeta
    :pick-actions pick-actions})
