(import tester :prefix "" :exit false)
(import ./../src/utils/ecs :prefix "")

(deftest
  (test "example"
        (do
          (def world (ecs/init))

          (ecs/def-component position [x y])
          (ecs/def-component velocity [x y])

          (ecs/add-entity world
                          (position 10 10)
                          (velocity 1 2))

          (ecs/add-entity world
                          (position 100 24))

          (ecs/add-system world
                          [:position :velocity]
                          (fn [pos vel dt]
                            (printf "MOVE: %q %q" pos vel)))

          (ecs/add-system world
                          [:position]
                          (fn [pos dt]
                            (printf "POSITION: %q" pos)))

          (:update world 1))))
