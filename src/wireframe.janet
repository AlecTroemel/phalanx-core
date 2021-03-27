# ------------------------------------------------------|
# |                    3D wireframe                     |
# |                                                     |
# | https://petercollingridge.appspot.com/3D-tutorial/  |
# ------------------------------------------------------|

(defn rotate-x [nodes theta]
  "rotate nodes about the X-axis"
  (let [sin-t (math/sin theta)
        cos-t (math/sin theta)]
    (map |(let [[x y z] $]
            [x
             (- (* y cos-t) (* x sin-t))
             (+ (* z cos-t) (* y sin-t))])
         nodes)))

(defn rotate-y [nodes theta]
  "rotate nodes about the Y-axis"
  (let [sin-t (math/sin theta)
        cos-t (math/sin theta)]
    (map |(let [[x y z] $]
            [(- (* x cos-t) (* z sin-t))
             y
             (+ (* z cos-t) (* x sin-t))])
         nodes)))

(defn rotate-z [nodes theta]
  "rotate nodes about the Y-axis"
  (let [sin-t (math/sin theta)
        cos-t (math/sin theta)]
    (map |(let [[x y z] $]
            [(- (* x cos-t) (* y sin-t))
             (+ (* y cos-t) (* x sin-t))
             z])
         nodes)))

(defn create-cuboid [x y z w h d]
  "generate nodes and edges of a cuboid"
  (let [w (/ w 2) h (/ h 2) d (/ d 2)]
    @{:nodes [[(- x w) (- y h) (- z d)]
              [(- x w) (- y h) (+ z d)]
              [(- x w) (+ y h) (- z d)]
              [(- x w) (+ y h) (+ z d)]
              [(+ x w) (- y h) (- z d)]
              [(+ x w) (- y h) (+ z d)]
              [(+ x w) (+ y h) (- z d)]
              [(+ x w) (+ y h) (+ z d)]]
      :edges [[1  2] [2  4] [4  3] [3  1]
              [5  6] [6  8] [8  7] [7  5]
              [1  5] [2  6] [3  7] [4  8]]}))

(defn create-cylinder [x y z r h]
  "TODO: generate nodes and edges for a cylinder")

(defn create-grid [x y z c d s]
  "TODO: generate grid"
  @{:nodes []
    :edges []})
