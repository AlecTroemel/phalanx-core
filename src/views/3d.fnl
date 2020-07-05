;; https://petercollingridge.appspot.com/3D-tutorial/rotating-objects

(global inspect (require :lib.inspect))
(global lume (require :lib.lume))
(include :lib.gfx)

(global NODE-SIZE 1)
(var objects [])

(fn rotate-x [nodes theta]
    "rotate nodes around the x axis"
    (let [sin-t (math.sin theta)
          cos-t (math.cos theta)]
      (lume.map nodes (lambda [node]
                        (lume.merge node
                                    {:y (- (* node.y cos-t) (* node.z sin-t))
                                     :z (+ (* node.z cos-t) (* node.y sin-t))})))))

(fn rotate-y [nodes theta]
    "rotate nodes around the y axis"
    (let [sin-t (math.sin theta)
          cos-t (math.cos theta)]
      (lume.map nodes (lambda [node]
                        (lume.merge node
                                    {:x (- (* node.x cos-t) (* node.z sin-t))
                                     :z (+ (* node.z cos-t) (* node.x sin-t))})))))

(fn rotate-z [nodes theta]
    "rotate nodes around the z axis"
    (let [sin-t (math.sin theta)
          cos-t (math.cos theta)]
      (lume.map nodes (lambda [node]
                        (lume.merge node
                                    {:x (- (* node.x cos-t) (* node.y sin-t))
                                     :y (+ (* node.y cos-t) (* node.x sin-t))})))))

(fn create-cuboid [x y z w h d]
    (let [w (/ w 2)
          h (/ h 2)
          d (/ d 2)]
      {:nodes [{:x (- x w) :y (- y h) :z (- z d)}
               {:x (- x w) :y (- y h) :z (+ z d)}
               {:x (- x w) :y (+ y h) :z (- z d)}
               {:x (- x w) :y (+ y h) :z (+ z d)}
               {:x (+ x w) :y (- y h) :z (- z d)}
               {:x (+ x w) :y (- y h) :z (+ z d)}
               {:x (+ x w) :y (+ y h) :z (- z d)}
               {:x (+ x w) :y (+ y h) :z (+ z d)}]
       :edges [{:from 1 :to 2} {:from 2 :to 4} {:from 4 :to 3} {:from 3 :to 1}
               {:from 5 :to 6} {:from 6 :to 8} {:from 8 :to 7} {:from 7 :to 5}
               {:from 1 :to 5} {:from 2 :to 6} {:from 3 :to 7} {:from 4 :to 8}]}))

(fn create-grid [x y z count dist sep]
    (let [nodes []
          edges []]
      (for [i 1 (* count 4) 4]
           (let [sep (* (/ (- i 1) 4) sep)]
             (lume.push nodes
                        {:x x : y :z (+ z sep)}
                        {:x (+ x dist) : y  :z (+ z sep)}
                        {:x (+ x sep) : y :z z}
                        {:x (+ x sep) : y :z (+ z dist)}))
           (lume.push edges
                      {:from i :to (+ i 1)}
                      {:from (+ i 2) :to (+ i 3)}))
      {: nodes : edges}))


(fn init []
    (lume.push objects
               (create-cuboid 0 0 0 200 4 200) ;; board
               (create-cuboid -80 20 -80 10 30 10) ;; goal1
               (create-cuboid 80 20 80 10 30 10) ;; goal 2
               (create-grid -80 5 -80 9 160 20)))

(fn update [dt] "hmm"
    (lume.each objects
               (lambda [obj]
                 (tset obj :nodes (rotate-y obj.nodes 0.005)))))

(fn draw []
    (love.graphics.translate 200 120)
    (lume.each objects
               (lambda [obj]
                 (let [nodes (rotate-x obj.nodes 10)
                       edges obj.edges]
                   (lume.each nodes
                              (lambda [node] (gfx.circle "white" node.x node.y NODE-SIZE)))
                   (lume.each edges
                              (lambda [edge]
                                (let [node1 (. nodes edge.from)
                                      node2 (. nodes edge.to)]
                                  (gfx.line node1.x node1.y node2.x node2.y))))))))

(fn keypressed [key] "todo")

{: draw : init : update : keypressed}
