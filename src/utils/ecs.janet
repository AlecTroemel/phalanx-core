# https://github.com/bakpakin/tiny-ecs/tree/demo-commandokibbles
# https://github.com/bit-phlippers/cl-ecs

(defmacro def-component [name fields]
  "Define a new component with the specified fields."
  ~(def ,name
     (fn ,name ,fields
       (zipcoll
        ,(map |(keyword $) fields)
        ,fields))))

(defmacro add-entity [world & components]
  "Add a new entity with the given components to the world."
  ~(array/push
    (get world :entities)
    (zipcoll
     ,(map |(keyword (first $)) components)
     ,(map |(eval $) components))))

(defn add-system [world query func]
  "Add a system to the world for the given query."
  (array/push (get world :systems) [query func]))

(defn- update [self dt]
  (each (query func) (get self :systems)
    (each match (filter (fn [e] (all |(get e $) query)) (self :entities))
      (func ;(map |(get match $) query) dt))))

(defn init []
  @{:entities @[]
    :systems @[]
    :update update})
