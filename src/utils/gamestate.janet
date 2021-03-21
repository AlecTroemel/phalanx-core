# HEAVLY based on https://github.com/vrld/hump/blob/master/gamestate.lua
(defn- noop [& _] nil)

(defn- change-state [self to & args]
  (let [pre (array/peek (self :_stack))]
    (when (nil? (get-in self [:initialized-states to]))
      ((get to :init noop) to))
    (put-in self [:initialized-states to] true)
    (array/push (self :_stack) to)
    ((get to :enter noop) to pre ;args)))

(defn- switch [self to & args]
  (let [pre (array/peek (self :_stack))]
    ((get pre :leave noop) pre)
    (array/pop (self :_stack))
    (:change-state self to ;args)))

(defn- push [self to & args]
  (:change-state self to args))

(defn- pop [self & args]
  (assert (> (length (self :_stack)) 1) "No more states to pop!")
  (let [pre (array/pop (self :_stack))
        to (array/peek (self :_stack))]
    ((get pre :leave noop) pre)
    ((get to :resume noop) to pre ;args)))

(defn- current [self]
  (array/peek (self :_stack)))

(defn- update [self & args]
  (when (get (:current self) :update)
    (:update (:current self) ;args)))

(defn- draw [self & args]
  (when (get (:current self) :draw)
    (:draw (:current self) ;args)))

(defn init []
  {:_stack @[]
   :change-state change-state
   :switch switch
   :push push
   :pop pop
   :current current
   :update update
   :draw draw})
