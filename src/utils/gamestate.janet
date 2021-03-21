# HEAVLY based on https://github.com/vrld/hump/blob/master/gamestate.lua
(defn- noop [& _] nil)

(defn- change-state [self to & args]
  (let [pre (array/peek (self :stack))]
    (when (nil? (get-in self [:initialized-states to]))
      ((get to :init noop) to))
    (put-in self [:initialized-states to] true)
    (array/push (self :stack) to)
    ((get to :enter noop) to pre ;args)))

(defn- switch [self to & args]
  (let [pre (array/peek (self :stack))]
    ((get pre :leave noop) pre)
    (array/pop (self :stack))
    (:change-state self to ;args)))

(defn- push [self to & args]
  (:change-state self to args))

(defn- pop [self & args]
  (assert (> (length (self :stack)) 1) "No more states to pop!")
  (let [pre (array/pop (self :stack))
        to (array/peek (self :stack))]
    ((get pre :leave noop) pre)
    ((get to :resume noop) to pre ;args)))

(defn- current [self]
  (array/peek (self :stack)))

(defn- update [self & args]
  (when (get (:current self) :update)
    (:update (:current self) ;args)))

(defn- draw [self & args]
  (when (get (:current self) :draw)
    (:draw (:current self) ;args)))

(defn init []
  @{:stack @[]
    :change-state change-state
    :switch switch
    :push push
    :pop pop
    :current current
    :update update
    :draw draw})

# Example

(def menu
  {:init (fn [self] (print "menu init"))
   :enter (fn [self prev & args] (printf "menu enter %q" args))
   :update (fn [self dt] (print "menu game state dt: " dt))})

(def game
  {:init (fn [self] (print "game init"))
   :update (fn [self dt] (print "game game state dt: " dt))
   :leave (fn [self] (print "game leave"))})

(var dt 0)
(def GS (init))

(:push GS menu)
(:update GS dt)

(print "pushing game")
(set dt (+ dt 1))
(:push GS game)
(:update GS dt)

(print "poppting game")
(set dt (+ dt 1))
(:pop GS)
(:update GS dt)
