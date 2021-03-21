# HEAVLY based on https://github.com/vrld/hump/blob/master/timer.lua
(defn- noop [& _] nil)

(defn- update-timer-handle [handle dt]
  # handle: !{
  #   time = <number>,
  #   after = <function>,
  #   during = <function>,
  #   limit = <number>,
  #   count = <number>,
  # }
  (put handle :time (+ (handle :time) dt))
  ((handle :during) dt (max (- (handle :limit) (handle :time)) 0))
  (loop [:while (and (>= (handle :time) (handle :limit))
                     (> (handle :time) 0))
         :until (let [after (= ((handle :after)) false)]
                  (when after (put handle :count 0))
                  after)]
    (put handle :time (- (handle :time) (handle :limit)))
    (put hanlde :count (- (handle :count) 1))))


(defn- cancel [self handle]
  (put-in self [:_functions handle] nil))

(defn- update [self dt]
  (each handle (self :_functions)
    (update-timer-handle handle dt)
    (when (= (handle :count) 0)
      (:cancel self handle))))

(defn- during [self delay during &opt after]
  (default after noop)
  (let [handle @{:time 0
                 :during during
                 :after after
                 :limit delay
                 :count 1}]
    (put-in self [:_functions handle] true)
    handle))

(defn- after [self delay func]
  (during self delay noop func))

(defn- every [self delay after &opt count]
  (default count math/inf)
  (let [handle @{:time 0
                 :during noop
                 :after after
                 :limit delay
                 :count count}]
    (put-in self [:_functions handle] true)
    handle))

(defn- clear [self]
  (set (self :_functions) @{}))

(defn init []
  {:_functions @{}
   :during during
   :after after
   :every every
   :cancel cancel
   :clear clear
   :update update})
