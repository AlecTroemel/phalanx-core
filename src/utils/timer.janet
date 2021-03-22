# HEAVLY based on https://github.com/vrld/hump/blob/master/timer.lua
(defn- noop [& _] nil)

(defn- update-timer-handle [handle dt]
  (let [{:time h-time
         :limit h-limit
         :count h-count} handle
        new-time (+ h-time dt)]
    (put handle :time new-time)
    (when (not (nil? (handle :during)))
      (:during handle dt))
    (when (and (>= new-time h-limit) (> h-count 0))
      (when (handle :after)
        (:after handle))
      (put handle :time (- new-time h-limit))
      (put handle :count (- h-count 1)))))


(defn- cancel [self handle]
  (put-in self [:_functions handle] nil))

(defn- update [self dt]
  (eachk handle (self :_functions)
    (update-timer-handle handle dt)
    (when (= (handle :count) 0)
      (:cancel self handle))))

(defn- during [self delay during-fn &opt after-fn]
  (let [handle @{:time 0
                 :limit delay
                 :count 1
                 :during during-fn
                 :after after-fn}]
    (put-in self [:_functions handle] true)
    handle))

(defn- after [self delay after-fn]
  (:during self delay nil after-fn))

(defn- every [self delay after &opt count]
  (default count math/inf)
  (let [handle @{:time 0
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
