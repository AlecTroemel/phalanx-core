;; a finite state machine
;; NOTE: not working yet
;; adapted from https://github.com/kyleconroy/lua-state-machine

(local machine [])
(tset machine "__index" machine)

(local NONE "none")
(local ASYNC "async")

(fn call-handler [handler params]
    (when handler
      (handler (unpack params))))

(fn create-transition [name]
    (local can nil)
    (local to nil)
    (local from nil)
    (local params nil)

    (fn transition [self ...]
        (match self.async-state
               ; Match non async
               NONE
               (do
                (let [(new-can new-to) (self:can name)]
                  (set can new-can)
                  (set to new-to))
                (set from self.current)
                (set params {: self : name : from : to })
                (if can
                    (do
                     (tset self :current-transitioning-event name)
                     (let [before-return (call-handler (. self (.. "onbefore" name)) params)
                           leave-return (call-handler (. self (.. "onleave" from)) params)]
                       (if (or (= before-return false) (= leave-return false))
                           false
                           (do
                            (tset self :async-state (.. name "WaitingOnLeave"))
                            (when (not (= leave-return ASYNC)) (transition self ...))
                            true))))
                    false))

               ; match waiting on leave async state
               (astate ? (= astate (.. name "WaitingOnLeave")))
               (do
                (tset self :current to)
                (let [enter-return (call-handler (or (. self (.. "onenter" to)) (. self (.. "on" to)))
                                                 params)]
                  (tset self :async-state (.. name "WaitingOnEnter"))
                  (when (not (= enter-return ASYNC)) (transition self ...))
                  true))

               ; match waiting on enter async state
               (astate ? (= astate (.. name "WaitingOnEnter")))
               (do
                (call-handler (or (. self (.. "onafter" name)) (. self (.. "on" name))) params)
                (call-handler self.onstatechange params)
                (tset self :async-state NONE)
                (tset self :current-transitioning-event nil)
                true)

               ; match default
               _
               (if (or (string.find self.async-state "WaitingOnLeave")
                       (string.find self.async-state "WaitingOnEnter"))
                   (do
                    (tset self :async-state NONE)
                    (transition self ...)
                    true)
                   (do
                    (tset self :current-transitioning-event nil)
                    false)))))

(fn add-to-map [map event]
    (if (= (type event.from) "string")
        (tset map event.from event.to)
        (each [_ from (ipairs event.from)] (tset map from event.to))))

(fn machine.create [options]
    (assert options.events)
    (local fsm {: options
                :current (or options.initial NONE)
                :async-state NONE
                :events {} })
    (setmetatable fsm machine)
    (each [_ event  (ipairs (or options.events {}))]
          (let [name events.name]
            (tset fsm name (or fsm.name (create_transition name)))
            (tset (. fsm events) name (or (. fsm.events name) {:map {}}))
            (add-to-map (-?> fsm (. :events) (. name) (. :map)) event)))

    (each [name callback (pairs (or options.callbacks {}))]
          (tset fsm name callback))
    fsm)

(fn machine:is [state] (= self.current state))

(fn machine:can [e]
    (let [event (. self.events e)
          to (and event (or
                         (-?> event (. :map) (. self.current))
                         (. event.map "*")))]
      (values (not (= to nil)) nil)))

(fn machine:cannot [e] (not (self:can e)))

(fn machine:transition [event]
    (when (= self.current-transitioning-event event)
      (: self self.current-transitioning-event)))

(fn machine:cancel-transition [event]
    (when (= self.current-transitioning-event event)
      (tset self :async-state NONE)
      (tset self :current-transitioning-event nil)))

(tset machine :NONE NONE)
(tset machine :ASYNC ASYNC)

machine
