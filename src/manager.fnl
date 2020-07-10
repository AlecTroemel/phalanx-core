(global machine (require :lib.fsm))
(global lume (require :lib.lume))

(fn onstatechange [self even from to desired]
    ((. (. self.state.views to) :init) self))

(fn init [self]
    (tset self :fsm
          (machine.create {:state {:views {:menu (include :views.menu)
                                           :game (include :views.game)
                                                 }}
                           :initial "menu"
                           :events [{:name "start" :from "menu" :to "game"}]
                           :callbacks {: onstatechange}})))


(fn update [self dt] ((. (. self.fsm.state.views self.fsm.current) :update) self.fsm dt))
(fn draw [self] ((. (. self.fsm.state.views self.fsm.current) :draw) self.fsm))
(fn keypressed [self key] ((. (. self.fsm.state.views self.fsm.current) :keypressed) self.fsm key))

{: init : update : draw : keypressed}
