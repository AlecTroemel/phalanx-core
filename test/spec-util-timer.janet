(import tester :prefix "" :exit false)
(import ./../src/utils/timer :prefix "")

(deftest
  (test "example"
        (do
          (def t (timer/init))

          (:after t 3 (fn [handle-self] (print "  After 3")))
          (:during t 5 (fn [handle-self dt] (print " during 5, dt " dt)))
          (:every t 2 (fn [handle-self] (print "  every 2")))

          # Simulate dt ticking
          (for dt 0 10
            (print "tick: " dt)
            (:update t 1)))))
