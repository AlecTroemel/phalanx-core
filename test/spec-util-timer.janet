(import tester :prefix "" :exit false)
(import ./../src/utils/timer)

(deftest
  (test "after | waits to be called"
        (let [t (timer/init)
              called false]
          (:after t 3 (fn [h] (set called true)))

          (:update t 1)
          (is (= called false))

          (:update t 1)
          (is (= called false))

          (:update t 1)
          (is (= called true))

          (:update t 1)
          (is (= called true))))
  (test "example"
        (do
          (def t (timer/init))


          (:during t 5 (fn [h dt] (print " during 5, dt " dt)))
          (:every t 2 (fn [h] (print "  every 2")))

          # Simulate dt ticking
          (for dt 0 10
            (print "tick: " dt)
            (:update t 1)))))
