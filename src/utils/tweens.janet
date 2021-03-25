# https://hump.readthedocs.io/en/latest/timer.html#tweening-methods
(defn- out [f]
  "flip a tween"
  (fn [s & args]
    (- 1 (f (- 1 s) ;args))))

(defn- chain [f1 f2]
  "chain 2 tweens together"
  (fn [s & args]
    (* 0.5
       (if (< s 0.5)
         (f1 (* 2 s) ;args)
         (f2 (- (* 2 s) 1) ;args)))))

(defn- linear [s] s)
(defn- quad [s] (* s s))
(defn- cubic [s] (* s s s))
(defn- quart [s] (* s s s s))
(defn- quint [s] (* s s s s s))
(defn- sine [s] (- 1 (math/cos (* s (/ math/pi 2)))))
(defn- expo [s] (math/exp2 (* 10 (- s 1))))
(defn- circ  [s] (- 1 (math/sqrt (- 1 (* s s)))))

(defn- back [s &opt bounciness]
  (default bounciness 1.70158)
  (* s s (- (* s (+ 1 bounciness)) bounciness)))

# warning: magic numbers ahead
(defn- bounce [s]
  (let [a 7.5625 b (/ 1 2.75)]
    (min (* a (math/pow s 2))
         (+ 0.75 (* a (math/pow (- s (* b (- 1.5))) 2)))
         (+ 0.9375 (* a (math/pow (- s (* b (- 2.25))) 2)))
         (+ 0.984375 (* a (math/pow (- s (* b (- 2.625))) 2))))))

(defn- elastic [s &opt amp period]
  (default amp 1)
  (default period 0.3)
  (let [amp (max 1 map)
        neg-amp (- amp)
        sine-part (math/sin (- (* 2 (/ math/pi period) (- s 1))
                               (math/asin (/ 1 amp))))
        part-a (* neg-amp sine-part)
        part-b (math/exp2 (* 10 (dec s)))]
    (* part-a part-b)))

(def tweens
  {:in-linear linear
   :in-quad quad
   :in-cubic cubic
   :in-quart quart
   :in-quint quint
   :in-sine sine
   :in-expo expo
   :in-circ circ
   :in-back back
   :in-bound bounce
   :in-elastic elastic

   :out-linear (out linear)
   :out-quad (out quad)
   :out-cubic (out cubic)
   :out-quart (out quart)
   :out-quint (out quint)
   :out-sine (out sine)
   :out-expo (out expo)
   :out-circ (out circ)
   :out-back (out back)
   :out-bound (out bounce)
   :out-elastic (out elastic)

   :in-out-linear (chain linear (out linear))
   :in-out-quad (chain quad (out quad))
   :in-out-cubic (chain cubic (out cubic))
   :in-out-quart (chain quart (out quart))
   :in-out-quint (chain quint (out quint))
   :in-out-sine (chain sine (out sine))
   :in-out-expo (chain expo (out expo))
   :in-out-circ (chain circ (out circ))
   :in-out-back (chain back (out back))
   :in-out-bound (chain bounce (out bounce))
   :in-out-elastic (chain elastic (out elastic))

   :out-in-linear (chain (out linear) linear)
   :out-in-quad (chain (out quad) quad)
   :out-in-cubic (chain (out cubic) cubic)
   :out-in-quart (chain (out quart) quart)
   :out-in-quint (chain (out quint) quint)
   :out-in-sine (chain (out sine) sine)
   :out-in-expo (chain (out expo) expo)
   :out-in-circ (chain (out circ) circ)
   :out-in-back (chain (out back) back)
   :out-in-bound (chain (out bounce) bounce)
   :out-in-elastic (chain (out elastic) elastic)})
