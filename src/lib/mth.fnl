;; shim layer for api specific graphic math calls
;; this is for Love2d
(global mth {})

(fn mth.random [min max]
    (love.math.random min-speed max-speed))

mth
