;; (fn color-up [x y] (color-at x (- y 1)))
;; (fn color-down [x y] (color-at x (+ y 1)))
;; (fn color-right [x y] (color-at (+ x 1) y))
;; (fn color-left [x y] (color-at (- x 1) y))

;; (fn count-line [color x y x-iter y-iter]
;;     (match (color-at x y)
;;            color (+ 1 (count-line color (x-iter x) (y-iter y) x-iter y-iter))
;;            _ 0))

;; (fn count-line-up [color x y] (count-line color x y #($1) #(- $1 1)))
;; (fn count-line-down [color x y] (count-line color x y #($1) #(+ $1 1)))
;; (fn count-line-left [color x y] (count-line color x y #(- $1 1) #($1)))
;; (fn count-line-right [color x y] (count-line color x y #(+ $1 1) #($1)))

;; (fn line-is-open [color x y x-iter y-iter]
;;     (match (color-at x y)
;;            nil true
;;            (other color) false
;;            color (line-is-open color (x-iter x) (y-iter y) x-iter y-iter)))

;; (fn line-up-is-open [color x y] (line-is-open color x y #($1) #(- $1 1)))
;; (fn line-down-is-open [color x y] (line-is-open color x y #($1) #(+ $1 1)))
;; (fn line-left-is-open [color x y] (line-is-open color x y #(- $1 1) #($1)))
;; (fn line-right-is-open [color x y] (line-is-open color x y #(+ $1 1) #($1)))


;; (fn other [color]
;;     (match color
;;            "white" "black"
;;            "black" "white"
;;            _ nil))

;; (fn possible-moves [color stone-map]
;;     "possible locations for the move action for a color on a map"
;;     (possible-adds color stone-map))


;; (fn possible-pushes [color stone-map]
;;     "possible locations for the push action for a color on a map. position is where pushing opponite too"
;;     (var moves [])
;;     (each [x line (pairs (stones-in-map color stone-map))]
;;           (each [y stone (pairs line)]
;;                 ;; opponate up
;;                 (when (= (color-up x y ) (other color))
;;                   (let [op-line-length (count-line-up (other color) x (- y 1))]
;;                     (when (and (line-up-is-open (other color) x (- y 1))
;;                                (>  (count-line-down color x y) op-line-length))
;;                       (table.insert moves x (math.max 0 (- y op-line-length))))))

;;                 ;; opponate down
;;                 (when (= (color-down x y ) (other color))
;;                   (let [op-line-length (count-line-down (other color) x (+ y 1))]
;;                     (when (and (line-down-is-open (other color) x (+ y 1))
;;                                (>  (count-line-up color x y) op-line-length))
;;                       (table.insert moves x (math.min map-size (+ y op-line-length))))))

;;                 ;; opponate left
;;                 (when (= (color-left x y ) (other color))
;;                   (let [op-line-length (count-line-left (other color) (- x 1) y)]
;;                     (when (and (line-left-is-open (other color) (- x 1) y)
;;                                (>  (count-line-right color x y) op-line-length))
;;                       (table.insert moves (math.max 0 (- x op-line-length)) y))))

;;                 ;; opponate right
;;                 (when (= (color-right x y ) (other color))
;;                   (let [op-line-length (count-line-right (other color) (+ x 1) y)]
;;                     (when (and (line-right-is-open (other color) (+ x 1) y)
;;                                (>  (count-line-left color x y) op-line-length))
;;                       (table.insert moves (math.min map-size (+ x op-line-length)) y))))))
;;     (lume.unique moves))


;; {:name "move" :from "selecting-action" :to "picking-stone"}
;; {:name "pick-stone" :from "picking-stone" :to "placing-stone"}
;; {:name "place-stone" :from "placing-stone" :to "picking-stone"} ; second stone move
