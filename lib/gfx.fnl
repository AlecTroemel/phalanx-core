;; shim layer for api specific graphics calls
;; this is for Love2d
(global gfx {})

(fn gfx.print [txt x y]
    (love.graphics.print txt x y))

(fn gfx.line [x y x2 y2]
    "draw line"
    (love.graphics.line x y x2 y2))

(fn gfx.point [x y]
    "draw a point"
    (love.graphics.line x y x y))

(fn gfx.screen-width []
    "get the current width of the screen"
    (love.graphics.getWidth))

(fn gfx.screen-height []
    "get the current width of the screen"
    (love.graphics.getHeight))

(fn gfx.circle [col x y r]
    (if (= col "white")
        (love.graphics.circle "fill" x y r)
        (love.graphics.circle "line" x y r)))

(fn gfx.rect [col x y w h]
    (if (= col "white")
        (love.graphics.rectangle "fill" x y w h)
        (love.graphics.rectangle "line" x y w h)))

gfx
