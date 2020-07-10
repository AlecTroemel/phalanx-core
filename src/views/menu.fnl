(global gfx (include :lib.gfx))
(global inspect (include :lib.inspect))

(fn init [mgr]
    nil)

(fn update [mgr dt]
    nil)

(fn draw [mgr]
    (gfx.print "Phalanx" 200 120))

(fn keypressed [mgr key]
    (match key
           "x" (mgr:start)))

{: init : update : draw : keypressed}
