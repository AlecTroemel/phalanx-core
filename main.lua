local fennel = require("fennel")
local phalanx = fennel.dofile("phalanx.fnl")

function love.load()
  phalanx.init()
end

function love.update(dt)
  phalanx.update(dt)
end

function love.draw()
  phalanx.draw()
end

function love.keypressed(key)
  if(key == "f5") then -- support reloading
    for k,v in pairs(fennel.dofile("phalanx.fnl")) do
      phalanx[k] = v
      phalanx.init()
    end
  end
  phalanx.keypressed(key)
end
