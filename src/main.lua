local fennel = require("fennel")
local manager = fennel.dofile("manager.fnl")

function love.load()
  manager:init()
end

function love.update(dt)
  manager:update(dt)
end

function love.draw()
  manager:draw()
end

function love.keypressed(key)
  manager:keypressed(key)
end
