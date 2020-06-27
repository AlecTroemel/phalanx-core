local game = fennel.dofile("views/game.fnl")

function love.load()
  game.init()
end

function love.update(dt)
  game.update(dt)
end

function love.draw()
  game.draw()
end

function love.keypressed(key)
  game.keypressed(key)
end
