function love.load()

  body = love.graphics.newImage("assets/body.png")
  face = love.graphics.newImage("assets/face.png")
  foot = love.graphics.newImage("assets/foot.png")
  hand = love.graphics.newImage("assets/hand.png")
  x = 500
  y = 500
  xVol = 0
  yVol = 0

end

function love.draw(dt)

  -- dt is deltaTime (how much time has passed)
  love.graphics.draw(face, x, y - 2)
  love.graphics.draw(body, x, y)
  love.graphics.draw(hand, x + 13, y + 12)
  love.graphics.draw(hand, x - 12, y + 12)
  love.graphics.draw(foot, x + 5, y + 33)
  love.graphics.draw(foot, x - 5, y + 33)
  print("X:" .. x)
  print("Y:" .. y)

end

function love.update(dt)

  if love.keyboard.isDown("a", "left") then
    xVol = xVol - 5 * (dt * 10)
  end
  if love.keyboard.isDown("d", "right") then
    xVol = xVol + 5 * (dt * 10)
  end
  if love.keyboard.isDown("w", "up") then
    yVol = yVol - 5 * (dt * 10)
  end
  if love.keyboard.isDown("s", "down") then
    yVol = yVol + 5 * (dt * 10)
  end

  x = x + xVol
  y = y + yVol
  xVol = xVol * 0.9
  yVol = yVol * 0.9

end