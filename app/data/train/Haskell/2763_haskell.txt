module Vector2D where
type Vector2D = (Double, Double)

zero :: Vector2D
zero = (0, 0)

unit :: Vector2D
unit = (1, 1)

fromScalar :: Double -> Vector2D
fromScalar scalar = (scalar, scalar)

fromAngle :: Double -> Vector2D
fromAngle angle = (cos angle, sin angle)

magnitude :: Vector2D -> Double
magnitude vector = sqrt(magnitudeSquared vector)

magnitudeSquared :: Vector2D -> Double
magnitudeSquared (x, y) = (x*x + y*y)

addVector :: Vector2D -> Vector2D -> Vector2D
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subtractVector :: Vector2D -> Vector2D -> Vector2D
subtractVector (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

multiplyVector :: Vector2D -> Vector2D -> Vector2D
multiplyVector (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

multiplyScalar :: Vector2D -> Double -> Vector2D
multiplyScalar vector scalar = multiplyVector vector $ fromScalar scalar

divideVector :: Vector2D -> Vector2D -> Vector2D
divideVector (x1, y1) (x2, y2) = (x1 / x2, y1 / y2)

divideScalar :: Vector2D -> Double -> Vector2D
divideScalar vector scalar = divideVector vector $ fromScalar scalar

distance :: Vector2D -> Vector2D -> Double
distance (x1, y1) (x2, y2) = sqrt $ dx * dx + dy * dx
  where dx = x1 - x2
        dy = y1 - y2

dot :: Vector2D -> Vector2D -> Double
dot (x1, y1) (x2, y2) = x1*x2 + y1+y2

reflect :: Vector2D -> Vector2D -> Vector2D
reflect vector1@(vectorX, vectorY) vector2@(normalX, normalY) = (vectorX - (2 * dotProduct * normalX), vectorY - (2 * dotProduct * normalY))
  where dotProduct = dot vector1 vector2

normalize :: Vector2D -> Vector2D
normalize vector
  | vectorMagnitude == 0 || vectorMagnitude == 1 = vector
  | otherwise = vector `divideVector` (fromScalar vectorMagnitude)
    where vectorMagnitude = magnitude vector

limit :: Vector2D -> Double -> Vector2D
limit vector maximum
  | magnitudeSquared vector <= maximum * maximum = vector
  | otherwise = (normalize vector) `multiplyScalar` maximum

getAngle :: Vector2D -> Double
getAngle (x, y) = (-1) * (atan2 (-y) x)

rotate :: Vector2D -> Double -> Vector2D
rotate (x, y) angle = (x * (cos angle) - y * (sin angle),
                       x * (sin angle) - y * (cos angle))

lerp :: Double -> Double -> Double -> Double
lerp start end amount = start + (end - start) * amount

lerpVector :: Vector2D -> Vector2D -> Double -> Vector2D
lerpVector (startX, startY) (endX, endY) amount = (lerp startX endX amount,
                                                   lerp startY endY amount)

                                                  
remap :: Double -> Double -> Double -> Double -> Double -> Double
remap value oldMin oldMax newMin newMax = newMin + (newMax - newMin) * ((value - oldMin) / (oldMax - oldMin))

remapVectorToScalar :: Vector2D -> Double -> Double -> Double -> Double -> Vector2D
remapVectorToScalar (x, y) oldMin oldMax newMin newMax = (remap x oldMin oldMax newMin newMax,
                                                          remap y oldMin oldMax newMin newMax)

                                                         
remapVectorToVectors :: Vector2D -> Vector2D -> Vector2D -> Vector2D -> Vector2D -> Vector2D
remapVectorToVectors (x, y) (oldMinX, oldMinY) (oldMaxX, oldMaxY) (newMinX, newMinY) (newMaxX, newMaxY) = (remap x oldMinX oldMaxX newMinX newMaxX,
                                                                                                  remap y oldMinY oldMaxY newMinY newMaxY)

                                                                                                 
angleBetween :: Vector2D -> Vector2D -> Double
angleBetween vector1 vector2 = acos (enforceAngleConstraints tempValue)
  where tempValue = (vector1 `dot` vector2) / (magnitude vector1 * magnitude vector2)
        enforceAngleConstraints value
          | value <= -1 = pi
          | value >= 1 = 0
          | otherwise = value

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * (pi / 180)

radiansToDegrees :: Double -> Double
radiansToDegrees radians = radians * (180 / pi)

clamp :: Double -> Double -> Double -> Double
clamp input min max
  | input < min = min
  | input > max = max
  | otherwise = input

clampToScalar :: Vector2D -> Double -> Double -> Vector2D
clampToScalar (x, y) min max = (clamp x min max, clamp y min max)

clampToVectors :: Vector2D -> Vector2D -> Vector2D -> Vector2D
clampToVectors (x, y) (minX, minY) (maxX, maxY) = (clamp x minX maxX,
                                                   clamp y minY maxY)

negate :: Vector2D -> Vector2D
negate vector = vector `multiplyScalar` (-1)
