module Barycenter where
    
barTriang :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
barTriang (a, b) (c, d) (e, f) = (x, y)
    where 
        x = (a + c + e) / 3
        y = (b + d + f) / 3