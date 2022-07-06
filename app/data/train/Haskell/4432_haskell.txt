thousands = [y | 	z <- [1..1000], 
					y <- [[x, yy, z] | yy <- [1..1000], let x = (1000 - yy - z), x > 0, x < yy, yy < z]
					]

isTriplet :: [Int] -> Bool
isTriplet xs = a^2 + b^2 == c^2
	where	
		a = head xs
		b = xs !! 1
		c = xs !! 2

problem9 = product $ head [x | x <- thousands, isTriplet x]



					
					

