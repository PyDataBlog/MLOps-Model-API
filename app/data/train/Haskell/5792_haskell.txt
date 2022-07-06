-- Project Euler Problem 15 lattice paths
--
-- Given a 20x20 grid and only movements down and to the right, how many paths are there from the top left to bottom right.
--

factorial = scanl (*) 1 [1..]

main = do
	print (factorial!!40 `div` (factorial!!20)^2)
	
