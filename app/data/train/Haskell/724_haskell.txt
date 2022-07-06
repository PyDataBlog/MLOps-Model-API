import System.Exit

mul [x, y] = xInt * yInt
	where xInt = read x :: Integer
	      yInt = read y :: Integer

mainLoop 0 = exitWith ExitSuccess
mainLoop x = do
	values <- getLine
	print $ mul (words values)
	mainLoop (x - 1)

main = do
	loops <- getLine
	mainLoop (read loops :: Integer)
