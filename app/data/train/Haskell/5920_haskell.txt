reverse' :: String -> String -> String
reverse' start end = case start of "" -> end
                                   start -> reverse' (tail start) ((head start):end)

main :: IO () 
main = do
	line <- getLine
	putStrLn $ reverse' line ""