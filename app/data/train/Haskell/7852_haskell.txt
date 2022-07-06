import H99.Problem11

main = do
        putStrLn "Enter a list of characters:"
        inputList <- readLn :: IO [Char]
        putStrLn (show (encodeModified inputList))
