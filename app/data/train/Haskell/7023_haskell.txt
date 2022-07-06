import Control.Monad

filter' _ [] = [-1]
filter' _ (_:xs) = xs

main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    line <- getLine
    arrLine <- getLine
    let k = read (words line !! 1) :: Int
    let arr = map (\x -> read x :: Int) (words arrLine)
    putStrLn $ unwords (map show (filter' k arr))

