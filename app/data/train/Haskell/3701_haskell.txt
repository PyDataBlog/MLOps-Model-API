module Main where

main :: IO ()
main = mapM_ putStrLn c_yes

c_yes :: [String]
c_yes = repeat "y"

{-
main2 :: IO ()
main2 = do putStrLn "y"
           main2
-}
