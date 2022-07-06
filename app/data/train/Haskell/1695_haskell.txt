module AddStuff where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

addTen = addStuff 5
fifteen = addTen 5

main :: IO ()
main = do
   let x = addStuff 10 15
       y = addTen 100
       z = fifteen
   putStrLn "=== Calculations ==="
   putStr "addStuff 10 15: "
   print x
   putStr "addTen 100    : "
   print y
   putStr "fifteen       : "
   print z
--   print 
--   print fifteen
