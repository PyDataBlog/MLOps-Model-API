add :: Integer -> Integer -> Integer
add x y =  x + y

inc = add 1

sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0