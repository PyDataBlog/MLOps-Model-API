-- Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.
module Main where

sumDigits :: Integer -> Integer
sumDigits n = foldl (+) 0 (toDigits' n)
    where
      toDigits' x
        | x < 10 = [x]
        | otherwise = toDigits' (div x 10) ++ [mod x 10]

main :: IO ()
main = do
     print(sumDigits 22)