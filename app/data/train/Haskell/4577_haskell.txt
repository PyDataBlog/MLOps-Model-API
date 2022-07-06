#!/usr/bin/env runhaskell

import Data.List (foldl', group)
import System.Environment (getArgs)

digits :: Integer -> [Int]
digits 0 = [0]
digits n = reverse (extract n)
  where extract 0 = []
        extract x = (fromIntegral x `mod` 10) : extract (fromIntegral x `div` 10)

fromDigits :: [Int] -> Integer
fromDigits d = foldl' (\a b -> a*10 + b) 0 (map toInteger d)

looknsay :: [Int] -> [Int]
looknsay d = concatMap (\x -> [length x, head x]) (group d)

main :: IO ()
main = do
  args <- getArgs
  putStr (
    case args of
      [seed]                 -> unlines (run seed Nothing)
      [seed, "-last"]        -> last (run seed Nothing)
      [seed, steps]          -> unlines (run seed (Just steps))
      [seed, steps, "-last"] -> last (run seed (Just steps))
      _                      -> "usage: seed [steps=20] [-last]"
    )

  where run seedStr Nothing         = run seedStr (Just "20")
        run seedStr (Just stepsStr) = simulate seed steps
          where seed  = read seedStr :: Integer
                steps = read stepsStr :: Int

        simulate seed steps
          | seed > 0 && seed < 10 = map (show . fromDigits) (take steps (iterate looknsay (digits seed)))
          | otherwise             = error "seed should be a non-zero digit (1,2,...,9)"
