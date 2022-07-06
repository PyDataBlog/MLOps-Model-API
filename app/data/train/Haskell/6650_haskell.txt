import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram str = size == 26
  where size = length $ foldl (\y x -> if not (x `elem` y) && isAlpha x then [x] ++  y  else y ) [] $ map toUpper str