module Lib where

import Data.List as List
import Data.List.Split
import Data.Map.Strict as Map

data BinSize = Small | Large deriving Show
data ShelfDifficulty = Normal | Level3 | Level4 deriving Show

data Bin = Bin { warehouse :: String,
                 room :: String,
                 bay :: String,
                 shelf :: String,
                 number :: String,
                 size :: BinSize,
                 difficulty :: ShelfDifficulty
                } deriving Show

type ProductId = String
type Quantity = Int

parseBinSize :: String -> BinSize
parseBinSize "S" = Small
parseBinSize "L" = Large

parseShelfDifficulty :: String -> ShelfDifficulty
parseShelfDifficulty "S01" = Normal
parseShelfDifficulty "S02" = Normal
parseShelfDifficulty "S03" = Level3
parseShelfDifficulty "S04" = Level4

parseBin :: String -> Bin
parseBin binStr =
    let [w, r, b, s, n, sz] = splitOn "-" binStr in
        Bin w r b s n (parseBinSize sz) (parseShelfDifficulty s)
    
parseLine :: String -> (ProductId, [(Bin, Quantity)])
parseLine line =
    let [b, p, q] = splitOn "\t" line in
        (p, [(parseBin b, read q)])

parseFile :: String -> Map.Map ProductId [(Bin, Quantity)]
parseFile f = 
    Map.fromListWith (++) $ List.map parseLine (lines f)

