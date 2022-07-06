{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( main ) where

import GHC.Generics ( Generic )

import Criterion.Main
import qualified Data.Binary as B
import qualified Data.Serialize as S

data SerializeMe a =
  SerializeMe [[(a,[a])]] [[(a,[a])]] [[(a,[a])]] [[(a,[a])]] [[(a,[a])]] [[(a,[a])]] [[(a,[a])]]
  deriving Generic
instance S.Serialize a => S.Serialize (SerializeMe a)
instance B.Binary a => B.Binary (SerializeMe a)

exampleData :: SerializeMe Double
exampleData = SerializeMe x x x x x x x
  where
    x :: [[(Double, [Double])]]
    x = replicate 200 (replicate 4 (pi, replicate 40 pi))

main :: IO ()
main =
  defaultMain
  [ bench "cereal-encode-nf"     $ nf S.encode     exampleData
  , bench "cereal-encodeLazy-nf" $ nf S.encodeLazy exampleData
  , bench "binary-encode-nf"     $ nf B.encode     exampleData
  ]
