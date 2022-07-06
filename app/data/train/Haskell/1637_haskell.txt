module Problem2 ( secondToLastElem ) where

secondToLastElem :: [a] -> a
secondToLastElem x = last (init x)