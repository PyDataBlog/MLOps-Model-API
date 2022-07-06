module Main () where

import Data.List (elemIndex)

type Header = String
type Row a = [a]

data Table a = Table { headers :: [Header]
                     , rows :: [Row a]
                     }

create :: Table a -> Row a -> Table a
create (Table headers rows) newRow = Table headers $ [newRow] ++ rows

type Predicate a = (Row a -> Bool)

readRow :: Table a -> Predicate a -> [Row a]
readRow (Table _ rows) predicate = filter predicate rows

readColumn :: Header -> Table a -> Predicate a -> [a]
readColumn headerName table@(Table headers rows) predicate = maybeGetColumn $ readRow table predicate

maybeGetColumn headerName headers = fmap (flip (!!)) $ elemIndex headerName headers 
