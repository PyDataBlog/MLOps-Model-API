-- Copyright (c) 2014 Zachary King

import System.Environment
import Data.Time

data Entry = Entry { line :: String
                   , time :: UTCTime
                   } deriving (Show)
type Index = [Entry]

emptyIndex :: Index
emptyIndex = []
 
main :: IO ()
main = loop emptyIndex
            
loop :: Index -> IO ()
loop index = do
  putStrLn "command:"
  line <- getLine
  time <- getCurrentTime
  if line /= "exit"
    then do 
      let index2 = index ++ [Entry line time]
      print index2
      loop index2
    else
    -- quit
      return ()
