{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

-- Bare 
main :: IO ()
main = scotty 3000 $ do
  get "/" $ text "Hello Haskeller on Heroku with Stackage!"
