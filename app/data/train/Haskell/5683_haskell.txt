{-# LANGUAGE DeriveGeneric #-}
module Package (Package(..)) where

import GHC.Generics
import qualified Data.Yaml as Yaml

import Description

data Package = Package
  { version :: String
--   , description :: Description
  }
  deriving (Show, Generic)
instance Yaml.FromJSON Package
