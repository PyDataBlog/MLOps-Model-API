{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( getKey
  , startAPI
  , verboseLog
  ) where

import ClassyPrelude
import Types.API.Base
import Types.General
import Types.Options

getKey :: APICaller APIKey
getKey = asks apiKey

isVerbose :: APICaller Bool
isVerbose = do
  verbosity <- asks apiVerbosity
  return $ verbosity == Verbose

verboseLog :: Text -> APICaller ()
verboseLog = whenM isVerbose . putStrLn

startAPI = flip runReaderT