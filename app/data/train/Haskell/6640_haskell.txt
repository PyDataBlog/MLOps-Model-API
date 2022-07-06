{-# LANGUAGE OverloadedStrings #-}

module CoinApi.Types.Period where

import CoinApi.Types.Internal

data Period = Period { period_id      :: !Text
                     , length_seconds :: !Int
                     , length_months  :: !Int
                     , unit_count     :: !Int
                     , unit_name      :: !Text
                     , display_name   :: !Text }
  deriving (Show, Eq)

instance FromJSON Period where
  parseJSON = withObject "Period" $ \o -> Period
                                          <$> o .: "period_id"
                                          <*> o .: "length_seconds"
                                          <*> o .: "length_months"
                                          <*> o .: "unit_count"
                                          <*> o .: "unit_name"
                                          <*> o .: "display_name"
