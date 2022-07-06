{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Openshift.V1.ProjectStatus where

import GHC.Generics
import Data.Text
import qualified Data.Aeson

-- | 
data ProjectStatus = ProjectStatus
    { phase :: Maybe Text -- ^ phase is the current lifecycle phase of the project 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON ProjectStatus
instance Data.Aeson.ToJSON ProjectStatus
