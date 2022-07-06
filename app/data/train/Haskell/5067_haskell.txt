{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Openshift.V1.GitBuildSource where

import GHC.Generics
import Data.Text
import qualified Data.Aeson

-- | 
data GitBuildSource = GitBuildSource
    { uri :: Text -- ^ points to the source that will be built, structure of the source will depend on the type of build to run 
    , ref :: Maybe Text -- ^ identifies the branch/tag/ref to build 
    , httpProxy :: Maybe Text -- ^ specifies a http proxy to be used during git clone operations 
    , httpsProxy :: Maybe Text -- ^ specifies a https proxy to be used during git clone operations 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON GitBuildSource
instance Data.Aeson.ToJSON GitBuildSource
