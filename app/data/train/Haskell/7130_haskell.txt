{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Openshift.V1.Handler where

import GHC.Generics
import Openshift.V1.ExecAction
import Openshift.V1.HTTPGetAction
import Openshift.V1.TCPSocketAction
import qualified Data.Aeson

-- | Handler defines a specific action that should be taken
data Handler = Handler
    { exec :: Maybe ExecAction -- ^ One and only one of the following should be specified. Exec specifies the action to take. 
    , httpGet :: Maybe HTTPGetAction -- ^ HTTPGet specifies the http request to perform. 
    , tcpSocket :: Maybe TCPSocketAction -- ^ TCPSocket specifies an action involving a TCP port. TCP hooks not yet supported 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON Handler
instance Data.Aeson.ToJSON Handler
