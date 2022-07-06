{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kubernetes.V1.EndpointSubset where

import GHC.Generics
import Kubernetes.V1.EndpointAddress
import Kubernetes.V1.EndpointPort
import qualified Data.Aeson

-- | EndpointSubset is a group of addresses with a common set of ports. The expanded set of endpoints is the Cartesian product of Addresses x Ports. For example, given:\n  {\n    Addresses: [{\"ip\": \"10.10.1.1\"}, {\"ip\": \"10.10.2.2\"}],\n    Ports:     [{\"name\": \"a\", \"port\": 8675}, {\"name\": \"b\", \"port\": 309}]\n  }\nThe resulting set of endpoints can be viewed as:\n    a: [ 10.10.1.1:8675, 10.10.2.2:8675 ],\n    b: [ 10.10.1.1:309, 10.10.2.2:309 ]
data EndpointSubset = EndpointSubset
    { addresses :: Maybe [EndpointAddress] -- ^ IP addresses which offer the related ports that are marked as ready. These endpoints should be considered safe for load balancers and clients to utilize. 
    , notReadyAddresses :: Maybe [EndpointAddress] -- ^ IP addresses which offer the related ports but are not currently marked as ready because they have not yet finished starting, have recently failed a readiness check, or have recently failed a liveness check. 
    , ports :: Maybe [EndpointPort] -- ^ Port numbers available on the related IP addresses. 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON EndpointSubset
instance Data.Aeson.ToJSON EndpointSubset
