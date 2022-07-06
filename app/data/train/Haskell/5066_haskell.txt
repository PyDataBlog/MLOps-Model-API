{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kubernetes.V1.Lifecycle where

import GHC.Generics
import Kubernetes.V1.Handler
import qualified Data.Aeson

-- | Lifecycle describes actions that the management system should take in response to container lifecycle events. For the PostStart and PreStop lifecycle handlers, management of the container blocks until the action is complete, unless the container process fails, in which case the handler is aborted.
data Lifecycle = Lifecycle
    { postStart :: Maybe Handler -- ^ PostStart is called immediately after a container is created. If the handler fails, the container is terminated and restarted according to its restart policy. Other management of the container blocks until the hook completes. More info: http://releases.k8s.io/HEAD/docs/user-guide/container-environment.md#hook-details 
    , preStop :: Maybe Handler -- ^ PreStop is called immediately before a container is terminated. The container is terminated after the handler completes. The reason for termination is passed to the handler. Regardless of the outcome of the handler, the container is eventually terminated. Other management of the container blocks until the hook completes. More info: http://releases.k8s.io/HEAD/docs/user-guide/container-environment.md#hook-details 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON Lifecycle
instance Data.Aeson.ToJSON Lifecycle
