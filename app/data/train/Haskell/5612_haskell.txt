{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kubernetes.V1.PersistentVolume where

import GHC.Generics
import Data.Text
import Kubernetes.V1.ObjectMeta
import Kubernetes.V1.PersistentVolumeSpec
import Kubernetes.V1.PersistentVolumeStatus
import qualified Data.Aeson

-- | PersistentVolume (PV) is a storage resource provisioned by an administrator. It is analogous to a node. More info: http://releases.k8s.io/HEAD/docs/user-guide/persistent-volumes.md
data PersistentVolume = PersistentVolume
    { kind :: Maybe Text -- ^ Kind is a string value representing the REST resource this object represents. Servers may infer this from the endpoint the client submits requests to. Cannot be updated. In CamelCase. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#types-kinds 
    , apiVersion :: Maybe Text -- ^ APIVersion defines the versioned schema of this representation of an object. Servers should convert recognized schemas to the latest internal value, and may reject unrecognized values. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#resources 
    , metadata :: Maybe ObjectMeta -- ^ Standard object's metadata. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#metadata 
    , spec :: Maybe PersistentVolumeSpec -- ^ Spec defines a specification of a persistent volume owned by the cluster. Provisioned by an administrator. More info: http://releases.k8s.io/HEAD/docs/user-guide/persistent-volumes.md#persistent-volumes 
    , status :: Maybe PersistentVolumeStatus -- ^ Status represents the current information/status for the persistent volume. Populated by the system. Read-only. More info: http://releases.k8s.io/HEAD/docs/user-guide/persistent-volumes.md#persistent-volumes 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON PersistentVolume
instance Data.Aeson.ToJSON PersistentVolume
