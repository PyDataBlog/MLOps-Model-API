{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kubernetes.V1.NamespaceList where

import GHC.Generics
import Data.Text
import Kubernetes.Unversioned.ListMeta
import Kubernetes.V1.Namespace
import qualified Data.Aeson

-- | NamespaceList is a list of Namespaces.
data NamespaceList = NamespaceList
    { kind :: Maybe Text -- ^ Kind is a string value representing the REST resource this object represents. Servers may infer this from the endpoint the client submits requests to. Cannot be updated. In CamelCase. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#types-kinds 
    , apiVersion :: Maybe Text -- ^ APIVersion defines the versioned schema of this representation of an object. Servers should convert recognized schemas to the latest internal value, and may reject unrecognized values. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#resources 
    , metadata :: Maybe ListMeta -- ^ Standard list metadata. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#types-kinds 
    , items :: [Namespace] -- ^ Items is the list of Namespace objects in the list. More info: http://releases.k8s.io/HEAD/docs/user-guide/namespaces.md 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON NamespaceList
instance Data.Aeson.ToJSON NamespaceList
