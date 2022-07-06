{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Openshift.V1beta1.Scale where

import GHC.Generics
import Data.Text
import Kubernetes.V1.ObjectMeta
import Openshift.V1beta1.ScaleSpec
import Openshift.V1beta1.ScaleStatus
import qualified Data.Aeson

-- | represents a scaling request for a resource.
data Scale = Scale
    { kind :: Maybe Text -- ^ Kind is a string value representing the REST resource this object represents. Servers may infer this from the endpoint the client submits requests to. Cannot be updated. In CamelCase. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#types-kinds 
    , apiVersion :: Maybe Text -- ^ APIVersion defines the versioned schema of this representation of an object. Servers should convert recognized schemas to the latest internal value, and may reject unrecognized values. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#resources 
    , metadata :: Maybe ObjectMeta -- ^ Standard object metadata; More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#metadata. 
    , spec :: Maybe ScaleSpec -- ^ defines the behavior of the scale. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#spec-and-status. 
    , status :: Maybe ScaleStatus -- ^ current status of the scale. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#spec-and-status. Read-only. 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON Scale
instance Data.Aeson.ToJSON Scale
