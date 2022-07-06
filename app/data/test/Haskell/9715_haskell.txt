{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Openshift.V1.ImageStreamImage where

import GHC.Generics
import Data.Text
import Kubernetes.V1.ObjectMeta
import Openshift.V1.Image
import qualified Data.Aeson

-- | 
data ImageStreamImage = ImageStreamImage
    { kind :: Maybe Text -- ^ Kind is a string value representing the REST resource this object represents. Servers may infer this from the endpoint the client submits requests to. Cannot be updated. In CamelCase. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#types-kinds 
    , apiVersion :: Maybe Text -- ^ APIVersion defines the versioned schema of this representation of an object. Servers should convert recognized schemas to the latest internal value, and may reject unrecognized values. More info: http://releases.k8s.io/HEAD/docs/devel/api-conventions.md#resources 
    , metadata :: Maybe ObjectMeta -- ^  
    , image :: Image -- ^ the image associated with the ImageStream and image name 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON ImageStreamImage
instance Data.Aeson.ToJSON ImageStreamImage
