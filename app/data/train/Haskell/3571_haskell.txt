{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Openshift.V1.SecurityContext where

import GHC.Generics
import Openshift.V1.Capabilities
import Openshift.V1.SELinuxOptions
import qualified Data.Aeson

-- | SecurityContext holds security configuration that will be applied to a container. Some fields are present in both SecurityContext and PodSecurityContext.  When both are set, the values in SecurityContext take precedence.
data SecurityContext = SecurityContext
    { capabilities :: Maybe Capabilities -- ^ The capabilities to add/drop when running containers. Defaults to the default set of capabilities granted by the container runtime. 
    , privileged :: Maybe Bool -- ^ Run container in privileged mode. Processes in privileged containers are essentially equivalent to root on the host. Defaults to false. 
    , seLinuxOptions :: Maybe SELinuxOptions -- ^ The SELinux context to be applied to the container. If unspecified, the container runtime will allocate a random SELinux context for each container.  May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence. 
    , runAsUser :: Maybe Integer -- ^ The UID to run the entrypoint of the container process. Defaults to user specified in image metadata if unspecified. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence. 
    , runAsNonRoot :: Maybe Bool -- ^ Indicates that the container must run as a non-root user. If true, the Kubelet will validate the image at runtime to ensure that it does not run as UID 0 (root) and fail to start the container if it does. If unset or false, no such validation will be performed. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence. 
    } deriving (Show, Eq, Generic)

instance Data.Aeson.FromJSON SecurityContext
instance Data.Aeson.ToJSON SecurityContext
