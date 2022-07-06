{-# LANGUAGE OverloadedStrings #-}

module Handler.TimesGallery where

import Import
import qualified Data.Map as M
import Andon.Types
import Andon.ClassData
import Andon.Gallery

getTimesGalleryR :: OrdInt -> Handler RepHtml
getTimesGalleryR times = defaultLayout $ do
    setTitle $ toHtml $ "Gallery " ++ show times
    $(widgetFile "times-gallery")
  where
    classes = filter ((== times) . getTimes) classData
