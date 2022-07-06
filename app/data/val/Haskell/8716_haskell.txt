{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
import Text.XML.HXT.Core
import Data.List
import Text.HandsomeSoup
import System.Random
import System.Environment
import Network.HTTP.Base
import System.Process
import Network.HTTP.Enumerator
import Network.HTTP.Types (methodPost)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> putStrLn help
            _ -> do gen    <- newStdGen
                    wpList <- wallpapers (args !! 0) (args !! 1)
                    wp     <- wallpaper $ wpList !! head (randomRs (0, length wpList) gen)
                    createProcess $ shell ("feh --bg-scale " ++ head wp)
                    putStrLn $ wp !! 1

help :: String
help = "Usage: randwp tag resolution"

wallpapers :: String -> String -> IO [String]
wallpapers q res = do html <- request q res
                      let doc = parseHtml $ L.unpack html
                      contents <- runX $ doc >>> css "a" ! "href"
                      return $ filter ("wallpaper" `isInfixOf`) contents

wallpaper :: String -> IO [String]
wallpaper url = do doc <- fromUrl url
                   runX $ getWp doc

getWp doc = doc >>> css "div" >>>
            hasAttrValue "id" (== "bigwall") >>>
            css "img" ! "src" <+> css "img" ! "alt"

request q res = do req0 <- parseUrl "http://wallbase.cc/search/"
                   let req = req0 { method         = methodPost
                                  , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                                  , requestBody    = RequestBodyLBS $ mkPost q res
                                  }
                   res <- withManager $ httpLbs req
                   return $ responseBody res

mkPost :: String -> String -> L.ByteString
mkPost q res = L.pack $ "query=" ++ urlEncode q ++ "&res=" ++ res ++ "&res_opt=gteq"